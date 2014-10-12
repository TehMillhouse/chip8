package chip8

import chip8.Instruction.Instruction

import scala.collection.immutable.List
import util.Random
import java.util.Date

object Instruction extends Enumeration {
  type Instruction = Value //((java.lang.Integer, java.lang.Integer) => Unit)
  val //SCDOWN,
  CLS,
  RTS,
  // SCRIGHT,
  // SCLEFT,
  // LOW,
  // HIGH,
  JMP,
  JSR,
  SKEQC,
  SKNEC,
  SKEQR,
  LOAD,
  ADDC,
  MOV,
  OR,
  AND,
  XOR,
  ADD,
  SUB,
  SHR,
  RSB,
  SHL,
  SKNER,
  MVI,
  JMI,
  RAND,
  SPRITE,
  // XSPRITE,
  SKPR,
  SKUP,
  GDELAY,
  KEY,
  SDELAY,
  SSOUND,
  ADI,
  FONT,
  // XFONT,
  BCD,
  STR,
  LDR,
  INVALID = Value

}

class Emulator {
  private val mem = new Array[Byte](0x1000)
  // mem map
  // no-man's-land / fonts: 0x0 - 0x200
  // user mem area:         0x200 - 0xE9F
  // stack:                 0xEA0 - 0xEFF
  // frame buffer:          0xF00 - 0xFFF

  private val rom_base = 0x200
  private val stack_top = 0xEFF
  private val fb_base = 0xF00
  private val font_base = 0x00

  val screen_height = 32
  val screen_width = 64

  private val font = Array(
    0xF0,0x90,0x90,0x90,0xF0, // 0
    0x20,0x60,0x20,0x20,0x70, // 1
    0xF0,0x10,0xF0,0x80,0xF0, // 2
    0xF0,0x10,0xF0,0x10,0xF0, // 3
    0x90,0x90,0xF0,0x10,0x10, // 4
    0xF0,0x80,0xF0,0x10,0xF0, // 5
    0xF0,0x80,0xF0,0x90,0xF0, // 6
    0xF0,0x10,0x20,0x40,0x40, // 7
    0xF0,0x90,0xF0,0x90,0xF0, // 8
    0xF0,0x90,0xF0,0x10,0xF0, // 9
    0xF0,0x90,0xF0,0x90,0x90, // A
    0xE0,0x90,0xE0,0x90,0xE0, // B
    0xF0,0x80,0x80,0x80,0xF0, // C
    0xE0,0x90,0x90,0x90,0xE0, // D
    0xF0,0x80,0xF0,0x80,0xF0, // E
    0xF0,0x80,0xF0,0x80,0x80  // F
  )

  private val r = new Array[Byte](16)
  private var r_i : Short = 0
  private var pc = 0
  private var sp = 0
  private var delay = 0
  private var sound = 0
  private var soundStart : Date = null
  private var timerStart : Date = null
  private var keys : (Byte => Boolean) = null
  private object key_barrier
  private var panic = false
  private object panic_barrier

  def setKeyboardCallbacks(getKey : Byte => Boolean): Unit = {
    keys = getKey
  }

  def signalKey(): Unit = {
    key_barrier.synchronized {
      key_barrier.notify()
    }
  }

  def loadRom(path: String) {
    reset()
    (new java.io.FileInputStream(path)).read(mem, rom_base, 0x1000-rom_base)
  }

  def disassemble() : List[Instruction] = {
    for (i <- List.range(rom_base, (0x1000 - rom_base) / 2))
      yield decode(((mem(i) << 8) | (mem(i + 1).toShort & 0xFF)) & 0xFFFF)
  }

  def reset(): Unit = {
    mem.map(_ => 0)
    r.map(_ => 0)
    r_i = 0
    pc = rom_base
    sp = stack_top - 1 // sp is to point to the freshest free 16-bit stack slot
    delay = 0
    sound = 0
    panic = false
    for (i <- font.indices)
      mem(i) = font(i).toByte
  }

  def pixel(x: Int,y: Int) : Boolean = {
    (mem(fb_base + y * (screen_width/8) + x/8) & (0x80 >> (x % 8))) != 0
  }

  private def nibble(n : Int, instruction : Int) : Byte = { // assumes 16-bit as base size
    ((instruction & (0xF << (n*4))) >> (n*4)).toByte
  }

  private def decode(inst : Int) = {
    /*
    val b = inst.toShort
    val p = pc.toShort
    println(f"op:$b%x@$p%x")
    */
    inst match {
      // case _ if (inst & 0xFFF0) == 0x00C0 => Instruction.SCDOWN
      case _ if (inst & 0xFFFF) == 0x00E0 => Instruction.CLS
      case _ if (inst & 0xFFFF) == 0x00EE => Instruction.RTS
      // case _ if (inst & 0xFFFF) == 0x00FB => Instruction.SCRIGHT
      // case _ if (inst & 0xFFFF) == 0x00FC => Instruction.SCLEFT
      // case _ if (inst & 0xFFFF) == 0x00FE => Instruction.LOW
      // case _ if (inst & 0xFFFF) == 0x00FF => Instruction.HIGH
      case _ if (inst & 0xF000) == 0x1000 => Instruction.JMP
      case _ if (inst & 0xF000) == 0x2000 => Instruction.JSR
      case _ if (inst & 0xF000) == 0x3000 => Instruction.SKEQC
      case _ if (inst & 0xF000) == 0x4000 => Instruction.SKNEC
      case _ if (inst & 0xF00F) == 0x5000 => Instruction.SKEQR
      case _ if (inst & 0xF000) == 0x6000 => Instruction.LOAD
      case _ if (inst & 0xF000) == 0x7000 => Instruction.ADDC
      case _ if (inst & 0xF00F) == 0x8000 => Instruction.MOV
      case _ if (inst & 0xF00F) == 0x8001 => Instruction.OR
      case _ if (inst & 0xF00F) == 0x8002 => Instruction.AND
      case _ if (inst & 0xF00F) == 0x8003 => Instruction.XOR
      case _ if (inst & 0xF00F) == 0x8004 => Instruction.ADD
      case _ if (inst & 0xF00F) == 0x8005 => Instruction.SUB
      case _ if (inst & 0xF0FF) == 0x8006 => Instruction.SHR
      case _ if (inst & 0xF00F) == 0x8007 => Instruction.RSB
      case _ if (inst & 0xF0FF) == 0x800E => Instruction.SHL
      case _ if (inst & 0xF00F) == 0x9000 => Instruction.SKNER
      case _ if (inst & 0xF000) == 0xA000 => Instruction.MVI
      case _ if (inst & 0xF000) == 0xB000 => Instruction.JMI
      case _ if (inst & 0xF000) == 0xC000 => Instruction.RAND
      case _ if (inst & 0xF000) == 0xD000 => Instruction.SPRITE
      // case _ if (inst & 0xF00F) == 0xD000 => Instruction.XSPRITE
      case _ if (inst & 0xF0FF) == 0xE09E => Instruction.SKPR
      case _ if (inst & 0xF0FF) == 0xE0A1 => Instruction.SKUP
      case _ if (inst & 0xF0FF) == 0xF007 => Instruction.GDELAY
      case _ if (inst & 0xF0FF) == 0xF00A => Instruction.KEY
      case _ if (inst & 0xF0FF) == 0xF015 => Instruction.SDELAY
      case _ if (inst & 0xF0FF) == 0xF018 => Instruction.SSOUND
      case _ if (inst & 0xF0FF) == 0xF01E => Instruction.ADI
      case _ if (inst & 0xF0FF) == 0xF029 => Instruction.FONT
      // case _ if (inst & 0xF0FF) == 0xF030 => Instruction.XFONT
      case _ if (inst & 0xF0FF) == 0xF033 => Instruction.BCD
      case _ if (inst & 0xF0FF) == 0xF055 => Instruction.STR
      case _ if (inst & 0xF0FF) == 0xF065 => Instruction.LDR
      case _                              => Instruction.INVALID
    }
  }

  def abort(): Unit = {
    panic_barrier.synchronized {
      panic = true
      panic_barrier.notifyAll()
    }
  }

  def step = () => run(1)

  def trace(instructions : Int) : Array[Instruction] = {
    val list = new Array[Instruction](instructions)
    for (i <- 0 until instructions) {
      val d = decode(((mem(pc) << 8) | (mem(pc + 1).toShort & 0xFF)) & 0xFFFF)
      step()
      list(i) = d
    }

    list
  }

  def run(instructions : Int) : Unit = {
    var n = instructions
    var inst = 0x0000
    while (!panic && (n > 0 || n == -1)) {
      inst = ((mem(pc) << 8) | (mem(pc + 1).toShort & 0xFF)) & 0xFFFF // CHIP-8 is big-endian
      import Instruction._

      decode(inst) match {
        case `CLS`   => for (i <- fb_base to 0xFFF) {
          mem(i) = 0.toByte
        }
        case `RTS`   =>
          pc = ((mem(sp+2) << 8) | (mem(sp + 3).toShort & 0xFF)) & 0xFFF
          sp += 2

        case `JMP`   => pc = (inst & 0xFFF) - 2 // offset common tail
        case `JSR`   =>
          mem(sp+1) = (pc & 0x0FF).toByte
          mem(sp) = (nibble(2, pc) & 0xF).toByte
          sp -= 2
          pc = (inst & 0xFFF) - 2 // offset common tail

        case `SKEQC` => if (r(nibble(2, inst)) == (inst & 0xFF).toByte) pc += 2
        case `SKNEC` => if (r(nibble(2, inst)) != (inst & 0xFF).toByte) pc += 2
        case `SKEQR` => if (r(nibble(2, inst)) == (inst & 0xF0).toByte) pc += 2
        case `LOAD`  => r(nibble(2, inst)) = (inst & 0xFF).toByte
        case `ADDC`  => r(nibble(2, inst)) = (r(nibble(2, inst)) + (inst & 0xFF).toByte).toByte
        case `MOV`   => r(nibble(2, inst)) = r(nibble(1, inst))
        case `OR`    => r(nibble(2, inst)) = (r(nibble(2, inst)) | r(nibble(1, inst))).toByte
        case `AND`   => r(nibble(2, inst)) = (r(nibble(2, inst)) & r(nibble(1, inst))).toByte
        case `XOR`   => r(nibble(2, inst)) = (r(nibble(2, inst)) ^ r(nibble(1, inst))).toByte
        case `ADD`   =>
          // unsigned extend
          val left = r(nibble(2, inst)).toShort & 0xFF
          val right = r(nibble(1, inst)).toShort & 0xFF
          r(0xF) = (nibble(2, left + right) >> 8).toByte // carry
          r(nibble(2, inst)) = (r(nibble(2, inst)) + r(nibble(1, inst))).toByte

        case `SUB`   =>
          // unsigned extend
          val left = r(nibble(2, inst)).toShort & 0xFF
          val right = r(nibble(1, inst)).toShort & 0xFF
          if (left < right)  // borrow
            r(0xF) = 0x1.toByte
          else
            r(0xF) = 0x0.toByte
          r(nibble(2, inst)) = (r(nibble(2, inst)) - r(nibble(1, inst))).toByte

        case `SHR`   =>
          r(0xF) = (r(nibble(2, inst)) & 0x1).toByte
          r(nibble(2, inst)) = ((r(nibble(2, inst)).toShort & 0xFF) >> 1).toByte

        case `RSB`   =>
          // unsigned extend
          val left = r(nibble(2, inst)).toShort & 0xFF
          val right = r(nibble(1, inst)).toShort & 0xFF
          if (left < right)  // borrow
            r(0xF) = 0x1.toByte
          else
            r(0xF) = 0x0.toByte
          r(nibble(2, inst)) = (r(nibble(2, inst)) - r(nibble(1, inst))).toByte

        case `SHL`   =>
          r(0xF) = ((r(nibble(2, inst)) & 0x80) >> 7).toByte
          r(nibble(2, inst)) = ((r(nibble(2, inst)).toShort & 0xFF) << 1).toByte

        case `SKNER` => if (r(nibble(2, inst)) != r(nibble(1,inst))) pc += 2
        case `MVI`   => r_i = (inst & 0xFFF).toShort
        case `JMI`   => pc = ((inst & 0xFFF).toShort + r(0)).toShort - 2
        case `RAND`  => r(nibble(2, inst)) = (Random.nextInt(255) & (inst & 0xFF)).toByte
        case `SPRITE`=> // TODO: fix collisions
          r(0x0F) = 0x00.toByte
          val x = r(nibble(2, inst))
          val y = r(nibble(1, inst))
          for (i <- 0 until (inst & 0xF)) {
            // draw sprite
            val sprite = mem((r_i & 0xFFF) + i).toShort & 0xFF
            val mem_loc = fb_base + (screen_width/8) * ((y + i) % screen_height) + ((x/8) % (screen_width/8))
            if ((mem(mem_loc) & (sprite >> (x % 8)).toByte).toByte != 0) {
              // collision
              r(0x0F) = 0x1.toByte
            }
            mem(mem_loc) = (mem(mem_loc) ^ (sprite >> (x % 8)).toByte).toByte
            if (x % 8 != 0) {
              // we need to find a second byte right of this one
              val mem_loc = fb_base + (screen_width/8) * ((y + i) % screen_height) + (((x+8)/8) % (screen_width/8))
              if ((mem(mem_loc) & (sprite << (8 - (x % 8))).toByte) != 0) {
                // collision
                r(0x0F) = 0x1.toByte
              }
              mem(mem_loc) = (mem(mem_loc) ^ (sprite << (8 - (x % 8))).toByte).toByte
            }
          }
        case `SKPR`  => if (keys(r(nibble(2, inst)))) pc += 2
        case `SKUP`  => if (!keys(r(nibble(2, inst)))) pc += 2
        case `GDELAY`=>
          var ticks : Long = 0xFF
          if (timerStart != null)
            ticks = ((new Date()).getTime() - timerStart.getTime()) / 1000 * 60
          if (delay - ticks < 0)
            r(nibble(2, inst)) = 0.toByte
          else
            r(nibble(2, inst)) = (delay - ticks).toByte

        case `KEY`   =>
          key_barrier.synchronized {
            key_barrier.wait()
          }
          var key = 0
          if (keys == null) {
            println("Keyboard callback not set, program will hang forever")
            panic_barrier.synchronized {
              panic_barrier.wait()
            }
            false
          }
          else {
            for (i <- 0 to 0xF)
              if (keys(i.toByte)) key = i
            r(nibble(2, inst)) = key.toByte
          }
        case `SDELAY`=>
          if (delay != 0) {
            // TODO properly handle resetting the timer mid-countdown
          }
          timerStart = new Date()
          delay = r(nibble(2, inst))

        case `SSOUND`=>
          if (delay != 0) {
            // TODO properly handle resetting the timer mid-countdown
          }
          soundStart = new Date()
          sound = r(nibble(2, inst))

        case `ADI`   => r_i = (r_i + (r(nibble(2, inst)).toShort & 0xFF)).toShort
        case `FONT`  => r_i = (font_base + (5 * r(nibble(2,inst)).toShort & 0xFF)).toShort
        case `BCD`   =>
          mem(r_i) = ((r(nibble(2, inst)).toInt & 0xFF) / 100).toByte
          mem(r_i+1) = (((r(nibble(2, inst)).toInt & 0xFF) % 100) / 10).toByte
          mem(r_i+2) = (((r(nibble(2, inst)).toInt & 0xFF) % 10) / 100).toByte
        case `STR`   =>
          for (i <- 0 to nibble(2,inst)) {
            mem(r_i + i) = r(i)
          }
          r_i = (r_i + (nibble(2,inst).toShort & 0xFF) + 1).toShort
        case `LDR`   =>
          for (i <- 0 to nibble(2,inst)) {
            r(i) = mem(r_i + i)
          }
          r_i = (r_i + (nibble(2,inst).toShort & 0xFF) + 1).toShort
        case i =>
          print("Unimplemented instruction:")
          println(i)
      }
      // TODO disable sound when timer has reached zero
      n -= 1
      pc += 2
    }
  }





}

