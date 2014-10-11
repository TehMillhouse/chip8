import util.Random
import java.util.Date
import java.lang.Thread

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

object Emulator {
  var mem = new Array[Byte](0x1000)
  // mem map
  // no-man's-land / fonts: 0x0 - 0x200
  // stack: 0xEA0 - 0xEFF
  // frame buffer: 0xF00 - 0xFFF

  val rom_base = 0x200
  val stack_top = 0xEFF
  val fb_base = 0xF00
  val font_base = 0x00
  val g_height = 32
  val g_width = 64

  val font = Array(
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

  val r = new Array[Byte](16)
  var r_i : Short = 0
  var pc = 0
  var sp = 0
  var delay = 0
  var sound = 0
  var soundStart = new Date()
  var timerStart = new Date()
  var keys = new Array[Boolean](16)
  val default_rom = "/home/max/roms/PONG";

  def loadRom(path: String) {
    reset()
    (new java.io.FileInputStream(path)).read(mem, 0x200, 0x1000-0x200)
  }

  def reset(): Unit = {
    mem.map(_ => 0)
    r.map(_ => 0)
    r_i = 0
    pc = rom_base
    sp = stack_top - 1 // sp is to point to the freshest free 16-bit stack slot
    delay = 0
    sound = 0
    keys.map(_ => 0)
    for (i <- font.indices)
      mem(i) = font(i).toByte
  }

  def main(args : Array[String]) {
    var rom_path = default_rom
    if (args.length > 0) {
      rom_path = args(0)
    }
    println(s"loading rom: $rom_path")
    loadRom(rom_path)
    println("rom loaded, starting rom...")
    while (true) {
      run(20)
      printScreen()
      Thread.sleep(100)
    }
    println("exiting.")
  }

  def nibble(n : Int, instruction : Int) : Byte = { // assumes 16-bit as base size
    ((instruction & (0xF << (n*4))) >> (n*4)).toByte
  }

  def decode(inst : Int) = {
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
      case _                              =>
        println("Invalid instruction")
        Instruction.INVALID
    }
  }

  def step = () => run(1)

  def run(instructions : Int): Unit = {
    var n = instructions
    var panic = false
    var inst = 0x0000
    while (!panic && n > 0 || n == -1 ) {
      inst = ((mem(pc) << 8) | (mem(pc + 1).toShort & 0xFF)) & 0xFFFF // CHIP-8 is big-endian
      import Instruction._

      decode(inst) match {
        case `CLS`   => for (i <- fb_base until 0xFFF) {
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

        case `SKEQC` => if (r(nibble(2, inst)) == (inst & 0xFF)) pc += 2
        case `SKNEC` => if (r(nibble(2, inst)) != (inst & 0xFF)) pc += 2
        case `SKEQR` => if (r(nibble(2, inst)) == (inst & 0xF0)) pc += 2
        case `LOAD`  => r(nibble(2, inst)) = (inst & 0xFF).toByte
        case `ADDC`  => r(nibble(2, inst)) = (r(nibble(2, inst)) + (inst & 0xFF)).toByte
        case `MOV`   => r(nibble(2, inst)) = r(nibble(1, inst))
        case `OR`    => r(nibble(2, inst)) = (r(nibble(2, inst)) | r(nibble(1, inst))).toByte
        case `AND`   => r(nibble(2, inst)) = (r(nibble(2, inst)) & r(nibble(1, inst))).toByte
        case `XOR`   => r(nibble(2, inst)) = (r(nibble(2, inst)) ^ r(nibble(1, inst))).toByte
        case `ADD`   =>
          val left = r(nibble(2, inst)).toShort & 0xFF // unsigned extend
          val right = r(nibble(1, inst)).toShort & 0xFF // unsigned extend
          r(0xF) = nibble(1, left + right) // carry
          r(nibble(2, inst)) = (r(nibble(2, inst)) + r(nibble(1, inst))).toByte

        case `SUB`   =>
          val left = r(nibble(2, inst)).toShort & 0xFF // unsigned extend
          val right = r(nibble(1, inst)).toShort & 0xFF // unsigned extend
          if (left < right)  // borrow
            r(0xF) = 0x1.toByte
          else
            r(0xF) = 0x0.toByte
          r(nibble(2, inst)) = (r(nibble(2, inst)) - r(nibble(1, inst))).toByte

        case `SHR`   =>
          r(0xF) = (r(nibble(2, inst)) & 0x1).toByte
          r(nibble(2, inst)) = (r(nibble(2, inst)).toShort & 0xFF >> 1).toByte

        case `RSB`   =>
          val left = r(nibble(2, inst)).toShort & 0xFF // unsigned extend
        val right = r(nibble(1, inst)).toShort & 0xFF // unsigned extend
          if (left < right)  // borrow
            r(0xF) = 0x1.toByte
          else
            r(0xF) = 0x0.toByte
          r(nibble(2, inst)) = (r(nibble(2, inst)) - r(nibble(1, inst))).toByte

        case `SHL`   =>
          r(0xF) = ((r(nibble(2, inst)) & 0x80) >> 7).toByte
          r(nibble(2, inst)) = (r(nibble(2, inst)).toShort & 0xFF << 1).toByte

        case `SKNER` => if (r(nibble(2, inst)) != r(nibble(1,inst))) pc += 2
        case `MVI`   => r_i = (inst & 0xFFF).toShort
        case `JMI`   => pc = ((inst & 0xFFF) + r(0)).toShort - 2
        case `RAND`  => r(nibble(2, inst)) = (Random.nextInt(255) & (inst & 0xFF)).toByte
        case `SPRITE`=> // TODO: fix collisions
          r(0xF) = 0x00.toByte
          val x = r(nibble(2, inst))
          val y = r(nibble(1, inst))
          //println(x, y)
          for (i <- 0 until (inst & 0xF)) {
            // draw sprite
            val sprite = mem(r_i + i)
            val mem_loc = fb_base + (g_width/8) * ((y + i) % g_height) + ((x/8) % (g_width/8))
            if ((mem(mem_loc) & ((sprite.toShort & 0xFF) >> (x % 8)).toByte).toByte != 0) {
              // collision
              r(0x00) = 0x1.toByte
            }
            mem(mem_loc) = (mem(mem_loc) ^ ((sprite.toShort & 0xFF) >> (x % 8)).toByte).toByte
            if (x % 8 != 0) {
              // we need to find a second byte right of this one
              val mem_loc = fb_base + (g_width/8) * ((y + i) % g_height) + (((x+8)/8) % (g_width/8))
              if ((mem(mem_loc) & ((sprite.toShort & 0xFF) << (8 - (x % 8))).toByte) != 0) {
                // collision
                r(0x00) = 0x1.toByte
              }
              mem(mem_loc) = (mem(mem_loc) ^ ((sprite.toShort & 0xFF) << (8 - (x % 8))).toByte).toByte
            }
          }
        case `SKPR`  => if (keys(nibble(2, inst))) pc += 2
        case `SKUP`  => if (!keys(nibble(2, inst))) pc += 2
        case `GDELAY`=>
          var ticks = ((new Date()).getTime() - timerStart.getTime()) / 1000 * 60
          if (delay - ticks < 0)
            r(nibble(2, inst)) = 0.toByte
          else
            r(nibble(2, inst)) = (delay - ticks).toByte

        case `KEY`   =>
          // FIXME
          r(nibble(2, inst)) = 0x5.toByte

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

        case `ADI`   => r_i = (r_i + r(nibble(2, inst))).toShort
        case `FONT`  => r_i = (font_base + (5 * r(nibble(2,inst)))).toShort
        case `BCD`   =>
          mem(r_i) = ((r(nibble(2, inst)).toInt & 0xFF) / 100).toByte
          mem(r_i+1) = (((r(nibble(2, inst)).toInt & 0xFF) % 100) / 10).toByte
          mem(r_i+2) = (((r(nibble(2, inst)).toInt & 0xFF) % 10) / 100).toByte
        case `STR`   =>
          for (i <- 0 until nibble(2,inst)) {
            mem(r_i + i) = r(i)
          }
          r_i = (r_i + (nibble(2,inst).toShort & 0xFF) + 1).toShort
        case `LDR`   =>
          for (i <- 0 until nibble(2,inst)) {
            r(i) = mem(r_i + i)
          }
          r_i = (r_i + (nibble(2,inst).toShort & 0xFF) + 1).toShort
        case i =>
          print("Unimplemented instruction:")
          println(i)
          panic = false

      }
      // TODO disable sound when timer has reached zero
      n -= 1
      pc += 2
    }
  }

  def printScreen(): Unit =
  {
    print("╔")
    for (_ <- 0 until g_width)
      print("═")
    println("╗")

    for (y <- 0 until g_height) {
      print("║")
      for (x <- 0 until (g_width/8)) {
        for (i <- 0 until 8) {
          if ((mem(fb_base + y * (g_width/8) + x) & (0x80 >> i)) != 0)
            print("█")
          else
            print(" ")
        }
      }
      print("║")
      println()
    }

    print("╚")
    for (_ <- 0 until g_width)
      print("═")
    println("╝")
  }



}

