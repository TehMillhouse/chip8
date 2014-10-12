package chip8

import java.awt.image.BufferStrategy
import java.awt.{Color, Canvas, Dimension}

import javax.swing.JFrame

class Screen extends Canvas {
  private var window : JFrame = null
  private var buffer : BufferStrategy = null
  private var getPixel : (Int, Int) => Boolean = null
  private var xres = 0
  private var yres = 0

  def init(win : JFrame, xres: Int, yres: Int, getPixel: (Int, Int) => Boolean) {
    this.window = win
    this.xres = xres
    this.yres = yres
    this.getPixel = getPixel

    val width = window.getWidth
    val height = window.getHeight
    setBounds(0,0,width,height)
    createBufferStrategy(2)
    buffer = getBufferStrategy
  }

  def draw(): Unit = {
    val width = window.getWidth
    val height = window.getHeight
    val xPixWidth = width / xres
    val yPixWidth = height / yres
    setBounds(0,0,width,height)

    val g = buffer.getDrawGraphics
    g.setColor(Color.black)
    g.fillRect(0, 0, width, height)

    g.setColor(Color.white)
    for (y <- 0 until yres) {
      for (x <- 0 until xres) {
        if (getPixel(x,y))
          g.fillRect(x * xPixWidth, y * yPixWidth, xPixWidth, yPixWidth)
      }
    }

    g.dispose()
    buffer.show()
  }

}

object Application extends App {

  var emulator = new Emulator()
  var controller = new Controller()
  controller.setEscapeCallback(() => emulator.abort())
  controller.setKeypressCallback(() => emulator.signalKey())
  emulator.setKeyboardCallbacks((x : Byte) => controller.status(x))

  System.setProperty("sun.java2d.opengl","True")
  var window = new JFrame("CHIP8")


  window.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  window.addKeyListener(controller)
  window.setSize(640, 320)
  window.setIgnoreRepaint(true) // we'll draw it ourselves anyways

  window.setVisible(true)
  window.setFocusable(true)
  var screen = new Screen()
  var pane = window.getContentPane
  pane.add(screen)
  screen.init(window, emulator.screen_width, emulator.screen_height, (x : Int, y : Int) => emulator.pixel(x,y))
  window.pack()
  screen.draw()

  var rom_path = ""
  if (args.length > 0) {
    rom_path = args(0)
  }

  println(s"loading rom: $rom_path")
  emulator.loadRom(rom_path)
  println("rom loaded, starting rom...")
  while (true) {
    emulator.run(10)
    Thread.sleep(20)
    screen.draw()
  }
  println("exiting.")

  // terminal drawing (for debugging)
  def printScreen()
  {
    print("╔")
    for (_ <- 0 until emulator.screen_width)
      print("═")
    println("╗")

    for (y <- 0 until emulator.screen_height) {
      print("║")
      for (x <- 0 until emulator.screen_width) {

        if (emulator.pixel(x, y))
          print("█")
        else
          print(" ")
      }
      print("║")
      println()
    }

    print("╚")
    for (_ <- 0 until emulator.screen_width)
      print("═")
    println("╝")
  }

}
