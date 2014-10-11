import chip8.Emulator
import chip8.Controller
import java.lang.Thread
import java.awt.event.KeyEvent
import java.awt.event.KeyListener
import javax.swing.JFrame

object Application extends App {

  val default_rom = "/home/max/roms/PONG"
  var emulator = new Emulator()
  var controller = new Controller()
  controller.setEscapeCallback(() => emulator.abort())
  controller.setKeypressCallback(() => emulator.signalKey())
  emulator.setKeyboardCallbacks(x => controller.status(x))

  var window = new JFrame("CHIP8")
  window.addKeyListener(controller)
  window.setSize(200, 200)
  window.setVisible(true)
  window.setFocusable(true)


  var rom_path = default_rom
  if (args.length > 0) {
    rom_path = args(0)
  }

  println(s"loading rom: $rom_path")
  emulator.loadRom(rom_path)
  println("rom loaded, starting rom...")
  while (true) {
    emulator.run(5)
    printScreen()
    Thread.sleep(50)
  }
  println("exiting.")


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
