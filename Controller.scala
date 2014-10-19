package chip8

import java.awt.event.{KeyEvent, KeyListener}

class Controller extends KeyListener {
  var status = new Array[Boolean](16)
  var escapePressed : () => _ = null
  var upPressed : () => _ = null
  var downPressed : () => _ = null
  var jPressed : () => _ = null
  var kPressed : () => _ = null
  private var signalKeypress : () => _ = null

  def setKeypressCallback(func: () => _) {
    signalKeypress = func
  }

  override def keyTyped(e : KeyEvent) {}

  override def keyPressed(e : KeyEvent) {
    if (e.getKeyChar == 27 && escapePressed != null) escapePressed()

    var nop = false
    e.getKeyCode match {
      case KeyEvent.VK_0 =>
        status(0x1) = true
      case KeyEvent.VK_1 =>
        status(0x2) = true
      case KeyEvent.VK_2 =>
        status(0x3) = true
      case KeyEvent.VK_3 =>
        status(0xC) = true
      case KeyEvent.VK_Q =>
        status(0x4) = true
      case KeyEvent.VK_W =>
        status(0x5) = true
      case KeyEvent.VK_E =>
        status(0x6) = true
      case KeyEvent.VK_R =>
        status(0xD) = true
      case KeyEvent.VK_A =>
        status(0x7) = true
      case KeyEvent.VK_S =>
        status(0x8) = true
      case KeyEvent.VK_D =>
        status(0x9) = true
      case KeyEvent.VK_F =>
        status(0xE) = true
      case KeyEvent.VK_Z =>
        status(0xA) = true
      case KeyEvent.VK_X =>
        status(0x0) = true
      case KeyEvent.VK_C =>
        status(0xB) = true
      case KeyEvent.VK_V =>
        status(0xF) = true

      case KeyEvent.VK_J =>
        if (jPressed != null) jPressed()
      case KeyEvent.VK_K =>
        if (kPressed != null) kPressed()
      case KeyEvent.VK_UP =>
        if (upPressed != null) upPressed()
      case KeyEvent.VK_DOWN =>
        if (downPressed != null) downPressed()

      case _ => nop = true
    }
    if (!nop) signalKeypress()
  }

  override def keyReleased(e : KeyEvent): Unit = {
    e.getKeyCode match {
      case KeyEvent.VK_0 =>
        status(0x1) = false
      case KeyEvent.VK_1 =>
        status(0x2) = false
      case KeyEvent.VK_2 =>
        status(0x3) = false
      case KeyEvent.VK_3 =>
        status(0xC) = false
      case KeyEvent.VK_Q =>
        status(0x4) = false
      case KeyEvent.VK_W =>
        status(0x5) = false
      case KeyEvent.VK_E =>
        status(0x6) = false
      case KeyEvent.VK_R =>
        status(0xD) = false
      case KeyEvent.VK_A =>
        status(0x7) = false
      case KeyEvent.VK_S =>
        status(0x8) = false
      case KeyEvent.VK_D =>
        status(0x9) = false
      case KeyEvent.VK_F =>
        status(0xE) = false
      case KeyEvent.VK_Z =>
        status(0xA) = false
      case KeyEvent.VK_X =>
        status(0x0) = false
      case KeyEvent.VK_C =>
        status(0xB) = false
      case KeyEvent.VK_V =>
        status(0xF) = false
      case _ =>
    }
  }
}

