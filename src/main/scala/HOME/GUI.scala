package HOME


import java.awt.{Dimension, GraphicsEnvironment}

import scala.collection.mutable.ListBuffer
import scala.swing._
import scala.swing.event.{MouseClicked, SelectionChanged}

//TODO: RECONSIDER HOW TO MODEL GUIROOM
class GUIRoom(Room : String) extends Panel with Room {
  var a : Device = new SimulatedLight("Lamp1", "HOME", LightType, 1)
  var b : Device = new SimulatedLight("Lamp2", "HOME", LightType, 1)
  var devices = new ListBuffer[Device]()
  devices += a
  devices += b

  for( c <- devices) {
    _contents += new Button("" + c.name)
    _contents += Swing.HStrut(20)
  }
}

sealed trait Room {
  def devices : ListBuffer[Device]
}

object SimpleGUI extends SimpleSwingApplication {

  def top = new MainFrame {
    title = "Home!"
    val tp = new TabbedPane {
      pages += new TabbedPane.Page("HOME", new GUIRoom("HOME"))
      pages += new TabbedPane.Page("+", new GUIRoom("+"))
      /*reactions += {
        ???
      }*/
    }
    listenTo(tp.selection)
    //used to set items in the main window inside a vertical BoxPanel
    contents = new BoxPanel(Orientation.Vertical) {
      contents ++= Seq(tp)
    }
    size =  WindowSize()
  }
}

object WindowSize {
  val SCREEN = GraphicsEnvironment.getLocalGraphicsEnvironment.getMaximumWindowBounds
  private val FRAME_PROP = 0.55

  val height = SCREEN.height * FRAME_PROP toInt
  val width = SCREEN.width * FRAME_PROP toInt

  def apply(): Dimension = {
    new Dimension(width ,height)
  }

}