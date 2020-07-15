package HOME


import java.awt.{Dimension, GraphicsEnvironment}

import scala.collection.mutable.ListBuffer
import scala.swing._
import scala.swing.event.{MouseClicked, SelectionChanged}

//TODO: RECONSIDER HOW TO MODEL GUIROOM
class GUIRoom extends Panel with Room{
  override def devices: ListBuffer[Device] = ???
}

sealed trait Room{
  def devices : ListBuffer[Device]
}

object SimpleGUI extends SimpleSwingApplication {


  def top = new MainFrame {
    title = "Home!"
    val tp = new TabbedPane {
      pages += new TabbedPane.Page("HOME", new GUIRoom())
      pages += new TabbedPane.Page("+", new GUIRoom())
      reactions += {
        ???
      }
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