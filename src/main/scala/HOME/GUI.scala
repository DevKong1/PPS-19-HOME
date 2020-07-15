package HOME


import java.awt.{Dimension,GraphicsEnvironment}

import scala.swing._

object SimpleGUI extends SimpleSwingApplication {
  private val FRAMEWIDTH_PROP = 0.55
  private val FRAMEHEIGHT_PROP = 0.55
  private val MAX_HEIGHT = ScreenSize("height") * FRAMEHEIGHT_PROP
  private val MAX_WIDHT = ScreenSize("width") * FRAMEWIDTH_PROP

  def top = new MainFrame {
    title = "Home!"

    val tp = new TabbedPane {
      pages += new TabbedPane.Page("Home", new ScrollPane())
      pages += new TabbedPane.Page("Kitchen", new ScrollPane())
    }

    contents = new BoxPanel(Orientation.Vertical) {
      contents ++= Seq(tp)
    }
    size =  new Dimension(MAX_WIDHT toInt,MAX_HEIGHT toInt)
  }
}

object ScreenSize {
  val SCREEN = GraphicsEnvironment.getLocalGraphicsEnvironment.getMaximumWindowBounds
  def apply(msg : String): Int = msg match{
    case "height" => SCREEN height
    case "width" => SCREEN width
  }

}