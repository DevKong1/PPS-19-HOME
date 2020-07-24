package HOME

import java.awt.{Dimension, GraphicsEnvironment}
import scala.language.postfixOps

  object WindowSize {
    import WindowSizeType._
    private val SCREEN = GraphicsEnvironment.getLocalGraphicsEnvironment.getMaximumWindowBounds
    private val FRAME_PROP = 0.55
    private val WIN_PROP_H = 0.2
    private val WIN_PROP_W = 0.5

    val height: Int = SCREEN.height * FRAME_PROP toInt
    val width: Int = SCREEN.width * FRAME_PROP toInt

    def apply(windType : WindowSizeType.Value): Dimension = windType match{
      case MainW => new Dimension(width, height)
      case Dialog => new Dimension(width*WIN_PROP_W toInt,height*WIN_PROP_H toInt)
    }
  }
  object WindowSizeType extends Enumeration {
    type Type = Value
    val MainW, Dialog = Value
  }

object IDGenerator {
  private var _id = 0
  def apply(): Int = {
    _id+1
  }
}