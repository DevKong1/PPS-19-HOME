package HOME

import java.awt.{Dimension, GraphicsEnvironment}
import scala.language.postfixOps

import HOME.MyClass._

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
    case AddProfile => new Dimension(width*0.4 toInt, height*0.2 toInt)
  }
}
object WindowSizeType extends Enumeration {
  type Type = Value
  val MainW, Dialog, AddProfile = Value
}

object IDGenerator {
  private val _id = 0
  def apply(): Int = {
    _id+1
  }
}

object MapDeviceCommands {
  private var commands: Set[String] = Set.empty

  def apply(dev: Device): Unit = dev.deviceType match {
    case LightType => commands = Set.empty
      commands += "setIntensity"
    case AirConditionerType | BoilerType => commands = Set.empty
      commands += "setTemperature"
    case DehumidifierType => commands = Set.empty
      commands += "setHumidity"
    case ShutterType => commands = Set.empty
      commands += "open"
      commands += "close"
    case TvType | StereoSystemType => commands = Set.empty
      commands += "setVolume"
      commands += "mute"
    case WashingMachineType => commands = Set.empty
      commands += "washingType"
      commands += "RPM"
      commands += "addExtra"
      commands += "removeExtra"
    case DishWasherType => commands = Set.empty
      commands += "setProgram"
      commands += "addExtra"
      commands += "removeExtra"
    case OvenType => commands = Set.empty
      commands += "setTemperature"
      commands += "setMode"
    case _ => this.errUnexpected(UnexpectedDeviceType, dev.deviceType.toString)
  }

  def getCommands : Set[String] = {
    commands ++= Set("on", "off")
    commands
  }
}