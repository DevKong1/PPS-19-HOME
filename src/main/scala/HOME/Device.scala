package HOME

import scala.util.matching.Regex

object Rooms {
  //TODO THIS LIST SHOULD BE MADE IN THE INTERFACE
  private val _allRooms = Array("salotto")

  def allRooms: Array[String] = _allRooms
}

sealed trait DeviceType {
  def dev_type: String
  def subTopicMsg: String
  def defaultConsumption: Int
}

object DeviceType {
  def apply(dev_type: String): DeviceType = dev_type match {
    case "Light" => LightType
    case _ => throw new IllegalArgumentException
  }
}

case object LightType extends DeviceType {
  override def dev_type: String = "Light"
  override def subTopicMsg: String = "setIntensity_"
  override def defaultConsumption: Int = 5
}

sealed trait Device {
  def name : String
  def room : String
  def device_type : DeviceType
  def consumption : Int
}

object Device {
  def apply(name: String, room: String, device_type: DeviceType, consumption: Int): Device = device_type match {
    case LightType => Light(name, room, device_type, consumption)
    case _ => throw new IllegalArgumentException
  }
}

sealed trait AssociableDevice extends Device with MQTTUtils {
  def pubTopic: String  //Topic used by sensors to send data
  def subTopic: String = getSubTopic  //Topic used by actuators to receive orders

  def getSubTopic: String = room + "/" + device_type + "/" + name

  def connect: Boolean = connect(name, regTopic, onMessageReceived)

  def subscribe: Boolean = subscribe(subTopic); subscribe(broadcastTopic)

  def onMessageReceived(topic: String, message: String): Unit

  def publish(message: String): Boolean = publish(pubTopic, message)

  def register: Boolean = publish(regTopic, regMsg + name)  //TODO define the format
}

sealed trait DeviceFactory {

  def Light(): Device

}

//helper object used by various devices to set the output strength
object IntensityChecker {
  def apply(min: Int, max: Int)(value: Int): Int = value match {
    case x if x > max => max
    case x if x < min => min
    case _ => value
  }
}

object Light {

  def apply(name: String, room: String, device_type: DeviceType = LightType, consumption: Int = LightType.defaultConsumption): SimulatedLight = SimulatedLight(name, room, device_type, consumption)
}

case class SimulatedLight(override val name: String, override val room: String, override val device_type: DeviceType, override val consumption: Int) extends Device with AssociableDevice {

  require(device_type == LightType)
  require(Rooms.allRooms contains room, "Incorrect room")

  override val pubTopic: String = null  //the light device has no sensor

  private var _on = false

  //min, max value for the intensity
  val minIntensity = 1
  val maxIntensity = 100
  private var intensity = 50

  def getIntensity: Int = intensity
  def isOn: Boolean = _on

  def turnOn(): Unit = _on = true
  def turnOff(): Unit = _on = false

  private def _mapIntensity = IntensityChecker(minIntensity,maxIntensity)(_)
  def setIntensity(value: Int): Unit = intensity = _mapIntensity(value)

  override def equals(that: Any): Boolean = that match {
    case SimulatedLight(name,_,_,_) => this.name == name
    case _ => false
  }

  val intensityMsg: Regex = ("("+Regex.quote(device_type.subTopicMsg)+")(\\d+)").r
  override def onMessageReceived(topic:String, message: String): Unit = topic match {
    case t if t == subTopic => message match {
      case "on" => turnOn()
      case "off" => turnOff()
      case intensityMsg(_, value) => setIntensity(value.toInt)
      case _ => throw new IllegalArgumentException("Unexpected message: " + message)
    }
    case t if t == broadcastTopic => message match {
      case m if m == disconnectedMsg => turnOff()
      case _ => throw new IllegalArgumentException("Unexpected message: " + message)
    }
    case _ => throw new IllegalArgumentException("Unexpected topic: " + topic)
  }
}