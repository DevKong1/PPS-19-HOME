package HOME

import scala.util.matching.Regex

object Rooms {
  //TODO THIS LIST SHOULD BE MADE IN THE INTERFACE
  private val _allRooms = Array("salotto")

  def allRooms: Array[String] = _allRooms
}

trait DeviceType {
  def subTopicMsg: String
  def defaultConsumption: Int
}

object DeviceType {
  def apply(dev_type: String): DeviceType = dev_type match {
    case "LightType" => LightType
    case "AirConditionerType" => AirConditionerType
    case "DehumidifierType" => DehumidifierType
    case "ShutterType" => ShutterType
    case "BoilerType" => BoilerType
    case "TvType" => TvType
    case "WashingMachineType" => WashingMachineType
    case "DishWasherType" => DishWasherType
    case "OvenType" => OvenType
    case "StereoSystemType" => StereoSystemType
    case _ => throw new IllegalArgumentException
  }
}

trait Device {
  def id : String
  def room : String
  def device_type : DeviceType
  def consumption : Int
}

object AssociableDevice {
  def apply(name: String, room: String, device_type: DeviceType, consumption: Int, pubTopic: String): AssociableDevice = device_type match {
    case LightType => Light(name, room, device_type, consumption, null)
    case _ => throw new IllegalArgumentException
  }
}

trait BasicDevice extends Device {
  private var _on = false

  def isOn: Boolean = _on

  def turnOn(): Unit = _on = true
  def turnOff(): Unit = _on = false
}

sealed trait AssociableDevice extends Device with MQTTUtils with JSONUtils {
  def pubTopic: String  //Topic used by sensors to send data
  def subTopic: String = getSubTopic  //Topic used by actuators to receive orders

  def getSubTopic: String = room + "/" + device_type + "/" + id

  def connect: Boolean = connect(id, regTopic, onMessageReceived)

  def subscribe: Boolean = subscribe(subTopic) && subscribe(broadcastTopic)

  def onMessageReceived(topic: String, message: String): Unit

  def publish(message: String): Boolean = publish(pubTopic, message)

  def register: Boolean = publish(regTopic, getRegistrationMsg(this).toString(), !retained)
}

case object LightType extends DeviceType {
  override def subTopicMsg: String = "setIntensity_"
  override def defaultConsumption: Int = 5
}

case object AirConditionerType extends DeviceType {
  override def subTopicMsg: String = "setTemperature_"
  override def defaultConsumption: Int = 0
}

case object DehumidifierType extends DeviceType {
  override def subTopicMsg: String = "setHumidity_"
  override def defaultConsumption: Int = 0
}

case object ShutterType extends DeviceType {
  override def subTopicMsg: String = "setShutterState_"
  override def defaultConsumption: Int = 0
}

case object BoilerType extends DeviceType {
  override def subTopicMsg: String = "setHomeTemperature_"
  override def defaultConsumption: Int = 10
}

case object TvType extends DeviceType {
  override def subTopicMsg: String = "setTvState_"
  override def defaultConsumption: Int = 0
}

case object WashingMachineType extends DeviceType {
  override def subTopicMsg: String = "setWashingPlan_"
  override def defaultConsumption: Int = 0
}

case object DishWasherType extends DeviceType {
  override def subTopicMsg: String = "setDishWasherPlan_"
  override def defaultConsumption: Int = 0
}

case object OvenType extends DeviceType {
  override def subTopicMsg: String = "setOvenTemperature_"
  override def defaultConsumption: Int = 0
}

case object StereoSystemType extends DeviceType {
  override def subTopicMsg: String = "setVolume_"
  override def defaultConsumption: Int = 0
}

object Light {
  def apply(name: String, room: String, device_type: DeviceType = LightType, consumption: Int = LightType.defaultConsumption,
            pubTopic: String = null): SimulatedLight = SimulatedLight(name, room, device_type, consumption, pubTopic)
}

case class SimulatedLight(override val id: String, override val room: String, override val device_type: DeviceType,
                          override val consumption: Int, override val pubTopic: String) extends Device with BasicDevice  with AssociableDevice {

  require(device_type == LightType)
  require(Rooms.allRooms contains room, "Incorrect room")

  //min, max value for the intensity
  val _minIntensity = 1
  val _maxIntensity = 100
  private var _intensity = 50

  def getIntensity: Int = _intensity

  private def _mapIntensity = ValueChecker(_minIntensity,_maxIntensity)(_)
  def setIntensity(value: Int): Unit = _intensity = _mapIntensity(value)

  override def equals(that: Any): Boolean = that match {
    case SimulatedLight(id,_,_,_,_) => this.id == id
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
