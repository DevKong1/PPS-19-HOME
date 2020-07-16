package HOME

import HOME.MyClass._
import scala.util.matching.Regex

object Rooms {
  //TODO THIS LIST SHOULD BE MADE IN THE INTERFACE
  private var _allRooms: Set[String] = Set("salotto")

  def addRoom(room: String): Unit = _allRooms += room
  def removeRoom(room: String): Unit = _allRooms -= room  //TODO remove all devices in the room
  def allRooms: Set[String] = _allRooms
}

sealed trait DeviceType {
  def subTopicMsg: String
  def defaultConsumption: Int

  val specificMsg: Regex = ("("+Regex.quote(subTopicMsg)+")(\\d+)").r
}

object DeviceType {
  def apply(devType: String): DeviceType = devType match {
    case d if d == LightType.getSimpleClassName => LightType
    case d if d == AirConditionerType.getSimpleClassName => AirConditionerType
    case d if d == DehumidifierType.getSimpleClassName => DehumidifierType
    case d if d == ShutterType.getSimpleClassName => ShutterType
    case d if d == BoilerType.getSimpleClassName => BoilerType
    case d if d == TvType.getSimpleClassName => TvType
    case d if d == WashingMachineType.getSimpleClassName => WashingMachineType
    case d if d == DishWasherType.getSimpleClassName => DishWasherType
    case d if d == OvenType.getSimpleClassName => OvenType
    case d if d == StereoSystemType.getSimpleClassName => StereoSystemType
    case _ => this.errUnexpected(UnexpectedDeviceType, devType)
  }
}

sealed trait Device extends JSONSender {
  def id : String
  def room : String
  def deviceType : DeviceType
  def consumption : Int
}

object AssociableDevice {
  def apply(name: String, room: String, deviceType: DeviceType, consumption: Int, pubTopic: String): AssociableDevice = deviceType match {
    case LightType => Light(name, room, deviceType, consumption, null)
    case _ => this.errUnexpected(UnexpectedDeviceType, deviceType.getSimpleClassName)
  }
}

sealed trait BasicDevice extends Device {
  private var _on = false

  def isOn: Boolean = _on

  def turnOn(): Unit = _on = true
  def turnOff(): Unit = _on = false
}

sealed trait AssociableDevice extends Device with BasicDevice with JSONSender with MQTTUtils {
  override var senderType: SenderType = SenderTypeDevice
  override var name: String = id
  override var lastWillTopic: String = regTopic
  override var lastWillMessage: String = disconnectedMsg

  var connected: Boolean = false
  var registered: Boolean = false

  def pubTopic: String  //Topic used by sensors to send data
  def subTopic: String = getSubTopic  //Topic used by actuators to receive orders

  def getSubTopic: String = room + topicSeparator + deviceType + topicSeparator + id

  def connect: Boolean = {
    connected = connect(this, onMessageReceived)
    connected
  }

  def subscribe: Boolean = subscribe(subTopic) && subscribe(broadcastTopic)

  def publish(message: String): Boolean = publish(pubTopic, message, this, retained)

  def register: Boolean = publish(regTopic, regMsg, this)

  def onMessageReceived(topic: String, msg: String): Unit = {
    def message: String = getMessageFromMsg(msg)
    topic match {
      case t if t == subTopic => message match {
        case m if m == regSuccessMsg => registered = true
        case m if m == onMsg => turnOn()
        case m if m == offMsg => turnOff()
        case _ => if (!subTopicOnMessageReceived(message)) this.errUnexpected(UnexpectedMessage, message)
      }
      case t if t == broadcastTopic => message match {
        case m if m == disconnectedMsg =>
          turnOff()
          registered = false
        case _ => this.errUnexpected(UnexpectedMessage, message)
      }
      case _ => this.errUnexpected(UnexpectedTopic, topic)
    }
  }

  def subTopicOnMessageReceived(msg: String): Boolean
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
  def apply(name: String, room: String, deviceType: DeviceType = LightType, consumption: Int = LightType.defaultConsumption,
            pubTopic: String = null): SimulatedLight = SimulatedLight(name, room, deviceType, consumption, pubTopic)
}

case class SimulatedLight(override val id: String, override val room: String, override val deviceType: DeviceType,
                          override val consumption: Int, override val pubTopic: String) extends Device with BasicDevice with AssociableDevice {

  require(deviceType == LightType)
  require(Rooms.allRooms contains room, this.errUnexpected(UnexpectedRoom, room))

  //min, max value for the intensity
  private val _minIntensity = 1
  private val _maxIntensity = 100
  private var _intensity = 50

  def getIntensity: Int = _intensity

  private def _mapIntensity: Int => Int = ValueChecker(_minIntensity,_maxIntensity)(_)
  def setIntensity(value: Int): Unit = _intensity = _mapIntensity(value)

  override def equals(that: Any): Boolean = that match {
    case SimulatedLight(id,_,_,_,_) => this.id == id
    case _ => false
  }

  override def subTopicOnMessageReceived(message: String): Boolean = message match {
    case deviceType.specificMsg(_, value) =>
      setIntensity(value.toInt)
      true
    case _ => false
  }
}
