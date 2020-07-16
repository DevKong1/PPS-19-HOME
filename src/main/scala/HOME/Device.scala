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

  private var _on: Boolean = false

  def isOn: Boolean = _on

  def turnOn(): Unit = _on = true
  def turnOff(): Unit = _on = false

  override def equals(o: Any): Boolean = o match{
    case device: Device => device.id == this.id
    case _ => false
  }

  require(Rooms.allRooms contains room, "Incorrect room")
}

object AssociableDevice {
  def apply(name: String, room: String, deviceType: DeviceType, consumption: Int, pubTopic: String): AssociableDevice = deviceType match {
    case LightType => Light(name, room, deviceType, consumption, null)
    case _ => this.errUnexpected(UnexpectedDeviceType, deviceType.getSimpleClassName)
  }
}

sealed trait AssociableDevice extends Device with JSONSender with MQTTUtils {
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
        case _ => deviceSpecificMessage(message)
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

  def deviceSpecificMessage(message: String): Unit
}

sealed trait ChangeableValue extends Device {
  //min, max value for the intensity
  def minValue : Int
  def maxValue : Int
  var value : Int

  private def _mapValue: Int => Int = ValueChecker(minValue,maxValue)(_)
  def setValue(newValue: Int): Unit = value = _mapValue(newValue)

  val valueMsg: Regex = ("("+Regex.quote(deviceType.subTopicMsg)+")(\\d+)").r
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
  override def subTopicMsg: String = "setWashingMachine_"
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

/////////////
/// LIGHT ///
/////////////

object Light {
  def apply(name: String, room: String, deviceType: DeviceType = LightType, consumption: Int = LightType.defaultConsumption,
            pubTopic: String = null): SimulatedLight = SimulatedLight(name, room, deviceType, consumption, pubTopic)
}

case class SimulatedLight(override val id: String, override val room: String, override val deviceType: DeviceType,
                          override val consumption: Int, override val pubTopic: String,
                          override val minValue : Int = 1, override val maxValue: Int = 100, override var value: Int = 50) extends Device with AssociableDevice with ChangeableValue {
  require(deviceType == LightType)

  override def deviceSpecificMessage(message: String): Unit = message match {
      case valueMsg(_, value) => setValue(value.toInt)
      case _ => this.errUnexpected(UnexpectedMessage, message)
  }
}

///////////////////////
/// AIR CONDITIONER ///
///////////////////////

object AirConditioner {
  def apply(name: String, room: String, device_type: DeviceType = AirConditionerType, consumption: Int = AirConditionerType.defaultConsumption,
            pubTopic: String = null): SimulatedAirConditioner = SimulatedAirConditioner(name, room, device_type, consumption, pubTopic)
}

case class SimulatedAirConditioner(override val id: String, override val room: String, override val deviceType: DeviceType,
                          override val consumption: Int, override val pubTopic: String,
                          override val minValue : Int = 10, override val maxValue: Int = 35, override var value: Int = 22) extends Device with AssociableDevice with ChangeableValue {
  require(deviceType == AirConditionerType)

  override def deviceSpecificMessage(message: String): Unit = message match {
    case valueMsg(_, value) => setValue(value.toInt)
    case _ => this.errUnexpected(UnexpectedMessage, message)
  }
}

////////////////////
/// DEHUMIDIFIER ///
////////////////////

object Dehumidifier {
  def apply(name: String, room: String, device_type: DeviceType = DehumidifierType, consumption: Int = DehumidifierType.defaultConsumption,
            pubTopic: String = null): SimulatedDehumidifier = SimulatedDehumidifier(name, room, device_type, consumption, pubTopic)
}

case class SimulatedDehumidifier(override val id: String, override val room: String, override val deviceType: DeviceType,
                                   override val consumption: Int, override val pubTopic: String,
                                   override val minValue : Int = 1, override val maxValue: Int = 100, override var value: Int = 10) extends Device with AssociableDevice with ChangeableValue {
  require(deviceType == DehumidifierType)

  override def deviceSpecificMessage(message: String): Unit = message match {
    case valueMsg(_, value) => setValue(value.toInt)
    case _ => this.errUnexpected(UnexpectedMessage, message)
  }
}

///////////////
/// SHUTTER ///
///////////////

object Shutter {
  def apply(name: String, room: String, device_type: DeviceType = ShutterType, consumption: Int = ShutterType.defaultConsumption,
            pubTopic: String = null): SimulatedShutter = SimulatedShutter(name, room, device_type, consumption, pubTopic)
}

case class SimulatedShutter(override val id: String, override val room: String, override val deviceType: DeviceType,
                                 override val consumption: Int, override val pubTopic: String) extends Device with AssociableDevice {
  require(deviceType == ShutterType)

  var _open = false

  def isOpen: Boolean = _open

  def open(): Unit = _open = true
  def close(): Unit = _open = false

  override def deviceSpecificMessage(message: String): Unit = message match {
   // case deviceType.specificMsg + "Open" => open()
   // case deviceType.specificMsg + "Close" => close()
    case _ => this.errUnexpected(UnexpectedMessage, message)
  }
}

//////////////
/// Boiler ///
//////////////

object Boiler {
  def apply(name: String, room: String, device_type: DeviceType = BoilerType, consumption: Int = BoilerType.defaultConsumption,
            pubTopic: String = null): SimulatedBoiler = SimulatedBoiler(name, room, device_type, consumption, pubTopic)
}

case class SimulatedBoiler(override val id: String, override val room: String, override val deviceType: DeviceType,
                                 override val consumption: Int, override val pubTopic: String,
                                 override val minValue : Int = 10, override val maxValue: Int = 35, override var value: Int = 22) extends Device with AssociableDevice with ChangeableValue {
  require(deviceType == BoilerType)

  override def deviceSpecificMessage(message: String): Unit = message match {
    case valueMsg(_, value) => setValue(value.toInt)
    case _ => this.errUnexpected(UnexpectedMessage, message)
  }
}

///////////
/// TV ///
//////////

object TV {
  def apply(name: String, room: String, device_type: DeviceType = TvType, consumption: Int = TvType.defaultConsumption,
            pubTopic: String = null): SimulatedTV = SimulatedTV(name, room, device_type, consumption, pubTopic)
}

case class SimulatedTV(override val id: String, override val room: String, override val deviceType: DeviceType,
                           override val consumption: Int, override val pubTopic: String,
                           override val minValue : Int = 0, override val maxValue: Int = 100, override var value: Int = 50) extends Device with AssociableDevice with ChangeableValue {
  require(deviceType == TvType)

  override def deviceSpecificMessage(message: String): Unit = message match {
    case valueMsg(_, value) => setValue(value.toInt)
    //case deviceType.specificMsg + "Mute" => setValue(minValue)
    case _ => this.errUnexpected(UnexpectedMessage, message)
  }
}

///////////////////////
/// WASHING MACHINE ///
//////////////////////

object WashingMachine {
  def apply(name: String, room: String, device_type: DeviceType = WashingMachineType, consumption: Int = WashingMachineType.defaultConsumption,
            pubTopic: String = null): SimulatedWashingMachine = SimulatedWashingMachine(name, room, device_type, consumption, pubTopic)
}

case class SimulatedWashingMachine(override val id: String, override val room: String, override val deviceType: DeviceType,
                       override val consumption: Int, override val pubTopic: String) extends Device with AssociableDevice{
  require(deviceType == WashingMachineType)


  var activeWashing: WashingType = MIX
  var activeRPM: RPM = MEDIUM
  var acriveExtras: Set[Extra] = Set()

  def setWashingType(newWashing: WashingType): Unit = activeWashing = newWashing
  def setRPM(newRPM: RPM): Unit = activeRPM = newRPM
  def addExtra(newExtra: Extra): Unit = acriveExtras += newExtra
  def removeExtra(toRemove: Extra): Unit = acriveExtras -= toRemove

  /*
  override def deviceSpecificMessage(message: String): Unit = WashingMachineMsg(message) match {
    case WashingMachineMsgWashingType => setWashingType(value)
    case deviceType.specificMsg + "RPM" => setRPM(value)
    case deviceType.specificMsg + "AddExtra" => addExtra(value)
    case deviceType.specificMsg + "RemoveExtra" => removeExtra(value)
    case _ => this.errUnexpected(UnexpectedMessage, message)
  } */
  override def deviceSpecificMessage(message: String): Unit = ???
}