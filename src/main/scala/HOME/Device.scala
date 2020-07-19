package HOME

import HOME.MyClass._

object Rooms {
  //TODO THIS LIST SHOULD BE MADE IN THE INTERFACE
  private var _allRooms: Set[String] = Set("salotto")

  def addRoom(room: String): Unit = _allRooms += room
  def removeRoom(room: String): Unit = _allRooms -= room  //TODO remove all devices in the room
  def allRooms: Set[String] = _allRooms
}

sealed trait DeviceType {
  def defaultConsumption: Int
}

object DeviceType {
  def listTypes: Set[DeviceType] = Set(LightType, AirConditionerType, DehumidifierType, ShutterType, BoilerType, TvType, WashingMachineType, DishWasherType, OvenType, StereoSystemType)

  def apply(devType: String): DeviceType = listTypes.find(_.getSimpleClassName == devType) match {
    case Some(t) => t
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

  require(Rooms.allRooms contains room, this.errUnexpected(UnexpectedRoom, room))
}

object AssociableDevice {
  //Used during the registration to simulate the subscriber device
  class AssociableDeviceImpl(override val id: String, override val room: String, override val deviceType: DeviceType,
                             override val consumption: Int, override val pubTopic: String) extends AssociableDevice {
    override def handleDeviceSpecificMessage(message: SpecificDeviceMsg): Unit = { /*Do nothing*/}
  }

  def apply(id: String, room: String, deviceType: DeviceType, consumption: Int, pubTopic: String): AssociableDevice = {
    new AssociableDeviceImpl(id, room, deviceType, consumption, pubTopic)
  }
}

sealed trait AssociableDevice extends Device with JSONSender with MQTTUtils {
  override def senderType: SenderType = SenderTypeDevice
  override def name: String = id
  override def lastWillTopic: String = regTopic
  override def lastWillMessage: String = disconnectedMsg

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
        case _ => handleDeviceSpecificMessage(SpecificDeviceMsg(message))
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

  def handleDeviceSpecificMessage(message: SpecificDeviceMsg): Unit
}

sealed trait ChangeableValue extends Device {
  //min, max value for the intensity
  def minValue : Int
  def maxValue : Int
  var value : Int

  private def _mapValue: Int => Int = ValueChecker(minValue,maxValue)(_)
  def setValue(newValue: Int): Unit = value = _mapValue(newValue)
}

sealed trait MutableExtras extends Device {
  var activeExtras: Set[GenericExtra] = Set()

  def addExtra(newExtra: GenericExtra): Unit = activeExtras += newExtra
  def removeExtra(toRemove: GenericExtra): Unit = activeExtras -= toRemove
}

case object LightType extends DeviceType {
  override def defaultConsumption: Int = 5
}

case object AirConditionerType extends DeviceType {
  override def defaultConsumption: Int = 0
}

case object DehumidifierType extends DeviceType {
  override def defaultConsumption: Int = 0
}

case object ShutterType extends DeviceType {
  override def defaultConsumption: Int = 0
}

case object BoilerType extends DeviceType {
  override def defaultConsumption: Int = 10
}

case object TvType extends DeviceType {
  override def defaultConsumption: Int = 0
}

case object WashingMachineType extends DeviceType {
  override def defaultConsumption: Int = 0
}

case object DishWasherType extends DeviceType {
  override def defaultConsumption: Int = 0
}

case object OvenType extends DeviceType {
  override def defaultConsumption: Int = 0
}

case object StereoSystemType extends DeviceType {
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

  override def handleDeviceSpecificMessage(message: SpecificDeviceMsg): Unit = message.command match {
      case "setIntensity" => setValue(message.value.toInt)
      case _ => this.errUnexpected(UnexpectedMessage, message.command)
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

  override def handleDeviceSpecificMessage(message: SpecificDeviceMsg): Unit = message.command match {
    case "setTemperature" => setValue(message.value.toInt)
    case _ => this.errUnexpected(UnexpectedMessage, message.command)
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

  override def handleDeviceSpecificMessage(message: SpecificDeviceMsg): Unit = message.command match {
    case "setHumidity" => setValue(message.value.toInt)
    case _ => this.errUnexpected(UnexpectedMessage, message.command)
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

  override def handleDeviceSpecificMessage(message: SpecificDeviceMsg): Unit = message.command match {
    case "open" => open()
    case "close" => close()
    case _ => this.errUnexpected(UnexpectedMessage, message.command)
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

  override def handleDeviceSpecificMessage(message: SpecificDeviceMsg): Unit = message.command match {
    case "setTemperature" => setValue(message.value.toInt)
    case _ => this.errUnexpected(UnexpectedMessage, message.command)
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

  override def handleDeviceSpecificMessage(message: SpecificDeviceMsg): Unit = message.command match {
    case "setVolume" => setValue(message.value.toInt)
    case "mute" => setValue(minValue)
    case _ => this.errUnexpected(UnexpectedMessage, message.command)
  }
}

///////////////////////
/// WASHING MACHINE ///
///////////////////////

object WashingMachine {
  def apply(name: String, room: String, device_type: DeviceType = WashingMachineType, consumption: Int = WashingMachineType.defaultConsumption,
            pubTopic: String = null): SimulatedWashingMachine = SimulatedWashingMachine(name, room, device_type, consumption, pubTopic)
}

case class SimulatedWashingMachine(override val id: String, override val room: String, override val deviceType: DeviceType,
                       override val consumption: Int, override val pubTopic: String) extends Device with AssociableDevice with MutableExtras{
  require(deviceType == WashingMachineType)


  var activeWashing: WashingType = WashingType.MIX
  var activeRPM: RPM = RPM.MEDIUM

  def setWashingType(newWashing: WashingType): Unit = activeWashing = newWashing
  def setRPM(newRPM: RPM): Unit = activeRPM = newRPM

  override def handleDeviceSpecificMessage(message: SpecificDeviceMsg): Unit = message.command match {
    case "WashingType" => setWashingType(WashingType(message.value))
    case "RPM" => setRPM(RPM(message.value))
    case "addExtra" => addExtra(WashingMachineExtra(message.value))
    case "removeExtra" => removeExtra(WashingMachineExtra(message.value))
    case _ => this.errUnexpected(UnexpectedMessage, message.command)
  }
}

//////////////////
/// DishWasher ///
//////////////////

object DishWasher {
  def apply(name: String, room: String, device_type: DeviceType = DishWasherType, consumption: Int = DishWasherType.defaultConsumption,
            pubTopic: String = null): SimulatedDishWasher = SimulatedDishWasher(name, room, device_type, consumption, pubTopic)
}


case class SimulatedDishWasher(override val id: String, override val room: String, override val deviceType: DeviceType,
                       override val consumption: Int, override val pubTopic: String) extends Device with AssociableDevice with MutableExtras {
  require(deviceType == DishWasherType)

  var activeWashing: DishWasherProgram = DishWasherProgram.FAST

  def setWashingProgram(newWashing: DishWasherProgram): Unit = activeWashing = newWashing

  override def handleDeviceSpecificMessage(message: SpecificDeviceMsg): Unit = message.command match {
    case "setProgram" => setWashingProgram(DishWasherProgram(message.value))
    case "addExtra" => addExtra(DishWasherExtra(message.value))
    case "removeExtra" => removeExtra(DishWasherExtra(message.value))
    case _ => this.errUnexpected(UnexpectedMessage, message.command)
  }
}

////////////
/// Oven ///
////////////

object Oven {
  def apply(name: String, room: String, device_type: DeviceType = OvenType, consumption: Int = OvenType.defaultConsumption,
            pubTopic: String = null): SimulatedOven = SimulatedOven(name, room, device_type, consumption, pubTopic)
}

case class SimulatedOven(override val id: String, override val room: String, override val deviceType: DeviceType,
                               override val consumption: Int, override val pubTopic: String,
                               override val minValue : Int = 0, override val maxValue: Int = 250, override var value: Int = 0) extends Device with AssociableDevice with ChangeableValue {
  require(deviceType == OvenType)

  var activeMode: OvenMode = OvenMode.CONVENTIONAL

  def setOvenMode(newMode: OvenMode): Unit = activeMode = newMode

  override def handleDeviceSpecificMessage(message: SpecificDeviceMsg): Unit = message.command match {
    case "setTemperature" => setValue(message.value.toInt)
    case "setMode" => setOvenMode(OvenMode(message.value))
    case _ => this.errUnexpected(UnexpectedMessage, message.command)
  }
}

/////////////////////
/// Stereo System ///
/////////////////////

object StereoSystem {
  def apply(name: String, room: String, device_type: DeviceType = StereoSystemType, consumption: Int = StereoSystemType.defaultConsumption,
            pubTopic: String = null): SimulatedStereoSystem = SimulatedStereoSystem(name, room, device_type, consumption, pubTopic)
}

case class SimulatedStereoSystem(override val id: String, override val room: String, override val deviceType: DeviceType,
                               override val consumption: Int, override val pubTopic: String,
                               override val minValue : Int = 0, override val maxValue: Int = 100, override var value: Int = 50) extends Device with AssociableDevice with ChangeableValue {
  require(deviceType == StereoSystemType)

  override def handleDeviceSpecificMessage(message: SpecificDeviceMsg): Unit = message.command match {
    case "setVolume" => setValue(message.value.toInt)
    case "mute" => setValue(minValue)
    case _ => this.errUnexpected(UnexpectedMessage, message.command)
  }
}