package HOME

import HOME.MyClass._


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

  def turnOn(): Boolean = {_on = true; _on}
  def turnOff(): Boolean = {_on = false; !_on}

  override def equals(o: Any): Boolean = o match {
    case device: Device => device.id == this.id
    case _ => false
  }

  require(Rooms.allRooms contains room, this.errUnexpected(UnexpectedRoom, room))
}

object AssociableDevice {
  //Used during the registration to simulate the subscriber device
  class AssociableDeviceImpl(override val id: String, override val room: String, override val deviceType: DeviceType,
                             override val consumption: Int) extends AssociableDevice {
    override def handleDeviceSpecificMessage(message: CommandMsg): Boolean = true
  }

  def apply(id: String, room: String, deviceType: DeviceType, consumption: Int): AssociableDevice = {
    new AssociableDeviceImpl(id, room, deviceType, consumption)
  }
}

sealed trait AssociableDevice extends Device with JSONSender with MQTTUtils {
  override def senderType: SenderType = SenderTypeDevice
  override def name: String = id
  override def lastWillTopic: String = regTopic
  override def lastWillMessage: String = Msg.disconnected

  private var _connected: Boolean = false
  private var _registered: Boolean = false

  def isConnected: Boolean = _connected
  def isRegistered: Boolean = _registered

  private val pubTopic: String = getPubTopic //Topic used by sensors to send data
  private val subTopic: String = getSubTopic  //Topic used by actuators to receive orders

  private def getBaseTopic: String = room + topicSeparator + deviceType + topicSeparator + id
  def getPubTopic: String = getBaseTopic + topicSeparator + pubTopicPostFix
  def getSubTopic: String = getBaseTopic + topicSeparator + subTopicPostFix

  def connect: Boolean = {
    _connected = connect(this, onMessageReceived)
    _connected
  }

  override def disconnect: Boolean = {
    _connected = !super.disconnect
    if (_registered && !_connected) _registered = false
    !_connected
  }

  def subscribe: Boolean = subscribe(subTopic) && subscribe(broadcastTopic)

  def publish(message: CommandMsg): Boolean = publish(pubTopic, message, this, retained)

  def register: Boolean = publish(regTopic, Msg.register, this)

  def onMessageReceived(topic: String, msg: String): Unit = {
    def message: String = getMessageFromMsg(msg)
    topic match {
      case t if t == subTopic => message match {
        case m if m == Msg.regSuccess => _registered = true
        case m if CommandMsg.fromString(m).command == Msg.on => if(turnOn()) sendConfirmUpdate(message)
        case m if CommandMsg.fromString(m).command == Msg.off => if(turnOff()) sendConfirmUpdate(message)
        case _ => if (handleDeviceSpecificMessage(CommandMsg.fromString(message))) sendConfirmUpdate(message)
      }
      case t if t == broadcastTopic => message match {
        case m if m == Msg.disconnected =>
          turnOff()
          _registered = false
        case _ => this.errUnexpected(UnexpectedMessage, message)
      }
      case _ => this.errUnexpected(UnexpectedTopic, topic)
    }
  }

  def handleDeviceSpecificMessage(message: CommandMsg): Boolean

  private def sendConfirmUpdate(message: String): Unit ={
    publish(updateTopic, CommandMsg(CommandMsg.fromString(message).id, Msg.confirmUpdate).toString, this)
  }
}

sealed trait ChangeableValue extends Device {
  //min, max value for the intensity
  def minValue : Int
  def maxValue : Int
  var value : Int

  private def _mapValue: Int => Int = ValueChecker(minValue,maxValue)(_)

  def setValue(newValue: Int): Boolean = { value = _mapValue(newValue); true}
  def getValue: Int = value
}

sealed trait MutableExtras[A <: GenericExtra] extends Device {
  private var _activeExtras: Set[A] = Set()

  def getExtras: Set[A] = _activeExtras
  def addExtra(newExtra: A): Boolean = {_activeExtras += newExtra; true}
  def removeExtra(toRemove: A): Boolean = {_activeExtras -= toRemove; true}
}

sealed trait SensorAssociableDevice[A] extends AssociableDevice {
  private val _minDelta: Double = 0.1  //Sensors only consider variations greater than 10%
  private var _lastVal: Option[A] = None

  def valueChanged(currentVal: A, message: String): Boolean =
    try {
      if (_lastVal.isEmpty || (currentVal match {
        case _val: Double => Math.abs(1-_val/_lastVal.get.asInstanceOf[Double]) > _minDelta
        case _val: Boolean => _val != _lastVal.get.asInstanceOf[Boolean]
        })) {
        return publish(CommandMsg(Msg.nullCommandId, message, currentVal))
      }
      false
    } finally {
      _lastVal = Some(currentVal)
    }
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

case object ThermometerType extends DeviceType {
  override def defaultConsumption: Int = 5
}

case object HygrometerType extends DeviceType {
  override def defaultConsumption: Int = 5
}

case object PhotometerType extends DeviceType {
  override def defaultConsumption: Int = 5
}

case object MotionSensorType extends DeviceType {
  override def defaultConsumption: Int = 5
}

/////////////
/// LIGHT ///
/////////////

object Light {
  def apply(name: String, room: String, deviceType: DeviceType = LightType, consumption: Int = LightType.defaultConsumption): SimulatedLight =
    SimulatedLight(name, room, deviceType, consumption)
}

case class SimulatedLight(override val id: String, override val room: String, override val deviceType: DeviceType,
                          override val consumption: Int, override val minValue : Int = 1, override val maxValue: Int = 100,
                          override var value: Int = 50) extends Device with AssociableDevice with ChangeableValue {
  require(deviceType == LightType)

  override def handleDeviceSpecificMessage(message: CommandMsg): Boolean = message.command match {
      case Msg.setIntensity => setValue(message.value.toInt)
      case _ => this.errUnexpected(UnexpectedMessage, message.command)
  }
}

///////////////////////
/// AIR CONDITIONER ///
///////////////////////

object AirConditioner {
  def apply(name: String, room: String, device_type: DeviceType = AirConditionerType, consumption: Int = AirConditionerType.defaultConsumption): SimulatedAirConditioner =
    SimulatedAirConditioner(name, room, device_type, consumption)
}

case class SimulatedAirConditioner(override val id: String, override val room: String, override val deviceType: DeviceType,
                                   override val consumption: Int, override val minValue : Int = 10, override val maxValue: Int = 35,
                                   override var value: Int = 22) extends Device with AssociableDevice with ChangeableValue {
  require(deviceType == AirConditionerType)

  override def handleDeviceSpecificMessage(message: CommandMsg): Boolean = message.command match {
    case Msg.setTemperature => setValue(message.value.toInt)
    case _ => this.errUnexpected(UnexpectedMessage, message.command)
  }
}

////////////////////
/// DEHUMIDIFIER ///
////////////////////

object Dehumidifier {
  def apply(name: String, room: String, device_type: DeviceType = DehumidifierType, consumption: Int = DehumidifierType.defaultConsumption): SimulatedDehumidifier =
    SimulatedDehumidifier(name, room, device_type, consumption)
}

case class SimulatedDehumidifier(override val id: String, override val room: String, override val deviceType: DeviceType,
                                 override val consumption: Int, override val minValue : Int = 1, override val maxValue: Int = 100,
                                 override var value: Int = 10) extends Device with AssociableDevice with ChangeableValue {
  require(deviceType == DehumidifierType)

  override def handleDeviceSpecificMessage(message: CommandMsg): Boolean = message.command match {
    case Msg.setHumidity => setValue(message.value.toInt)
    case _ => this.errUnexpected(UnexpectedMessage, message.command)
  }
}

///////////////
/// SHUTTER ///
///////////////

object Shutter {
  def apply(name: String, room: String, device_type: DeviceType = ShutterType, consumption: Int = ShutterType.defaultConsumption): SimulatedShutter =
    SimulatedShutter(name, room, device_type, consumption)
}

case class SimulatedShutter(override val id: String, override val room: String, override val deviceType: DeviceType,
                                 override val consumption: Int) extends Device with AssociableDevice {
  require(deviceType == ShutterType)

  private var _open = false

  def isOpen: Boolean = _open

  def open(): Boolean = {_open = true; _open}
  def close(): Boolean = {_open = false; !_open}

  override def handleDeviceSpecificMessage(message: CommandMsg): Boolean = message.command match {
    case Msg.open => open()
    case Msg.close => close()
    case _ => this.errUnexpected(UnexpectedMessage, message.command)
  }
}

//////////////
/// Boiler ///
//////////////

object Boiler {
  def apply(name: String, room: String, device_type: DeviceType = BoilerType, consumption: Int = BoilerType.defaultConsumption): SimulatedBoiler =
    SimulatedBoiler(name, room, device_type, consumption)
}

case class SimulatedBoiler(override val id: String, override val room: String, override val deviceType: DeviceType,
                           override val consumption: Int, override val minValue : Int = 10, override val maxValue: Int = 35,
                           override var value: Int = 22) extends Device with AssociableDevice with ChangeableValue {
  require(deviceType == BoilerType)

  override def handleDeviceSpecificMessage(message: CommandMsg): Boolean = message.command match {
    case Msg.setTemperature => setValue(message.value.toInt)
    case _ => this.errUnexpected(UnexpectedMessage, message.command)
  }
}

///////////
/// TV ///
//////////

object TV {
  def apply(name: String, room: String, device_type: DeviceType = TvType, consumption: Int = TvType.defaultConsumption): SimulatedTV =
    SimulatedTV(name, room, device_type, consumption)
}

case class SimulatedTV(override val id: String, override val room: String, override val deviceType: DeviceType,
                       override val consumption: Int, override val minValue : Int = 0, override val maxValue: Int = 100,
                       override var value: Int = 50) extends Device with AssociableDevice with ChangeableValue {
  require(deviceType == TvType)

  override def handleDeviceSpecificMessage(message: CommandMsg): Boolean = message.command match {
    case Msg.setVolume => setValue(message.value.toInt)
    case Msg.mute => setValue(minValue)
    case _ => this.errUnexpected(UnexpectedMessage, message.command)
  }
}

///////////////////////
/// WASHING MACHINE ///
///////////////////////

object WashingMachine {
  def apply(name: String, room: String, device_type: DeviceType = WashingMachineType, consumption: Int = WashingMachineType.defaultConsumption): SimulatedWashingMachine =
    SimulatedWashingMachine(name, room, device_type, consumption)
}

case class SimulatedWashingMachine(override val id: String, override val room: String, override val deviceType: DeviceType,
                                   override val consumption: Int) extends Device with AssociableDevice with MutableExtras[WashingMachineExtra] {
  require(deviceType == WashingMachineType)

  private var _activeWashing: WashingType = WashingType.MIX
  private var _activeRPM: RPM = RPM.MEDIUM

  def getWashingType: WashingType = _activeWashing
  def setWashingType(newWashing: WashingType): Boolean = {_activeWashing = newWashing; true}
  def getRPM: RPM = _activeRPM
  def setRPM(newRPM: RPM): Boolean = {_activeRPM = newRPM; true}

  override def handleDeviceSpecificMessage(message: CommandMsg): Boolean = message.command match {
    case Msg.washingType => setWashingType(WashingType(message.value))
    case Msg.RPM => setRPM(RPM(message.value))
    case Msg.addExtra => addExtra(WashingMachineExtra(message.value))
    case Msg.removeExtra => removeExtra(WashingMachineExtra(message.value))
    case _ => this.errUnexpected(UnexpectedMessage, message.command)
  }
}

//////////////////
/// DishWasher ///
//////////////////

object DishWasher {
  def apply(name: String, room: String, device_type: DeviceType = DishWasherType, consumption: Int = DishWasherType.defaultConsumption): SimulatedDishWasher =
    SimulatedDishWasher(name, room, device_type, consumption)
}


case class SimulatedDishWasher(override val id: String, override val room: String, override val deviceType: DeviceType,
                               override val consumption: Int) extends Device with AssociableDevice with MutableExtras[DishWasherExtra] {
  require(deviceType == DishWasherType)

  private var _activeWashing: DishWasherProgram = DishWasherProgram.FAST

  def getWashingProgram: DishWasherProgram = _activeWashing
  def setWashingProgram(newWashing: DishWasherProgram): Boolean = {_activeWashing = newWashing; true}

  override def handleDeviceSpecificMessage(message: CommandMsg): Boolean = message.command match {
    case Msg.setProgram => setWashingProgram(DishWasherProgram(message.value))
    case Msg.addExtra => addExtra(DishWasherExtra(message.value))
    case Msg.removeExtra => removeExtra(DishWasherExtra(message.value))
    case _ => this.errUnexpected(UnexpectedMessage, message.command)
  }
}

////////////
/// Oven ///
////////////

object Oven {
  def apply(name: String, room: String, device_type: DeviceType = OvenType, consumption: Int = OvenType.defaultConsumption): SimulatedOven =
    SimulatedOven(name, room, device_type, consumption)
}

case class SimulatedOven(override val id: String, override val room: String, override val deviceType: DeviceType,
                         override val consumption: Int, override val minValue : Int = 0, override val maxValue: Int = 250,
                         override var value: Int = 0) extends Device with AssociableDevice with ChangeableValue {
  require(deviceType == OvenType)

  private var _activeMode: OvenMode = OvenMode.CONVENTIONAL

  def getOvenMode: OvenMode = _activeMode
  def setOvenMode(newMode: OvenMode): Boolean = {_activeMode = newMode; true}

  override def handleDeviceSpecificMessage(message: CommandMsg): Boolean = message.command match {
    case Msg.setTemperature => setValue(message.value.toInt)
    case Msg.setMode => setOvenMode(OvenMode(message.value))
    case _ => this.errUnexpected(UnexpectedMessage, message.command)
  }
}

/////////////////////
/// Stereo System ///
/////////////////////

object StereoSystem {
  def apply(name: String, room: String, device_type: DeviceType = StereoSystemType, consumption: Int = StereoSystemType.defaultConsumption): SimulatedStereoSystem =
    SimulatedStereoSystem(name, room, device_type, consumption)
}

case class SimulatedStereoSystem(override val id: String, override val room: String, override val deviceType: DeviceType,
                                 override val consumption: Int, override val minValue : Int = 0, override val maxValue: Int = 100,
                                 override var value: Int = 50) extends Device with AssociableDevice with ChangeableValue {
  require(deviceType == StereoSystemType)

  override def handleDeviceSpecificMessage(message: CommandMsg): Boolean = message.command match {
    case Msg.setVolume => setValue(message.value.toInt)
    case Msg.mute => setValue(minValue)
    case _ => this.errUnexpected(UnexpectedMessage, message.command)
  }
}

///////////////////
/// THERMOMETER ///
///////////////////

object Thermometer {
  def apply(name: String, room: String, deviceType: DeviceType = ThermometerType, consumption: Int = ThermometerType.defaultConsumption): SimulatedThermometer =
    SimulatedThermometer(name, room, deviceType, consumption)
}

case class SimulatedThermometer(override val id: String, override val room: String, override val deviceType: DeviceType,
                                override val consumption: Int) extends Device with AssociableDevice with SensorAssociableDevice[Double] {
  require(deviceType == ThermometerType)

  def valueChanged(currentVal: Double): Boolean = valueChanged(currentVal, Msg.temperatureRead)

  override def handleDeviceSpecificMessage(message: CommandMsg): Boolean = this.errUnexpected(UnexpectedMessage, message.command)
}

//////////////////
/// HYGROMETER ///
//////////////////

object Hygrometer {
  def apply(name: String, room: String, deviceType: DeviceType = HygrometerType, consumption: Int = HygrometerType.defaultConsumption): SimulatedHygrometer =
    SimulatedHygrometer(name, room, deviceType, consumption)
}

case class SimulatedHygrometer(override val id: String, override val room: String, override val deviceType: DeviceType,
                               override val consumption: Int) extends Device with AssociableDevice with SensorAssociableDevice[Double] {
  require(deviceType == HygrometerType)

  def valueChanged(currentVal: Double): Boolean = valueChanged(currentVal, Msg.humidityRead)

  override def handleDeviceSpecificMessage(message: CommandMsg): Boolean = this.errUnexpected(UnexpectedMessage, message.command)
}

//////////////////
/// PHOTOMETER ///
//////////////////

object Photometer {
  def apply(name: String, room: String, deviceType: DeviceType = PhotometerType, consumption: Int = PhotometerType.defaultConsumption): SimulatedPhotometer =
    SimulatedPhotometer(name, room, deviceType, consumption)
}

case class SimulatedPhotometer(override val id: String, override val room: String, override val deviceType: DeviceType,
                               override val consumption: Int) extends Device with AssociableDevice with SensorAssociableDevice[Double] {
  require(deviceType == PhotometerType)

  def valueChanged(currentVal: Double): Boolean = valueChanged(currentVal, Msg.intensityRead)

  override def handleDeviceSpecificMessage(message: CommandMsg): Boolean = this.errUnexpected(UnexpectedMessage, message.command)
}

//////////////////////
/// MOTION_DETECTOR //
//////////////////////

object MotionSensor {
  def apply(name: String, room: String, deviceType: DeviceType = MotionSensorType, consumption: Int = MotionSensorType.defaultConsumption): SimulatedMotionSensor =
    SimulatedMotionSensor(name, room, deviceType, consumption)
}

case class SimulatedMotionSensor(override val id: String, override val room: String, override val deviceType: DeviceType,
                                 override val consumption: Int) extends Device with AssociableDevice with SensorAssociableDevice[Boolean] {
  require(deviceType == MotionSensorType)

  def valueChanged(currentVal: Boolean): Boolean = valueChanged(currentVal, Msg.motionDetected)

  override def handleDeviceSpecificMessage(message: CommandMsg): Boolean = this.errUnexpected(UnexpectedMessage, message.command)
}