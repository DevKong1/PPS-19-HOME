package HOME

import HOME.MyClass._

import scala.collection.mutable.ListBuffer
import scala.runtime.Nothing$

object Coordinator extends JSONSender with MQTTUtils {
  override def senderType: SenderType = SenderTypeCoordinator
  override def name: String = "Coordinator"
  override def lastWillTopic: String = broadcastTopic
  override def lastWillMessage: String = Msg.disconnected

  var devices: Set[Device] = Set()
  var activeProfile: Profile = Profile("DEFAULT")
  var subTopics: ListBuffer[String] = new ListBuffer[String]()

  def addDevice(device: Device): Unit = devices += device

  def removeDevice(device: String): Unit = devices --= devices.filter(_.name == device)

  def getDevices: Set[Device] = devices

  def getActiveProfile: Profile = activeProfile
  def setProfile(newProfile: Profile): Unit = activeProfile = newProfile

  def connect: Boolean = connect(this, onMessageReceived)

  def subscribe: Boolean = subscribe(regTopic)

  def publish(device: AssociableDevice, message: CommandMsg): Boolean = publish(device.getSubTopic, message, this, !retained)
  def publish(topic: String, message: String): Boolean = publish(topic, message, this)

  def onMessageReceived(topic: String, message: String): Unit = topic match {
    case t if t == regTopic => handleRegMsg(message)
    //TODO topic+message managed by the active profile
    case _ => this.errUnexpected(UnexpectedTopic, topic)
  }

  def handleRegMsg(msg: String): Unit = {
    val device: AssociableDevice = getSenderFromMsg[AssociableDevice](msg)
    if (device == null) this.errUnexpected(UnexpectedDevice, null)

    getMessageFromMsg(msg) match {
      case m if m == Msg.register =>
        addDevice(device)
        subscribe(device.getPubTopic)
        publish(device.getSubTopic, Msg.regSuccess)
      case m if m == Msg.disconnected =>
        removeDevice(device.name)
        unsubscribe(device.getPubTopic)
      case m => this.errUnexpected(UnexpectedMessage, m)
    }
  }
}

object Rooms {
  private var _allRooms = Set("Salotto", "Home", "Kitchen", "Garage", "Bedroom", "Bagno", "Corridoio")

  def addRoom(room: String): Unit = _allRooms += room
  def removeRoom(room: String): Unit = _allRooms -= room  //TODO remove all devices in the room
  def allRooms: Set[String] = _allRooms
}
////////////////
/// PROFILES ///
////////////////

sealed trait Profile {
  val name: String
  val description: String

  def onActvation(): Unit

  def onThermometerNotification(): Unit
  def onHygrometerNotification(): Unit
  def onPhotometerNotification(): Unit
  def onMotionSensorNotification(): Unit

  def doProgrammedRoutine(): Unit

  override def equals(o: Any): Boolean = o match {
    case profile: Profile if this.name == profile.name => true
    case _ => false
  }
}

//////////////////////
/// BASIC PROFILES ///
//////////////////////

trait BasicProfile extends Profile {
  val initialRoutine: Device => Unit
  val thermometerNotificationCommands: Device => Unit
  val hygrometerNotificationCommands: Device => Unit
  val photometerNotificationCommands: Device => Unit
  val motionSensorNotificationCommands: Device => Unit

  val programmedRoutineCommands: Device => Unit

  def applyCommand(command: Device => Unit): Unit = {
    for (device <- Coordinator.getDevices) {
      command(device)
    }
  }

  override def onActvation(): Unit = applyCommand(initialRoutine)

  override def onThermometerNotification(): Unit = applyCommand(thermometerNotificationCommands)
  override def onHygrometerNotification(): Unit = applyCommand(hygrometerNotificationCommands)
  override def onPhotometerNotification(): Unit = applyCommand(photometerNotificationCommands)
  override def onMotionSensorNotification(): Unit = applyCommand(motionSensorNotificationCommands)
}

object Profile {
  var savedProfiles: Set[Profile] = Set(DEFAULT_PROFILE, NIGHT)

  def getProfiles: Set[Profile] = savedProfiles
  def addProfile(profile: Profile): Unit = savedProfiles += profile

  private case object DEFAULT_PROFILE extends BasicProfile  {
    override val name: String = Constants.default_profile_name
    override val description: String = "Default Profile"

    override val initialRoutine: Device => Unit = null
    override val thermometerNotificationCommands: Device => Unit = null
    override val hygrometerNotificationCommands: Device => Unit = null
    override val photometerNotificationCommands: Device => Unit = null
    override val motionSensorNotificationCommands: Device => Unit = null
    override val programmedRoutineCommands: Device => Unit = null

    override def doProgrammedRoutine(): Unit = {}
  }

  private case object NIGHT extends BasicProfile  {
    override val name: String = "NIGHT"
    override val description: String = "Default Profile"

    override val initialRoutine: Device => Unit = {
      case device: AssociableDevice if device.deviceType == LightType => Coordinator.publish(device.getSubTopic, Msg.off)
      case device: AssociableDevice if device.deviceType == ShutterType => Coordinator.publish(device.getSubTopic, Msg.close)
      case device: AssociableDevice if device.deviceType == TvType => Coordinator.publish(device.getSubTopic, Msg.mute)
    }

    //TODO REMPO _.id , SHOULD BE NULL OR SOMETHING
    override val thermometerNotificationCommands: Device => Unit = _.id
    override val hygrometerNotificationCommands: Device => Unit = _.id
    override val photometerNotificationCommands: Device => Unit = _.id
    override val motionSensorNotificationCommands: Device => Unit = _.id
    override val programmedRoutineCommands: Device => Unit = _.id

    override def doProgrammedRoutine(): Unit = {}
  }

  def apply(name: String): Profile = getProfiles.find(_.name == name) match {
    case Some(t) => t
    case _ => this.errUnexpected(UnexpectedProfile, name)
  }
}

///////////////////////
/// CUSTOM PROFILES ///
///////////////////////

case class CustomProfile(override val name: String, override val description: String,
                         initialRoutineSet: Set[Device => Unit], thermometerNotificationCommandsSet: Set[Device => Unit],
                         hygrometerNotificationCommandsSet: Set[Device => Unit], photometerNotificationCommandsSet: Set[Device => Unit],
                         motionSensorNotificationCommandsSet: Set[Device => Unit], programmedRoutineCommandsSet: Set[Device => Unit],
                         override val doProgrammedRoutine: Unit) extends Profile {

  val initialRoutine: Set[Device => Unit] = initialRoutineSet
  val thermometerNotificationCommands: Set[Device => Unit] =  thermometerNotificationCommandsSet
  val hygrometerNotificationCommands: Set[Device => Unit] = hygrometerNotificationCommandsSet
  val photometerNotificationCommands: Set[Device => Unit] = photometerNotificationCommandsSet
  val motionSensorNotificationCommands: Set[Device => Unit] = motionSensorNotificationCommandsSet

  val programmedRoutineCommands: Set[Device => Unit] = programmedRoutineCommandsSet

  def applyCommand(commands: Set[Device => Unit]): Unit = {
    for (device <- Coordinator.getDevices) {
      for(command <- commands) {
        command(device)
      }
    }
  }

  override def onActvation(): Unit = applyCommand(initialRoutine)

  override def onThermometerNotification(): Unit = applyCommand(thermometerNotificationCommands)
  override def onHygrometerNotification(): Unit = applyCommand(hygrometerNotificationCommands)
  override def onPhotometerNotification(): Unit = applyCommand(photometerNotificationCommands)
  override def onMotionSensorNotification(): Unit = applyCommand(motionSensorNotificationCommands)
}

object CustomProfileBuilder {

  //Set of device and command
  def generateCommandSet(commands: Set[(Device,CommandMsg)]): Set[Device => Unit] = {
    var result: Set[Device => Unit] = Set.empty

    for(command <- commands) {
      val device: Device = command._1
      val message: CommandMsg = command._2

      result += {
        _.id match {
          case t if t == device.id => Coordinator.publish(device.asInstanceOf[AssociableDevice], message) //TODO asInstanceOf only cause simulated
        }
      }
    }
    result
  }

    def generateFromParams(name: String, description: String,
                           initialRoutine: Set[Device => Unit], thermometerNotificationCommands: Set[Device => Unit],
                           hygrometerNotificationCommands: Set[Device => Unit], photometerNotificationCommands: Set[Device => Unit],
                           motionSensorNotificationCommands: Set[Device => Unit], programmedRoutineCommands: Set[Device => Unit],
                           doProgrammedRoutine: Unit): Profile =  CustomProfile(name, description, initialRoutine, thermometerNotificationCommands,
                                                                                hygrometerNotificationCommands, photometerNotificationCommands,
                                                                                motionSensorNotificationCommands, programmedRoutineCommands, doProgrammedRoutine)
}
