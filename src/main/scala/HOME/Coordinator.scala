package HOME

import HOME.MyClass._
import scala.collection.mutable.ListBuffer

object Coordinator extends JSONSender with MQTTUtils {
  override def senderType: SenderType = SenderTypeCoordinator
  override def name: String = "Coordinator"
  override def lastWillTopic: String = broadcastTopic
  override def lastWillMessage: String = Msg.disconnected

  var devices: Set[Device] = Set()
  var activeProfile: Profile = Profile("DEFAULT")
  var subTopics: ListBuffer[String] = new ListBuffer[String]()

  def addDevice(device: Device): Unit = devices += device

  def removeDevice(device: Device): Unit = devices -= device

  def getDevices: Set[Device] = devices

  def getActiveProfile: Profile = activeProfile

  def connect: Boolean = connect(this, onMessageReceived)

  def subscribe: Boolean = subscribe(regTopic)

  def publish(topic: String, message: CommandMsg): Boolean = publish(topic, message, this, !retained)
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
        removeDevice(device)
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

sealed trait Profile {

  val name: String
  val description: String

  var initialRoutine: Set[Device => Unit]
  var thermometerNotificationCommands: Set[Device => Unit]
  var hygrometerNotificationCommands: Set[Device => Unit]
  var photometerNotificationCommands: Set[Device => Unit]
  var motionSensorNotificationCommands: Set[Device => Unit]

  var programmedRoutineCommands: Set[Device => Unit]

  def applyCommands(commands: Set[Device => Unit]): Unit = {
    for (device <- Coordinator.getDevices) {
      for (command <- commands) {
        command(device)
      }
    }
  }

  def onActvation(): Unit = applyCommands(initialRoutine)

  def onThermometerNotification(): Unit = applyCommands(thermometerNotificationCommands)
  def onHygrometerNotification(): Unit = applyCommands(hygrometerNotificationCommands)
  def onPhotometerNotification(): Unit = applyCommands(photometerNotificationCommands)
  def onMotionSensorNotification(): Unit = applyCommands(motionSensorNotificationCommands)

  def doProgrammedRoutine(): Unit

}

object Profile {
  def getProfiles: Set[Profile] = Set(DEFAULT_PROFILE, NIGHT)

  private case object DEFAULT_PROFILE extends Profile  {
    override val name: String = Constants.default_profile_name
    override val description: String = "Default Profile"

    override var initialRoutine: Set[Device => Unit] = Set()
    override var thermometerNotificationCommands: Set[Device => Unit] = Set()
    override var hygrometerNotificationCommands: Set[Device => Unit] = Set()
    override var photometerNotificationCommands: Set[Device => Unit] = Set()
    override var motionSensorNotificationCommands: Set[Device => Unit] = Set()
    override var programmedRoutineCommands: Set[Device => Unit] = Set()

    override def doProgrammedRoutine(): Unit = {}
  }

  private case object NIGHT extends Profile  {
    override val name: String = "NIGHT"
    override val description: String = "Default Profile"

    override var initialRoutine: Set[Device => Unit] = Set(
      {_.deviceType match { case LightType => ???}}
    )
    override var thermometerNotificationCommands: Set[Device => Unit] = Set()
    override var hygrometerNotificationCommands: Set[Device => Unit] = Set()
    override var photometerNotificationCommands: Set[Device => Unit] = Set()
    override var motionSensorNotificationCommands: Set[Device => Unit] = Set()
    override var programmedRoutineCommands: Set[Device => Unit] = Set()

    override def doProgrammedRoutine(): Unit = {}
  }



  def apply(name: String): Profile = getProfiles.find(_.name == name) match {
    case Some(t) => t
    case _ => this.errUnexpected(UnexpectedProfile, name)
  }

}
