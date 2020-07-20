package HOME

import HOME.MyClass._
import scala.collection.mutable.ListBuffer

sealed trait Coordinator extends JSONSender with MQTTUtils {
  override def senderType: SenderType = SenderTypeCoordinator
  override def name: String = "Coordinator"
  override def lastWillTopic: String = broadcastTopic
  override def lastWillMessage: String = Msg.disconnected

  var devices: Set[Device]
  var activeProfile: Profile
  var subTopics: ListBuffer[String]

  def addDevice(device: Device): Unit
  def removeDevice(device: Device): Unit
  def getDevices : Set[Device]

  def connect: Boolean
  def disconnect: Boolean
  def subscribe: Boolean
  def publish(topic: String, message: CommandMsg): Boolean
  def publish(topic: String, message: String): Boolean

  def onMessageReceived(topic:String, message: String): Unit
}

object Coordinator {
  def apply(name: String): Coordinator = CoordinatorImpl()
}

case class CoordinatorImpl() extends Coordinator {

  override var devices: Set[Device] = Set()
  override var activeProfile: Profile = Profile(ProfileNameDefault)
  override var subTopics: ListBuffer[String] = new ListBuffer[String]()

  override def addDevice(device: Device): Unit = devices += device

  override def removeDevice(device: Device): Unit = devices -= device

  override def getDevices: Set[Device] = devices

  override def connect: Boolean = connect(this, onMessageReceived)

  override def subscribe: Boolean = subscribe(regTopic)

  override def publish(topic: String, message: CommandMsg): Boolean = publish(topic, message, this, !retained)
  override def publish(topic: String, message: String): Boolean = publish(topic, message, this)

  override def onMessageReceived(topic: String, message: String): Unit = topic match {
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
        subscribe(device.pubTopic)
        publish(device.subTopic, Msg.regSuccess)
      case m if m == Msg.disconnected =>
        removeDevice(device)
        unsubscribe(device.pubTopic)
      case m => this.errUnexpected(UnexpectedMessage, m)
    }
  }
}

sealed trait Profile {
  val name: ProfileName
  val description: String

  def applyRoutine(): Unit
  def onMessageReceived(): Unit
}

sealed trait ProfileName {
  def name: String
}

case object ProfileNameDefault extends ProfileName {
  override def name: String = "DEFAULT"
}

object Profile {
  private class DEFAULT_PROFILE extends Profile  {
    override val name: ProfileName = ProfileNameDefault
    override val description: String = "Default Profile"

    override def applyRoutine(): Unit = {}
    override def onMessageReceived(): Unit = {}
  }

  def apply(name: ProfileName): Profile = name match {
    case ProfileNameDefault => new DEFAULT_PROFILE
    case _ => new DEFAULT_PROFILE
  }
}