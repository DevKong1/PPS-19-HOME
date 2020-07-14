package HOME

import play.api.libs.json.{JsValue, Json}

import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

sealed trait Coordinator {

  var devices: Set[Device]
  var activeProfile: Profile
  var subTopics: ListBuffer[String]

  val name = "Coordinator"

  def addDevice(device: Device): Unit
  def removeDevice(device: Device): Unit
  def getDevices : Set[Device]

  def connect: Boolean
  def disconnect: Boolean
  def subscribe: Boolean
  def publish(topic: String, message: String): Boolean

  def onMessageReceived(topic:String, message: String): Unit
}

case class CoordinatorImpl() extends Coordinator with MQTTUtils with JSONUtils {
  override var devices: Set[Device] = Set()
  override var activeProfile: Profile = Profile("default")
  override var subTopics: ListBuffer[String] = new ListBuffer[String]()

  override def addDevice(device: Device): Unit = devices += device

  override def removeDevice(device: Device): Unit = devices -= device

  override def getDevices: Set[Device] = devices

  override def connect: Boolean = connect(name, broadcastTopic, onMessageReceived)

  override def subscribe: Boolean = subscribe(regTopic)

  override def publish(topic: String, message: String): Boolean = publish(topic, message, !retained)

  override def onMessageReceived(topic: String, message: String): Unit = topic match {
    case t if t == regTopic => handleRegMsg(message)
    case _ => throw new IllegalArgumentException("Unexpected topic: " + topic)
  }

  def handleRegMsg(message: String): Unit = {
    Try {
      val jsValue: JsValue = Json.parse(message)
      val device: AssociableDevice = getDeviceFromRegistrationMsg(jsValue)
      if (device == null) throw new IllegalArgumentException

      getMsgField(jsValue) match {
        case m if m == regMsg =>
          addDevice(device)
          subscribe(device.pubTopic)
        case m if m == disconnectedMsg =>
          val device: AssociableDevice = getDeviceFromRegistrationMsg(jsValue)
          removeDevice(getDeviceFromRegistrationMsg(jsValue))
          unsubscribe(device.pubTopic)
        case _ => throw new IllegalArgumentException
      }
    } match {
      case Failure(exception) =>
        client = null
        throw new IllegalArgumentException(s"ERROR : $exception + ${exception.getCause}")
      case Success(_) => true
      case _ => throw new IllegalArgumentException("Unexpected result")
    }

  }
}

sealed trait Profile {
  val name: String
  val description: String

  def applyRoutine(): Unit
  def onMessageReceived(): Unit
}

object Profile {

  private class DEFAULT_PROFILE extends Profile {
    override val name: String = "DEFAULT"
    override val description: String = "Default Profile"

    override def applyRoutine(): Unit = {}
    override def onMessageReceived(): Unit = {}
  }

  def apply(name: String): Profile = name.toUpperCase match {
    case _ => new DEFAULT_PROFILE
  }

}