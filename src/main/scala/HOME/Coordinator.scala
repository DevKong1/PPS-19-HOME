package HOME

import scala.collection.mutable.ListBuffer

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
  def subscribe(): Unit
  def publish(topic: String, message: String): Boolean

  def onMessageReceived(topic:String, message: String): Unit

}

case class CoordinatorImpl() extends Coordinator with MQTTUtils {
  override var devices: Set[Device] = Set()
  override var activeProfile: Profile = Profile("default")
  override var subTopics: ListBuffer[String] = new ListBuffer[String]()

  override def addDevice(device: Device): Unit = devices += device

  override def removeDevice(device: Device): Unit = devices -= device

  override def getDevices: Set[Device] = devices

  override def connect: Boolean = connect(name, broadcastTopic, onMessageReceived)

  override def subscribe(): Unit = subscribe(regTopic)

  override def publish(topic: String, message: String): Boolean = publish(topic, message, !retained)

  override def onMessageReceived(topic: String, message: String): Unit = topic match {
    case t if t == regTopic => message match {
      case m if m startsWith disconnectedMsg => println("Device " + m stripPrefix disconnectedMsg + " removed")
      //TODO remove device
      case m if m startsWith regMsg => println("new Device " + m stripPrefix regMsg + " added")
      //TODO subscribe to device pubTopic and add device to list
      case _ => throw new IllegalArgumentException("Unexpected message: " + message)
    }
    case _ => throw new IllegalArgumentException("Unexpected topic: " + topic)
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