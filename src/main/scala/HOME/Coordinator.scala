package HOME

import scala.collection.mutable.ListBuffer

sealed trait Coordinator {

  var devices: ListBuffer[Device]
  var activeProfile: Profile

  val subTopic: String
  val generalTopic: String

  def addDevice(device: Device): Unit
  def removeDevice(device: Device): Unit
  def getDevices : List[Device]

  def connect(): Boolean
  def subscribe(): Unit
  def publish[A](message: A): Boolean

  def onMessageReceived[A](message: A): Unit

}

case class CoordinatorImpl() extends Coordinator {
  override var devices: ListBuffer[Device] = new ListBuffer[Device]()
  override var activeProfile: Profile = Profile("default")
  override val subTopic: String = ""
  override val generalTopic: String = ""

  override def addDevice(device: Device): Unit = devices += device

  override def removeDevice(device: Device): Unit = devices -= device

  override def getDevices: List[Device] = devices.toList

  override def connect(): Boolean = ???

  override def subscribe(): Unit = ???

  override def publish[A](message: A): Boolean = ???

  override def onMessageReceived[A](message: A): Unit = ???
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