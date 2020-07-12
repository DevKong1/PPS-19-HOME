package HOME

sealed trait Coordinator {

  var devices: Set[Device]
  var activeProfile: Profile

  val subTopic: String
  val generalTopic: String

  def addDevice(device: Device): Unit
  def removeDevice(device: Device): Unit
  def getDevices : Set[Device]

  def connect(): Boolean
  def subscribe(): Unit
  def publish[A](message: A): Boolean

  def onMessageReceived[A](message: A): Unit

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