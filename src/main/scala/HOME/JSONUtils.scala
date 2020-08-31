package HOME

import java.lang.reflect.MalformedParametersException

import HOME.MyClass._
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsObject, _}

import scala.util.{Failure, Success, Try}

/** Used by AssociableDevices and Coordinator when publishing messages **/
sealed trait SenderType {
  def _type : String
}

case object SenderTypeDevice extends SenderType {
  override def _type: String = "device"
}

case object SenderTypeCoordinator extends SenderType {
  override def _type: String = "coordinator"
}

/** Basic JSON message Sender over MQTT **/
trait JSONSender {
  def senderType: SenderType
  def name: String
  def lastWillTopic: String
  def lastWillMessage: String
}

/** This trait helps with the writing and reading operations of JSON messages.
 *  Each JSON message contains a body and the sender information
**/
trait JSONUtils {
  private val msgField: String = "msg"
  private val senderField: String = "sender"
  private val typeField: String = "type"
  private val nameField: String = "name"
  private val idField: String = "id"
  private val roomField: String = "room"
  private val deviceTypeField: String = "deviceType"
  private val consumptionField: String = "consumption"

  /** Used to handle a JSONSender write to a JSON message **/
  private implicit val jsonSenderWrites: Writes[JSONSender] = {
    case sender@s if s.isInstanceOf[AssociableDevice] => deviceWrites.writes(sender.asInstanceOf[AssociableDevice])
    case s if s == Coordinator => coordinatorWrites.writes(Coordinator)
  }

  /** Used to write a DeviceType to a JSON message **/
  private implicit val deviceTypeWrites: Writes[DeviceType] = (deviceType: DeviceType) => Json.obj(
    nameField -> deviceType.getSimpleClassName
  )

  /** Used to write an AssociableDevice to a JSON message **/
  private implicit val deviceWrites: Writes[AssociableDevice] = (device: AssociableDevice) => Json.obj(
    typeField -> device.senderType._type,
    idField -> device.id,
    roomField -> device.room,
    deviceTypeField -> device.deviceType,
    consumptionField -> device.consumption,
  )

  /** Used to write the Coordinator to a JSON message **/
  private implicit val coordinatorWrites: Writes[Coordinator.type] = _ => Json.obj(
    typeField -> Coordinator.senderType._type,
    nameField -> Coordinator.name
  )

  /** Used to handle a JSONSender read from a JSON message **/
  private implicit val jsonSenderReads: Reads[JSONSender] = {
    case sender@s if (s \ typeField).validate[String].get == SenderTypeDevice._type => deviceReads.reads(sender.asInstanceOf[JsObject])
    case sender@s if (s \ typeField).validate[String].get == SenderTypeCoordinator._type => coordinatorReads.reads(sender)
  }

  /** Used to read a DeviceType from a JSON message **/
  private implicit val deviceTypeReads: Reads[DeviceType] = (JsPath \ nameField).read[String].map {
    DeviceType.apply
  }

  /** Used to read an AssociableDevice from a JSON message **/
  private implicit val deviceReads: Reads[AssociableDevice] = (
    (JsPath \ idField).read[String] and
      (JsPath \ roomField).read[String] and
      (JsPath \ deviceTypeField).read[DeviceType] and
      (JsPath \ consumptionField).read[Int]
    ) (AssociableDevice.apply _)

  /** Used to read the Coordinator from a JSON message **/
  private implicit val coordinatorReads: Reads[Coordinator.type] = (JsPath \ nameField).read[String].map {
    _ => Coordinator
  }

  /** Creates the JSON message from the command message and the sender and returns it to String **/
  def getMsg(command: CommandMsg, sender: JSONSender): String = getMsg(command.toString, sender)

  /** Creates the JSON message from the message and the sender and returns it to String
   *
   * @param message the message body
   * @param sender the message sender
   * @return  [[String]] the JSON message to String
   */
  def getMsg(message: String, sender: JSONSender): String = {
    if (message == null || sender == null) return null
    JsObject(
      Seq(
        msgField -> JsString(message),
        senderField -> Json.toJson(sender),
      )
    ).toString()
  }

  /** Retrieves the message body from the JSON string message **/
  def getMessageFromMsg(msg: String): String = toJsValue(msg) match {
      case v if v == null => null
      case v => (v \ msgField).validate[String].get
    }

  /** Retrieves the message sender from the JSON string message **/
  def getSenderFromMsg[A >: Null <: JSONSender](msg: String): A = toJsValue(msg) match {
      case v if v == null => null
      case v => (v \ senderField).validate[JSONSender].get.asInstanceOf[A]
  }

  /** Parses a JSON string message and returns its JsValue representation **/
  private def toJsValue(msg: String): JsValue = {
    if (msg == null) return null
    Try {
      Json.parse(msg)
    } match {
      case Failure(exception) =>
        throw new MalformedParametersException(s"ERROR : $exception + ${exception.getCause}")
      case Success(value) => value
      case result => this.errUnexpected(UnexpectedResult, result.get.toString())
    }
  }
}
