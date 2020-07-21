package HOME

import java.lang.reflect.MalformedParametersException

import HOME.MyClass._
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsObject, _}

import scala.util.{Failure, Success, Try}

sealed trait SenderType {
  def _type : String
}

case object SenderTypeDevice extends SenderType {
  override def _type: String = "device"
}

case object SenderTypeCoordinator extends SenderType {
  override def _type: String = "coordinator"
}

trait JSONSender {
  def senderType: SenderType
  def name: String
  def lastWillTopic: String
  def lastWillMessage: String
}

trait JSONUtils {
  private val msgField: String = "msg"
  private val senderField: String = "sender"
  private val typeField: String = "type"
  private val nameField: String = "name"
  private val idField: String = "id"
  private val roomField: String = "room"
  private val deviceTypeField: String = "deviceType"
  private val consumptionField: String = "consumption"

  private implicit val jsonSenderWrites: Writes[JSONSender] = {
    case sender@s if s.isInstanceOf[AssociableDevice] => deviceWrites.writes(sender.asInstanceOf[AssociableDevice])
    case sender@s if s.isInstanceOf[Coordinator] => coordinatorWrites.writes(sender.asInstanceOf[Coordinator])
  }

  private implicit val deviceTypeWrites: Writes[DeviceType] = (deviceType: DeviceType) => Json.obj(
    nameField -> deviceType.getSimpleClassName
  )

  private implicit val deviceWrites: Writes[AssociableDevice] = (device: AssociableDevice) => Json.obj(
    typeField -> device.senderType._type,
    idField -> device.id,
    roomField -> device.room,
    deviceTypeField -> device.deviceType,
    consumptionField -> device.consumption,
  )

  private implicit val coordinatorWrites: Writes[Coordinator] = (coordinator: Coordinator) => Json.obj(
    typeField -> coordinator.senderType._type,
    nameField -> coordinator.name
  )

  private implicit val jsonSenderReads: Reads[JSONSender] = {
    case sender@s if (s \ typeField).validate[String].get == SenderTypeDevice._type => deviceReads.reads(sender.asInstanceOf[JsObject])
    case sender@s if (s \ typeField).validate[String].get == SenderTypeCoordinator._type => coordinatorReads.reads(sender.asInstanceOf[JsObject])
  }

  private implicit val deviceTypeReads: Reads[DeviceType] = (JsPath \ nameField).read[String].map {
    DeviceType.apply
  }

  private implicit val deviceReads: Reads[AssociableDevice] = (
    (JsPath \ idField).read[String] and
      (JsPath \ roomField).read[String] and
      (JsPath \ deviceTypeField).read[DeviceType] and
      (JsPath \ consumptionField).read[Int]
    ) (AssociableDevice.apply _)

  private implicit val coordinatorReads: Reads[Coordinator] = (JsPath \ nameField).read[String].map {
    Coordinator.apply
  }

  def getMsg(command: CommandMsg, sender: JSONSender): String = getMsg(command.toString, sender)

  def getMsg(message: String, sender: JSONSender): String = {
    if (message == null || sender == null) return null
    JsObject(
      Seq(
        msgField -> JsString(message),
        senderField -> Json.toJson(sender),
      )
    ).toString()
  }

  def getMessageFromMsg(msg: String): String = toJsValue(msg) match {
      case v if v == null => null
      case v => (v \ msgField).validate[String].get
    }

  def getSenderFromMsg[A >: Null <: JSONSender](msg: String): A = toJsValue(msg) match {
      case v if v == null => null
      case v => (v \ senderField).validate[JSONSender].get.asInstanceOf[A]
  }

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
