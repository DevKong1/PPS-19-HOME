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

  private implicit val jsonSenderWrites: Writes[JSONSender] = {
    case sender@s if s.isInstanceOf[AssociableDevice] => deviceWrites.writes(sender.asInstanceOf[AssociableDevice])
    case sender@s if s.isInstanceOf[Coordinator] => coordinatorWrites.writes(sender.asInstanceOf[Coordinator])
  }

  private implicit val deviceTypeWrites: Writes[DeviceType] = (deviceType: DeviceType) => Json.obj(
    "name" -> deviceType.getSimpleClassName
  )

  private implicit val deviceWrites: Writes[AssociableDevice] = (device: AssociableDevice) => Json.obj(
    typeField -> device.senderType._type,
    "id" -> device.id,
    "room" -> device.room,
    "device_type" -> device.deviceType,
    "consumption" -> device.consumption,
    "pubTopic" -> (if (device.pubTopic == null) "" else device.pubTopic)
  )

  private implicit val coordinatorWrites: Writes[Coordinator] = (coordinator: Coordinator) => Json.obj(
    typeField -> coordinator.senderType._type,
    "name" -> coordinator.name
  )

  private implicit val jsonSenderReads: Reads[JSONSender] = {
    case sender@s if (s \ typeField).validate[String].get == SenderTypeDevice._type => deviceReads.reads(sender.asInstanceOf[JsObject])
    case sender@s if (s \ typeField).validate[String].get == SenderTypeCoordinator._type => coordinatorReads.reads(sender.asInstanceOf[JsObject])
  }

  private implicit val deviceTypeReads: Reads[DeviceType] = (JsPath \ "name").read[String].map {
    DeviceType.apply
  }

  private implicit val deviceReads: Reads[AssociableDevice] = (
    (JsPath \ "id").read[String] and
      (JsPath \ "room").read[String] and
      (JsPath \ "device_type").read[DeviceType] and
      (JsPath \ "consumption").read[Int] and
      (JsPath \ "pubTopic").read[String]
    ) (AssociableDevice.apply _)

  private implicit val coordinatorReads: Reads[Coordinator] = (JsPath \ "name").read[String].map {
    Coordinator.apply
  }

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
