package HOME

import play.api.libs.functional.syntax._
import play.api.libs.json.{JsObject, _}
import MyClass._

trait JSONUtils extends MQTTUtils {
  implicit val deviceTypeWrites: Writes[DeviceType] = (deviceType: DeviceType) => Json.obj(
    "dev_type" -> deviceType.getSimpleClassName
  )

  implicit val deviceWrites: Writes[AssociableDevice] = (device: AssociableDevice) => Json.obj(
    "id" -> device.id,
    "room" -> device.room,
    "device_type" -> device.device_type,
    "consumption" -> device.consumption,
    "pubTopic" -> (if(device.pubTopic == null) "" else device.pubTopic)
  )

  implicit val deviceTypeReads: Reads[DeviceType] = (JsPath \ "dev_type").read[String].map{DeviceType.apply}

  implicit val deviceReads: Reads[AssociableDevice] = (
    (JsPath \ "id").read[String] and
      (JsPath \ "room").read[String] and
      (JsPath \ "device_type").read[DeviceType] and
      (JsPath \ "consumption").read[Int] and
      (JsPath \ "pubTopic").read[String]
    )(AssociableDevice.apply _)

  def getRegistrationMsg(device: AssociableDevice): JsValue = {
    JsObject(
      Seq(
        "msg" -> JsString(regMsg),
        "device" -> Json.toJson(device),
      )
    )
  }

  def getDeviceFromRegistrationMsg(jsvalue: JsValue): AssociableDevice = jsvalue match {
    case null => null
    case v => (v \ "device").validate[AssociableDevice].get
  }

  def getMsgField(jsvalue: JsValue): String = jsvalue match {
    case null => null
    case v => (v \ "msg").validate[String].get
  }
}
