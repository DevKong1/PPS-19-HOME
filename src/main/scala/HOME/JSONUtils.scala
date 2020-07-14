package HOME

import play.api.libs.functional.syntax._
import play.api.libs.json.{JsObject, _}

trait JSONUtils {
  implicit val deviceTypeWrites: Writes[DeviceType] = (deviceType: DeviceType) => Json.obj(
    "dev_type" -> deviceType.getClass.getSimpleName.split("\\$").last
  )

  implicit val deviceWrites: Writes[Device] = (device: Device) => Json.obj(
    "id" -> device.id,
    "room" -> device.room,
    "device_type" -> device.device_type,
    "consumption" -> device.consumption
  )

  implicit val deviceTypeReads: Reads[DeviceType] = (JsPath \ "dev_type").read[String].map{DeviceType.apply}

  implicit val deviceReads: Reads[Device] = (
    (JsPath \ "id").read[String] and
      (JsPath \ "room").read[String] and
      (JsPath \ "device_type").read[DeviceType] and
      (JsPath \ "consumption").read[Int]
    )(Device.apply _)

  def getRegistrationMsg(device: Device): JsValue = {
    JsObject(
      Seq(
        "msg" -> JsString("register"),
        "device" -> Json.toJson(device),
      )
    )
  }

  def getDeviceFromRegistrationMsg(jsvalue: JsValue): Device = jsvalue match {
    case null => null
    case v => (v \ "device").validate[Device].get
  }
}