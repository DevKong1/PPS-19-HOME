package HOME

import play.api.libs.functional.syntax._
import play.api.libs.json.{JsObject, _}

trait JSONUtils {
  implicit val deviceTypeWrites: Writes[DeviceType] = (deviceType: DeviceType) => Json.obj(
    "dev_type" -> deviceType.dev_type
  )

  implicit val deviceWrites: Writes[Device] = (device: Device) => Json.obj(
    "name" -> device.name,
    "room" -> device.room,
    "device_type" -> device.device_type.dev_type,
    "consumption" -> device.consumption
  )

  implicit val deviceTypeReads: Reads[DeviceType] = (JsPath \ "dev_type").read[String].map{DeviceType.apply}

  implicit val deviceReads: Reads[Device] = (
    (JsPath \ "name").read[String] and
      (JsPath \ "room").read[String] and
      (JsPath \ "device_type").read[String].map(_ => LightType) and
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
    case jsval => (jsval \ "device").validate[Device].get
  }
}