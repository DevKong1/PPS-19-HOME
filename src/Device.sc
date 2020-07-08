sealed trait Device {

  def name : String
  def room : String
  def device_type : String
  def consumption : Int

}

sealed trait DeviceFactory {

  def Light(): Device

}

object Light {

  def apply(name: String, room: String, device_type: String, consumption: Int, on: Boolean) = new SimulatedLight(name,room,device_type,consumption)

}

case class SimulatedLight(override val name: String, override val room: String, override val device_type: String, override val consumption: Int) extends Device {

  private var _on = false

  override def equals(that: Any) = ???

}