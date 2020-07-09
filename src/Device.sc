sealed trait Device {

  def name : String
  def room : String
  def device_type : String
  def consumption : Int

}

sealed trait DeviceFactory {

  def Light(): Device

}

//helper object used by various devices to set the output strength
object IntensityChecker {
    def apply(min: Int, max: Int)(value: Int): Int = value match {
      case x if x > max => max
      case x if x < min => min
      case _ => value
    }
}

object Light {

  def apply(name: String, room: String, device_type: String, consumption: Int, on: Boolean) = new SimulatedLight(name,room,device_type,consumption)

}

case class SimulatedLight(override val name: String, override val room: String, override val device_type: String, override val consumption: Int) extends Device {

  private var _on = false
  private var intensity = 1
  private def _mapIntensity = IntensityChecker(1,100)

  def turnOn(): Unit = _on = true
  def turnOff(): Unit = _on = false

  def setIntensity(value: Int): Unit = _mapIntensity(value)

  override def equals(that: Any) = that match {
    case SimulatedLight(name,_,_,_) => this.name == name
    case _ => false
  }

}