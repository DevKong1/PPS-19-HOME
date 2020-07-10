package HOME

object Rooms {
  //TODO THIS LIST SHOULD BE MADE IN THE INTERFACE
  private val _allRooms = Array("salotto")

  def allRooms = _allRooms
}

sealed trait Device {
  def name : String
  def room : String
  def device_type : String
  def consumption : Int
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
  def apply(name: String, room: String, device_type: String = "Light", consumption: Int = 5) = new SimulatedLight(name,room,device_type,consumption)
}

case class SimulatedLight(override val name: String, override val room: String, override val device_type: String, override val consumption: Int) extends Device {

  require(device_type == "Light")
  require(Rooms.allRooms contains room, "Incorrect room")

  private var _on = false

  //min, max value for the intensity
  val minIntensity = 1
  val maxIntensity = 100
  private var intensity = 50

  def getIntensity = intensity
  def isOn = _on

  def turnOn(): Unit = _on = true
  def turnOff(): Unit = _on = false

  private def _mapIntensity = IntensityChecker(minIntensity,maxIntensity)(_)
  def setIntensity(value: Int): Unit = intensity = _mapIntensity(value)

  override def equals(that: Any) = that match {
    case SimulatedLight(name,_,_,_) => this.name == name
    case _ => false
  }


}
