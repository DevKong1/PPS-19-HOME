package HOME

object Application{

  var devicesPerRoom = Set()

  def main(args: Array[String]): Unit = {
    Coordinator.connect
    Constants.defaultRooms foreach Rooms.addRoom
    GUI.setRooms(Constants.defaultRooms)
    GUI
  }
}
