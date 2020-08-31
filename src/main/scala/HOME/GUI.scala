package HOME

import java.awt.{Color, Toolkit}
import java.net.URL

import HOME.MyClass._
import javax.swing.border.{LineBorder, TitledBorder}
import javax.swing.{Box, ImageIcon, SwingUtilities}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.language.postfixOps
import scala.swing.Dialog.{Message, Result}
import scala.swing._
import scala.swing.event.{ButtonClicked, MouseClicked, SelectionChanged, ValueChanged}
import scala.util.Success

/** Entity updatable by coordinator and printable in graphical interface*/
sealed trait Updatable{
  /**Receives update by coordinator
   *
   * @param cmdString device property to update
   * @param newVal new value to set
   */
  def updateDevice(cmdString: String,newVal:String):Unit

  /**
   * device held by this entity
   * @return device
   */
  def device:Device

  /**
   * graphical representation of this entity
   * @return graphical representation
   */
  def printPane(): Component
}

/** A room is a core component in GUI
 * used by [[GUIRoom]]
 */
sealed trait Room {
  /** devices can be of any form as long as they can be updated by coordinator and "printable"
   */
  var devices : Set[Updatable]
  def name : String
  def addDevice(dev:Device): Unit
  def removeDevice(dev:Device): Unit
}

/** Provides an interface for communicating updates to [[Coordinator]]
 * every feature that can be updated by user needs to implement this trait,
 * see [[BinaryFeature]],[[DeviceFeature]]
 *
 */
sealed trait EditableFeature{
  /** sends device's properties update to coordinator
   *
   * @param devName device updating its feature
   * @param cmdMsg [[Msg]] type of update
   * @param newValue new feature value
   * @return future stating whether the connected device updated its value
   *
   * Such return promise will be completed only when the physically connected device updates
   * it's feature value and sends a confirm back to [[Coordinator]].
   * Update confirmation leads to feature update in GUI.
   */
  def userUpdate(devName : String,cmdMsg :String,newValue:String): Future[Unit] = {
    val p = Promise[Unit]
    Coordinator.sendUpdate(devName, cmdMsg, newValue).onComplete {
      case Success(_) => setVal(newValue); p.success(() => Unit);
      case _ => Dialog.showMessage(title ="Update Error",message = "Something wrong happened while trying to update a device",messageType = Message.Error);  p.failure(_)
    }
    p.future
  }

  /**  feature value getter
   *
   * @return feature value
   */
  def getVal : String
  /** feature value setter */
  def setVal(v:String) : Unit
}

/** Graphical representation of a
 *
 * @param name room name
 * @param devices devices in the room
 */
class GUIRoom(override val name:String, override var devices:Set[Updatable]) extends ScrollPane with Room {
  val devicePanel = new BoxPanel(Orientation.Vertical)
  val adDeviceBtn: Button =
    new Button("Add device") {
      reactions += {
        case ButtonClicked(_) => DeviceDialog(devicePanel)
      }
    }
  val bp: BorderPanel = new BorderPanel {
    add(new FlowPanel() {
      border = new TitledBorder("Sensors")
      contents ++= devices.filter( dev => Device.isSensor(dev.device)).map(_.printPane())
    },BorderPanel.Position.North)

    add(devicePanel, BorderPanel.Position.Center)
    add(adDeviceBtn, BorderPanel.Position.South)
  }
  contents = bp
  for (i <- devices.filter( dev => !Device.isSensor(dev.device))) addDevicePane(i.printPane())


  /** Adds a GUIDevice to room
   *
   * @param dev GUIDevice to add to room
   * utility function used internally
   */
  private def addDevicePane(dev : Component): Unit ={
    devicePanel.peer.add(Box.createVerticalStrut(Constants.GUIDeviceGAP))
    devicePanel.contents += dev
  }

  /** adds a new device to room
   *
   * @param dev device to be added
   */
  override def addDevice(dev:Device): Unit ={
    if (this.devices.exists(_.device.id == dev.id)) return
    val tmp = PrintDevicePane(dev)
    addDevicePane(tmp)
    devices+=tmp
    this.repaint()
  }

  /** removes a device from room
   *
   * @param dev device to be removed
   */
  override def removeDevice(dev:Device):Unit={
    devices -= devices.find(_.device.name == dev.name).get
    Coordinator.removeDevice(dev.id)
  }
}
/**Factory for [[GUIRoom]]*/
object GUIRoom{
  /**
   *
   * @param roomName room name
   * @param devices devices in room
   * @return a new instance of [[GUIRoom]]
   *
   * provides abstraction between devices and [[GUIDevice]]
   */
  def apply(roomName: String,devices:Set[Device]): GUIRoom = new GUIRoom(roomName,devices.map(PrintDevicePane(_)))
}

/** Layout of Home's page
 *
 * @param name name of the Home
 * @param devices list of home's device
 */
class HomePageLayout(override val name:String, override var devices:Set[Updatable]) extends BoxPanel(Orientation.Vertical) with Room{
  val avgTemp = new Label("Internal temperature: ")
  val avgHum = new Label("Internal humidity: ")
  val actualCons = new Label("Actual consume: ")
  val totalCons = new Label("Total consume: ")
  val getCons: Button = new Button("Get consume") {
    reactions += {
      case ButtonClicked(_) => actualCons.text = "Actual consume: " + BigDecimal(Coordinator.getActiveConsumption).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble+"W"
        totalCons.text = "Total consume: " + BigDecimal(Coordinator.getTotalConsumption).setScale(5, BigDecimal.RoundingMode.HALF_UP).toDouble+"kW"
    }
  }
  val bp: FlowPanel = new FlowPanel() {
    border = new TitledBorder("Outdoor sensors")
    contents ++= devices.filter( dev => Device.isSensor(dev.device)).map(_.printPane())
  }
  val welcomePanel: FlowPanel = new FlowPanel() {
    contents += new Label("Welcome to your HOME") {
      font = new Font("Arial", 0, 36)
    }
  }
  val datePanel: FlowPanel = new FlowPanel() {
    hGap = 70
    contents ++= Seq(new Label("Date: " + DateTime.getDate), new Label("Time: " + DateTime.getCurrentTime))
  }
  val temperaturePanel: FlowPanel = new FlowPanel() {
    hGap = 70
    contents += avgTemp
  }
  val humidityPanel: FlowPanel = new FlowPanel() {
    hGap = 70
    contents +=avgHum
  }
  val consumePanel: FlowPanel = new FlowPanel() {
    hGap = 70
    contents ++= Seq(getCons, actualCons, totalCons)
  }
  val currentProfile = new Label("Current active profile: " + Coordinator.getActiveProfile.name)
  val profilePanel: FlowPanel = new FlowPanel() {
    hGap = 70
    contents ++= Seq(currentProfile,
      new Button("Change profile") {
        reactions += {
          case ButtonClicked(_) => ChangeOrDeleteProfile(this.text, currentProfile)
        }
      },
      new Button("Delete profile") {
        reactions += {
          case ButtonClicked(_) => ChangeOrDeleteProfile(this.text, currentProfile)
        }
      },
      new Button("Create profile") {
        reactions += {
          case ButtonClicked(_) => CreateProfile()
        }
      }
    )
  }
  contents ++= Seq(bp, welcomePanel, datePanel, temperaturePanel, humidityPanel, consumePanel, profilePanel)
  /**No need to add device to HomePage*/
  override def addDevice(dev:Device): Unit ={}

  /** removes a device from room
   *
   * @param dev device to be removed
   *
   * can't remove devices in home
   */
  override def removeDevice(dev:Device):Unit={}

  /** Called when a thermometer/hygrometer updates its value and average value needs to be updated */
  def updateTemp(newVal:Double): Unit = avgTemp.text = "Average temperature: "+newVal.round+"°C"
  def updateHum(newVal:Double): Unit = avgHum.text = "Average humidity: "+newVal.round+"%"

}

/** Factory for [[HomePageLayout]]
 *
 */
object HomePage {
  def apply(roomName:String, devices:Set[Device]): HomePageLayout = new HomePageLayout(roomName, devices.map(PrintDevicePane(_)))
}


/** Singleton GUI for HOME system
 *
 * GUI is made by a [[TabbedPane]] where each page is a [[GUIRoom]]
 */
object GUI extends MainFrame {
  var rooms: Set[Room] = Set.empty
  val home: HomePageLayout = HomePage("Home", Coordinator.getDevices.filter(_.room == "Home"))
  private var avgTemp : Map[String,Double] = Map.empty
  private var avgHum : Map[String,Double] = Map.empty

  for(room <- Rooms.allRooms) room match {
    case "Home" =>
    case _ => rooms += GUIRoom(room, Coordinator.getDevices.filter(_.room == room))
  }
  private val tp: TabbedPane = new TabbedPane {
    //Initializing basic rooms
    pages+= new TabbedPane.Page("Home", home.asInstanceOf[HomePageLayout])
    for(i <- rooms) pages += new TabbedPane.Page(i.name,i.asInstanceOf[GUIRoom])
    rooms+=home
    pages+= new TabbedPane.Page(Constants.AddPane,new BorderPanel())
  }

  /** GUI's version of a main method.
   *
   * @return system's GUI
   */
  def top: MainFrame = new MainFrame {
    title = "HOME"
    reactions += {
      /**
       * Whenever the last [[TabbedPane.Page]] is clicked the procedure
       * for instantiating a new room is started.
       *
       * 'for' used as a sequence control
       */
      case SelectionChanged(_) =>
        for {
          last <- getLastIndex
          name <- getRoomName
        } yield {
          val devices = Constants.devicesPerRoom(name)
          val newRoom = GUIRoom(name,devices)
          RegisterDevice(devices.map(_.asInstanceOf[AssociableDevice]))
          val newRoomPane = new TabbedPane.Page(name, newRoom)
          Rooms.addRoom(name)
          rooms += newRoom
          tp.selection.page = newRoomPane
          tp.pages.insert(last.index, newRoomPane)
        }
    }
    //used to set items in the main window inside a vertical BoxPanel
    contents = tp
    listenTo(tp.selection)
    size = WindowSize(WindowSizeType.MainW)
  this.peer.setLocationRelativeTo(null)
  this.visible = true
  }

  /** Room currently open in GUI
   *
   * @return room name
   */
   def getCurrentRoom: String = {
    tp.selection.page.title
  }

  /** Remove a device from GUI
   *
   * @param device device to be removed
   */
  def removeDevice(device:Device) : Unit = {
    val room = rooms.find(_.devices.map(_.device.name) contains device.name)
    if (room.isDefined) room.get.removeDevice(device)
  }

  /** Updates a device's feature
   *
   * @param d device to update
   * @param cmdMsg [[Msg]] type of update
   * @param newVal new feature value
   *
   * Called whenever a profile makes a change to a device feature and needs to reflect such change to GUI devices
   */
   def updateDevice(d: Device,cmdMsg:String,newVal:String):Unit =
    rooms.find(_.devices.map(_.device.name).contains(d.name)).get.devices.find(_.device.name == d.name).get.updateDevice(cmdMsg,newVal)

  /** Updates average temperature in home
   *
   * @param id thermometer that changed its value
   * @param newVal new temperature value
   */
  def tempAvgUpdate(id:String,newVal:Double): Unit = {
    if (home != null) {
      avgTemp += (id->newVal)
      home.updateTemp(avgTemp.values.sum/avgTemp.size)
    }
  }

  /** Updates average humidity in home
   *
   * @param id hygrometer that changed its value
   * @param newVal new humidity value
   */
  def humAvgUpdate(id:String,newVal:Double):Unit = {
    if (home != null) {
      avgHum += (id -> newVal)
      home.updateHum(avgHum.values.sum/avgHum.size)
    }
  }

  /** Shutdown application */
  override def closeOperation(): Unit = {
    super.closeOperation()
    Application.closeApplication()
  }

  /** Index of clicked page in [[TabbedPane]]
   *
   * @return page clicked
   */
  private def getLastIndex: Option[TabbedPane.Page] = {
    tp.selection.page.title match {
      case Constants.AddPane => tp.pages.find(page => page.title equals Constants.AddPane)
      case _ => None
    }
  }

  /** Dialog where user can choose new room's name
   *
   * @return  [[Some]] new room name if user input is valid, [[None]] otherwise
   */
  private def getRoomName: Option[String] = {
    import Dialog._
    val name = showInput(tp,
      "Room name:",
      "Add room",
      Message.Plain,
      Swing.EmptyIcon,
      Nil, "")
    /*Saddest input check ever*/
    if (name.isDefined && CheckNonNull(name.get) && !(name.get == Constants.AddPane) && !tp.pages.exists(_.title == name.get)) {
      Rooms.addRoom(name.get)
      name
    } else {
      if (name.isDefined) {
        showMessage(tp, "Room already existing or incorrect room name", Message.Error toString)
      }
      None
    }
  }
}

/** Dialog through which users can add devices to a room
 *
 */
class AddDeviceDialog(devicePanel: BoxPanel) extends Dialog {
  private val dimension = WindowSize(WindowSizeType.DialogInput)
  //Can't add sensors
  private val deviceType = new ComboBox[DeviceType](DeviceType.listTypes -- Seq(MotionSensorType,ThermometerType,HygrometerType) toSeq)

  preferredSize = dimension
  title = "Add device"
  contents = new BoxPanel(Orientation.Vertical) {
    private val labels = new FlowPanel() {
      contents ++= Seq(new Label("Device name: "),new Label("Device type: "),deviceType)
    }
    private val buttons = new FlowPanel() {
      contents ++= Seq(
        new Button("Create"){
          reactions += {
            case ButtonClicked(_) =>
              val room = GUI.getCurrentRoom
              val dev = Device(deviceType.selection.item.toString,DeviceIDGenerator(),room).get.asInstanceOf[AssociableDevice]
              RegisterDevice(dev).onComplete {
                    case Success(_) => GUI.rooms.find(_.name equals room).get.addDevice(dev)
                      SwingUtilities.invokeLater(() => {
                        devicePanel.validate()
                        devicePanel.repaint()
                      })
                      close
                    case _ => Dialog.showMessage(message = "Couldn't add device, try again", messageType = Dialog.Message.Error)
                }
          }
        },
        new Button("Cancel") {
          reactions += {
            case ButtonClicked(_) =>
              close()
          }
        })
    }
    contents++= Seq(labels,buttons)
  }
  this.peer.setLocationRelativeTo(null)
  open()
}

/** Factory for [[AddDeviceDialog]]
 *
 */
object DeviceDialog {
  def apply(devicePanel: BoxPanel): AddDeviceDialog = {
    new AddDeviceDialog(devicePanel)
  }
}

/** Dialog through users can change an active profile or delete a profile
 *
 * @param delete use to create dialog
 * @param labelProfile use to change active profile
 */
class ChangeOrDeleteProfileDialog(delete: String, labelProfile: Label) extends Dialog {
  private val dimension = WindowSize(WindowSizeType.AddProfile)
  private val profiles = new ComboBox[String](Profile.getProfileNames toSeq)
  preferredSize = dimension
  modal = true
  private val dialog = new BoxPanel(Orientation.Vertical) {
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += new FlowPanel() {
        contents ++= Seq(new Label("Profiles: "), profiles)
      }
    }
    contents += applyDialog
  }
  contents = dialog
  this.peer.setLocationRelativeTo(null)
  open()

  /** Apply correct dialog matching with Button's name
   *
   * @return
   */
  def applyDialog: Button = {
    delete match {
      case "Change profile" =>
        this.title = "Change Profile"
        new Button("Confirm") {
          reactions += {
            case ButtonClicked(_) => changeProfile(delete)
          }
        }
      case "Delete profile" =>
        this.title = "Delete Profile"
        new Button("Delete") {
          reactions += {
            case ButtonClicked(_) => changeProfile(delete)
          }
        }
    }
  }

  /** Used to active a profile or delete a profile
   *
   * @param name Profile's name
   */
  def changeProfile(name: String): Unit = {
    val selectedProfile = profiles.selection.item
    name match {
      case "Change profile" =>
        labelProfile.text = "Current active profile: " + selectedProfile
        Coordinator.setProfile(Profile(selectedProfile))
      case _ => Profile.removeProfile(selectedProfile)
    }
    close()
  }
}

/** Factory for [[ChangeOrDeleteProfileDialog]]
 *
 */
object ChangeOrDeleteProfile {
  def apply(delete: String, labelProfile: Label): ChangeOrDeleteProfileDialog = {
    new ChangeOrDeleteProfileDialog(delete, labelProfile)
  }
}

/** Use to create a custom profile
 *
 */
class CreateProfileDialog extends Dialog {
  private val profileName = new TextField(10)
  private val description = new TextField(10)
  var onActivationCommands: Set[(Device, CommandMsg)] = Set.empty
  var sensorRules: List[(String, Double, String, Device)] = List.empty
  var thermometerNotificationCommands: List[(List[(String, Double, String, Device)], Set[(Device, CommandMsg)])] = List.empty
  var hygrometerNotificationCommands: List[(List[(String, Double, String, Device)], Set[(Device, CommandMsg)])] = List.empty
  var photometerNotificationCommands: List[(List[(String, Double, String, Device)], Set[(Device, CommandMsg)])] = List.empty
  var motionSensorNotificationCommands: List[(List[(String, String, Device)], Set[(Device, CommandMsg)])] = List.empty
  title = "New Profile"
  modal = true

  private val newProfileDialog = this

  contents = new GridPanel(6,2) {
    contents += new FlowPanel() {
      contents ++= Seq(new Label("Insert a profile name: "), profileName)
    }
    contents += new FlowPanel() {
      contents ++= Seq(new Label("Insert a description: "), description)
    }
    contents += new FlowPanel() {
      contents ++= Seq(new Label("On activation: "),
        new Button("Add rules") {
        reactions += {case ButtonClicked(_) => AllDevice(Rooms.allRooms, newProfileDialog, null)}}
      )
    }
    contents += new FlowPanel() {
      contents ++= Seq(new Label("On sensor changed: "), new Button("Add rules") {
        reactions += { case ButtonClicked(_) => SensorReaction(newProfileDialog)}}
      )
    }
    contents += new FlowPanel() {
      contents += new Button("Confirm") {
        reactions += {
          case ButtonClicked(_) =>
            if (AlertMessage.alertIsCorrectName(profileName.text.map(_.toUpper))) {
              val generatedOnActivationCommand: Set[Device => Unit] = CustomProfileBuilder.generateCommandSet(onActivationCommands)
              var generatedThermometerSensorCommandsMap: Map[(String, Double) => Boolean, Set[Device => Unit]] = Map.empty
              var generatedHygrometerSensorCommandsMap: Map[(String, Double) => Boolean, Set[Device => Unit]] = Map.empty
              var generatedPhotometerSensorCommandsMap: Map[(String, Double) => Boolean, Set[Device => Unit]] = Map.empty
              var generatedMotionSensorCommandsMap: Map[String, Set[Device => Unit]] = Map.empty

              //for each Sensor with attached commands
              for (rules <- sensorRules.groupBy(_._4)) {
                if (rules._1.deviceType == MotionSensorType) {
                  //get all the commands associated to this Sensor
                  val motionSensorCommands = motionSensorNotificationCommands.filter(_._1.head._3 == rules._1).flatMap(_._2).toSet
                  val generatedMotionSensorCommands = CustomProfileBuilder.generateCommandSet(motionSensorCommands)
                  generatedMotionSensorCommandsMap = generatedMotionSensorCommandsMap + (rules._1.room -> generatedMotionSensorCommands)
                } else {
                  for (rule <- rules._2) {
                    val checkFunction = CustomProfileBuilder.generateCheckFunction(rule._1, rule._2, rule._4.room)
                    if (thermometerNotificationCommands.nonEmpty) {
                      val thermometerCommands = thermometerNotificationCommands.filter(_._1.head.equals(rule._1, rule._2, rule._3, rule._4)).flatMap(_._2).toSet
                      val generatedThermometerCommands = CustomProfileBuilder.generateCommandSet(thermometerCommands)
                      generatedThermometerSensorCommandsMap = generatedThermometerSensorCommandsMap + (checkFunction -> generatedThermometerCommands)
                    }
                    if (hygrometerNotificationCommands.nonEmpty) {
                      val hygrometerCommands = hygrometerNotificationCommands.filter(_._1.head.equals(rule._1, rule._2, rule._3, rule._4)).flatMap(_._2).toSet
                      val generatedHygrometerCommands = CustomProfileBuilder.generateCommandSet(hygrometerCommands)
                      generatedHygrometerSensorCommandsMap = generatedHygrometerSensorCommandsMap + (checkFunction -> generatedHygrometerCommands)
                    }
                    if (photometerNotificationCommands.nonEmpty) {
                      val photometerCommands = photometerNotificationCommands.filter(_._1.head.equals(rule._1, rule._2, rule._3, rule._4)).flatMap(_._2).toSet
                      val generatedPhotometerCommands = CustomProfileBuilder.generateCommandSet(photometerCommands)
                      generatedPhotometerSensorCommandsMap = generatedPhotometerSensorCommandsMap + (checkFunction -> generatedPhotometerCommands)
                    }
                  }
                }
              }
              //instantiate the new Custom Profile
              val newProfile = CustomProfileBuilder.generateFromParams(profileName.text.map(_.toUpper), description.text, generatedOnActivationCommand, generatedThermometerSensorCommandsMap,
                generatedHygrometerSensorCommandsMap, generatedPhotometerSensorCommandsMap, generatedMotionSensorCommandsMap, DummyUtils.dummySet, {})
              Profile.addProfile(newProfile)
              close()
            } else {

            }
        }
      }
      contents += new Button("Cancel") {
        reactions += {
          case ButtonClicked(_) => close()
        }
      }
    }
  }
  this.peer.setLocationRelativeTo(null)
  open()
}

/** Factory for [[CreateProfileDialog]]
 *
 */
object CreateProfile {
  def apply(): CreateProfileDialog = {
    new CreateProfileDialog()
  }
}

/** Dialog used to add sensor's rule on profile activation
 *
 * @param dialog instance to add sensor's rules
 */
class SensorReactionDialog(dialog: CreateProfileDialog) extends Dialog {
  modal = true
  title = "Sensor Reaction"
  location = new Point(300,0)
  preferredSize = new Dimension(900, 400)
  contents = new ScrollPane() {
    contents = applyTemplate
  }

  var key: List[(String, Double, String, Device)] = List.empty
  val emptySet: Set[(Device, CommandMsg)] = Set.empty

  def applyTemplate : BoxPanel = {
    val panel = new BoxPanel(Orientation.Vertical)
    for(i <- Coordinator.getDevices) {
      val devicePanel = new BoxPanel(Orientation.Horizontal)
      devicePanel.peer.add(Box.createVerticalStrut(10))
      devicePanel.border = new LineBorder(Color.BLACK, 2)
      if(Device.isSensor(i)) {
        val comboRooms: StringComboBox = StringComboBox(Rooms.allRooms toSeq)
        val value = new TextField(10)
        devicePanel.contents += new FlowPanel() {
          contents += new Label(i.deviceType+"_"+i.room+": ")
          contents += applyComponent(i, this)
          if(i.deviceType != MotionSensorType) {
            contents += value
          }
          contents += new Label("Select rooms where you want to apply rules")
          contents += comboRooms
          contents += new Button("Do") {
            reactions += {
              case ButtonClicked(_) =>
                for(sym <- devicePanel.contents(1).asInstanceOf[FlowPanel].contents) yield {
                  sym match {
                    case x: ComboBox[_] if !x.equals(comboRooms) =>
                      i.deviceType match {
                        case MotionSensorType => key = List((giveSymbol(sym), Double.NaN, comboRooms.selection.item, i))
                        case _ =>if (AlertMessage.alertIsCorrectValue(value.text)) {
                          key = List((giveSymbol(sym), value.text.toDouble, comboRooms.selection.item, i))
                        } else {
                          key = List.empty
                        }
                      }
                    case _ =>
                  }
                }
                if (key.nonEmpty) {
                  dialog.sensorRules ++= key
                  roomsDevices(comboRooms.selection.item)
                }
            }
          }
        }
        panel.contents += devicePanel
      }
    }
    panel.contents += new Button("Confirm") {
      reactions += {
        case ButtonClicked(_) => close()
      }
    }
    panel
  }

  /** Used to create correct Component matching device's type
   *
   * @param dev instance of Device
   * @param panel instance of Panel
   * @return the correct Component
   */
  def applyComponent(dev: Device, panel: FlowPanel) : Component = dev.deviceType match {
    case MotionSensorType => panel.contents+=new Label("Motion ")
      StringComboBox(Set("Detecting") toSeq)
    case HygrometerType =>  panel.contents+=new Label("Humidity ")
      StringComboBox(Set("=", ">=", "<=", ">", "<") toSeq)
    case PhotometerType => panel.contents+=new Label("Intensity ")
      StringComboBox(Set("=", ">=", "<=", ">", "<") toSeq)
    case ThermometerType => panel.contents+=new Label("Temperature ")
      StringComboBox(Set("=", ">=", "<=", ">", "<") toSeq)
    case _ => this.errUnexpected(UnexpectedDeviceType, dev.deviceType.toString)
  }

  /** Give symbol of selected item
   *
   * @param x used to match the Component
   * @return Give the correct item
   */
  def giveSymbol(x: Any): String = x match {
    case p: StringComboBox => p.selection.item
    case _ => ""
  }

  /** Used to Create a dialog with all room's devices
   *
   * @param room Room where you want all devices
   * @return a Dialog with all room's devices
   */
  def roomsDevices(room: String) : Dialog = {
    AllDevice(Set(room), dialog, key)
  }
  this.peer.setLocationRelativeTo(null)
  open()
}

/** Factory for [[SensorReactionDialog]]
 *
 */
object SensorReaction {
  def apply(dialog: CreateProfileDialog): SensorReactionDialog = {
    new SensorReactionDialog(dialog)
  }
}

/** Dialog to get all Home's devices
 *
 * @param rooms Rooms you want to add rules
 * @param dialog Dialog instance to add commands
 * @param sensorRule list of sensor's rules
 */
class AllDeviceDialog(rooms: Set[String], dialog: CreateProfileDialog, sensorRule: List[(String, Double, String, Device)]) extends Dialog {
  modal = true
  title = "All Devices"
  location = new Point(300, 250)
  preferredSize = new Dimension(1000, 500)

  contents = new ScrollPane() {
    contents = applyTemplate
  }

  def applyTemplate: BoxPanel = {
    val panel = new BoxPanel(Orientation.Vertical)
    for (i <- Coordinator.getDevices) {
      val devPanel = new BoxPanel(Orientation.Horizontal)
      if (!Device.isSensor(i) && rooms.contains(i.room)) {
        val applyButton = new Button("Add")
        devPanel.peer.add(Box.createVerticalStrut(10))
        devPanel.border = new LineBorder(Color.BLACK, 2)
        devPanel.contents += new FlowPanel() {
          contents += new Label(i.deviceType + "_" + i.room)
          MapDeviceCommands.apply(i)
          for (a <- MapDeviceCommands.getCommands) {
            val component = applyComponent(a, i, this)
            applyButton.reactions += {
              case ButtonClicked(_) => addRule(component, i, switchStatus(a))
            }
          }
          contents += applyButton
        }
        panel.contents += devPanel
      }
    }

    panel.contents += new FlowPanel() {
      contents += new Button("Confirm") {
        reactions += {
          case ButtonClicked(_) => close()
        }
      }
    }
    panel
  }

  /** Give the correct Component matching command
   *
   * @param command command used to match
   * @param device  instance of Device
   * @param panel   instance of Panel
   * @return the correct Component to add
   */
  def applyComponent(command: String, device: Device, panel: FlowPanel): Component = command match {
    case Msg.washingType => device.deviceType match {
      case WashingMachineType =>
        val component = StringComboBox(Seq(WashingType.MIX, WashingType.RAPID, WashingType.WOOL) map (_.toString))
        panel.contents ++= Seq(new Label("Washing type: "), component)
        component
      case _ => null
    }
    case Msg.setProgram => device.deviceType match {
      case DishWasherType =>
        val component = StringComboBox(Seq(DishWasherProgram.DIRTY, DishWasherProgram.FAST, DishWasherProgram.FRAGILE) map (_.toString))
        panel.contents ++= Seq(new Label("Program type: "), component)
        component
      case _ => null
    }
    case Msg.RPM => device.deviceType match {
      case WashingMachineType | DishWasherType =>
        val component = StringComboBox(Seq(RPM.SLOW, RPM.MEDIUM, RPM.FAST) map (_.toString))
        panel.contents ++= Seq(new Label("RPM: "), component)
        component
      case _ => null
    }
    case Msg.addExtra => device.deviceType match {
      case WashingMachineType =>
        val component = StringComboBox(Seq(WashingMachineExtra.SpecialColors, WashingMachineExtra.SuperDirty, WashingMachineExtra.SuperDry) map (_.toString))
        panel.contents ++= Seq(new Label("Extras: "), component)
        component
      case DishWasherType =>
        val component = StringComboBox(Seq(DishWasherExtra.SuperDirty, DishWasherExtra.SuperHygiene, DishWasherExtra.SuperSteam) map (_.toString))
        panel.contents ++= Seq(new Label("Extras: "), component)
        component
      case _ => null
    }
    case Msg.setMode => device.deviceType match {
      case OvenType =>
        val component = StringComboBox(Seq(OvenMode.CONVENTIONAL, OvenMode.DEFROSTING, OvenMode.GRILL, OvenMode.LOWER,
          OvenMode.UPPER, OvenMode.VENTILATED) map (_.toString))
        panel.contents ++= Seq(new Label("Working mode: "), component)
        component
      case _ => null
    }
    case Msg.close | Msg.mute | Msg.off =>
      val component = new ToggleButton(command) {
        reactions += {
          case ButtonClicked(_) => this.text = switchStatus(this.text)
        }
      }
      panel.contents ++= Seq(component)
      component
    case _ =>
      val component = new TextField(10)
      panel.contents ++= Seq(new Label(command), component)
      component
  }

  /** Used to switch ToggleButton text
   *
   * @param status status used to match
   * @return A status who corresponding the opposite to initial status
   */
  def switchStatus(status: String): String = status match {
    case "on" => "off"
    case "off" => "on"
    case "close" => "open"
    case "open" => "close"
    case _ => status
  }

  /** Used to add rule when a profile will be active
   *
   * @param component component where you want to get rule
   * @param device    device where you want to apply the rule
   * @param command   command to apply
   */
  def addRule(component: Component, device: Device, command: String): Unit = {
    val componentValue = getComponentInfo(component, command)
    componentValue match {
      case "" =>
      case _ => sensorRule match {
        case null => dialog.onActivationCommands ++= Set((device, CommandMsg(Msg.nullCommandId, command, componentValue)))
        case _ => sensorRule.head._4.deviceType match {
          case ThermometerType => dialog.thermometerNotificationCommands ++= List((sensorRule, Set((device, CommandMsg(Msg.nullCommandId, command, componentValue)))))
          case PhotometerType => dialog.photometerNotificationCommands ++= List((sensorRule, Set((device, CommandMsg(Msg.nullCommandId, command, componentValue)))))
          case HygrometerType => dialog.hygrometerNotificationCommands ++= List((sensorRule, Set((device, CommandMsg(Msg.nullCommandId, command, componentValue)))))
          case MotionSensorType => dialog.motionSensorNotificationCommands ++= List((List((sensorRule.head._1, sensorRule.head._3, sensorRule.head._4)),
            Set((device, CommandMsg(Msg.nullCommandId, command, componentValue)))))
          case _ =>
        }
      }
    }
  }


  /** Return correct Components value
   *
   * @param x Component used to match
   * @param command Command used to match
   * @return the correct value
   */
  def getComponentInfo(x: Any, command: String): String = x match {
    case p: TextField => command match {
      case Msg.setIntensity | Msg.setTemperature | Msg.setHumidity | Msg.setVolume => if(!p.text.isEmpty) p.text else ""
      case _ => ""
    }
    case p: ToggleButton => command match {
      case Msg.on | Msg.off | Msg.close | Msg.open | Msg.mute => if(p.selected) p.text else ""
      case _ => ""
    }
    case p: StringComboBox => command match {
      case Msg.washingType | Msg.RPM | Msg.addExtra | Msg.setMode | Msg.setProgram => p.selection.item
      case _ => ""
    }
    case _ => ""
  }
  this.peer.setLocationRelativeTo(null)
  open()
}

/** Factory for [[AllDeviceDialog]]
 *
 */
object AllDevice {
  def apply(rooms: Set[String], dialog: CreateProfileDialog, sensorRule: List[(String, Double, String, Device)]): AllDeviceDialog = {
    new AllDeviceDialog(rooms, dialog, sensorRule)
  }
}

/**
 * Graphical representation of a device and its features, every device has its own implementation.
 *  @param device the device represented by this panel.
 */
abstract class GUIDevice(override val device : Device) extends FlowPanel with Updatable{
  override val name: String = device.name
  val ON = "ON"
  val OFF = "OFF"
  private val devType = new Label("DeviceType: "+device.deviceType)
  private val status: BinaryFeature = BinaryFeature(device.name,OFF,Msg.off,ON,Msg.on)
  lazy val close: Unit = this.visible = false

  border = new LineBorder(Color.BLACK, 2)
  contents += new DeviceIcon(device.deviceType.toString)
  private val deviceInfo = new GridPanel(2, 2)
  deviceInfo.contents ++= Seq(
    devType,
    new Label("Consumption: " + device.consumption),
    status,
    new Button("Delete") {
      reactions +={
        case ButtonClicked(_) => Dialog.showConfirmation(message="Are you sure you want to delete this device? There is no coming back",title ="Delete device") match{
          case Result.Ok => GUI.removeDevice(device); close
          case _ => //Do nothing
        }
      }
    }
  )

  if (!Device.isSensor(device)) contents += deviceInfo

  /**
   * Updates this device and its graphical representation.
   *
   * @param cmdString the command this device has to execute
   * @param newVal to set the property to.
   *
   * Provides the two basic commands every device needs to respond to;
   * called whenever a profile needs to update a device.
   */
  override def updateDevice(cmdString: String,newVal:String): Unit = cmdString match {
    case msg :String if msg == Msg.on => device.turnOn(); status.setVal("ON")
    case msg :String if msg == Msg.off => device.turnOff(); status.setVal("OFF")
    case _ =>
  }

  /** adds a user-editable feature
   *
   * @param feature feature to add
   * @tparam A needs to be a printable entity[[Component]] updatable by user[[EditableFeature]]
   */
  def addFeatures[A <: Component with EditableFeature](feature: (String,A)*): Unit ={
    for(feat <- feature) {
      val (name,newFeature) = feat
      contents ++= Seq(new Label(name), newFeature)
    }
  }
  /** prints this GUIDevice */
  override def printPane() : Component = this

  /** An icon inside a label.
   *
   * images are stored inside a resource folder and accessed by deviceType.
   * @param iconName type of icon to load
   *
   * Icons are stored under deviceType name.
   */
  private class DeviceIcon(iconName :String) extends Label {
    text = iconName
    border = new LineBorder(Color.black,1)

    private val iconUrl :URL = this.getClass.getResource("/" + iconName + Constants.IconExt)
    icon = new ImageIcon(Toolkit.getDefaultToolkit.getImage(iconUrl))

    horizontalTextPosition = Alignment.Center
    verticalTextPosition = Alignment.Bottom
  }
}
/** A device feature that can be changed either by user or profile.
 *
 * @param deviceName name of the device this feature belongs to.
 * @param featureTitle name of such feature
 * @param initialValue current value to display
 * @param setterComponent component through which users can change feature val
 *                        @tparam A Component with the ability to return the new value set by user and to set it (if needed) by [[Coordinator]] via [[EditableFeature]]
 * @param updateType type of update([[Msg]]) this feature sends to coordinator
 *
 *  once a click is performed on this label, [[setterComponent]] is opened and user can change feature value; if so,
 *  such change is sent to coordinator via [[Promise]]. When the promise completes, this feature value is updated.
 */
case class DeviceFeature[A<: Component { def setVal(v:String): Unit; def getVal:String }](deviceName :String,featureTitle : String, initialValue: String, setterComponent: A ,updateType:String) extends Label with EditableFeature{
  text = initialValue
  border = new LineBorder(Color.black,1)
  reactions+={
    case MouseClicked(_,_,_,_,_) => new Dialog(){
      title = featureTitle
      import scala.language.reflectiveCalls
      private val value : Label = new Label(setterComponent.getVal)

      contents = new BoxPanel(Orientation.Vertical) {
        contents ++= Seq(
          new FlowPanel() {
            contents ++= Seq(
              new Label("Set "+featureTitle+": "),
              setterComponent,
              value
            )
          },
          new FlowPanel() {
            contents ++= Seq(
              new Button("Confirm") {
                reactions += {
                  case ButtonClicked(_) => userUpdate(deviceName,updateType,setterComponent.getVal).onComplete{
                        /** Can't update label val from inside this dialog */
                    case Success(_) => setVal(setterComponent.getVal); close()
                    case _ => Dialog.showMessage(title = "Update error",message = "Something went wrong while updating a device",messageType= Message.Error)
                  }
                }
              },
              new Button("Cancel") {
                reactions += {
                  case ButtonClicked(_) => close()
                }
              })
          }
        )
      }
      reactions+={
        case ValueChanged(_) => value.text = setterComponent.getVal;
      }
      listenTo(setterComponent)
      this.peer.setLocationRelativeTo(null)
      open()
    }
  }

  listenTo(mouse.clicks)
  this.visible = true

  /** feature value getter
   *
   * @return feature value
   */
  override def getVal: String = text

  /** feature value setter */
  override def setVal(v: String): Unit = text = v
}
/** More basic feature than [[DeviceFeature]]
 *
 * @param devName device name this feature belongs to
 * @param toDisplay first possible value
 * @param displayCmd [[Msg]] to send when switching to [[toDisplay]]
 * @param other second possible value
 * @param otherCmd [[Msg]] to send when switching to [[other]]
 *
 * Represents a binary feature of a device (es. Yes/No), see [[ShutterPane]]
 */
case class BinaryFeature(devName:String,toDisplay:String,displayCmd:String,other : String,otherCmd:String) extends ToggleButton with EditableFeature {
  override def getVal: String = status
  override def setVal(v:String): Unit = {if(status!=v)this.doClick()}

  text = toDisplay
  private var status = toDisplay
  reactions += {
    case ButtonClicked(_) =>
      switchStatus { case `toDisplay` => userUpdate(cmdMsg = otherCmd); status = other; case _ => userUpdate(cmdMsg = displayCmd);status = toDisplay}
  }
  /** takes a (String => Unit), applies it to status and returns new status value
   */
  val switchStatus: (String => Unit) => String = (supplier: String => Unit) => {
    supplier(status)
    status
  }

  /** sends update requested by user via GUI to [[Coordinator]]
   * @param cmdMsg [[Msg]] update type
   * @param newValue new feature value.
   * @return [[Future]] representing when the connected device confirms the update
   *
   * Overriding [[EditableFeature]] update since being a two value feature, no setter component is needed
   * and updates are generated whenever a click is performed on this button.
   */
  override def userUpdate(devName:String = devName,cmdMsg :String, newValue : String = switchStatus{case `toDisplay` => status = other case _ => status = toDisplay}): Future[Unit] ={
    val p = Promise[Unit]
    Coordinator.sendUpdate(devName,cmdMsg).onComplete {
      case Success(_) => text = status; p.success(()=>Unit)
      case _ => Dialog.showMessage(title = "Update error",message = "Something went wrong while updating a device",messageType= Message.Error)
    }
    p.future
  }
}

/** Factory for [[DeviceFeature]] instances.
 *
 */
object Feature{
  /**
   *
   * @param devName see [[DeviceFeature]]
   * @param title see [[DeviceFeature]]
   * @param text see [[DeviceFeature]]
   * @param setterComponent see [[DeviceFeature]]
   * @param updateType see [[DeviceFeature]]
   * @tparam A see [[DeviceFeature]]
   * @return new DeviceFeature instance
   */
  def apply[A<: Component { def setVal(v:String): Unit; def getVal:String }](devName:String,title:String,text:String,setterComponent:A,updateType:String): DeviceFeature[A] = DeviceFeature(devName, title, text, setterComponent, updateType)

}

/**
 * Factory for [[GUIDevice]] instances.
 *
 * Given a device, returns its graphical representation
 */
object PrintDevicePane {
  /**
   *
   * @param device device to represent
   * @return GUIDevice representing device param
   *
   */
  def apply(device: Device) : GUIDevice = device.deviceType  match{
    case AirConditionerType => AirConditionerPane(AirConditioner(device.name,device.room))
    case DehumidifierType => DehumidifierPane(Dehumidifier(device.name,device.room))
    case DishWasherType => DishWasherPane(DishWasher(device.name,device.room))
    case LightType => LightPane(Light(device.name,device.room))
    case OvenType => OvenPane(Oven(device.name,device.room))
    case ShutterType => ShutterPane(Shutter(device.name,device.room))
    case StereoSystemType => StereoPane(StereoSystem(device.name,device.room))
    case TvType => TVPane(TV(device.name,device.room))
    case WashingMachineType => WashingMachinePane(WashingMachine(device.name,device.room))
    case BoilerType => BoilerPane(Boiler(device.name,device.room))

    /*Sensors
      The else branch is used only by tests: when the application starts sensors copies are replaced with
      real sensors for simulation purposes
     */
    case ThermometerType =>
      device match {
        case thermometer: SimulatedThermometer =>
          ThermometerPane(thermometer)
        case _ =>
          ThermometerPane(Thermometer(device.name, device.room))
      }
    case HygrometerType =>
      device match {
        case hygrometer: SimulatedHygrometer =>
          HygrometerPane(hygrometer)
        case _ =>
          HygrometerPane(Hygrometer(device.name, device.room))
      }
    case MotionSensorType =>
      device match {
        case sensor: SimulatedMotionSensor =>
          MotionSensorPane(sensor)
        case _ =>
          MotionSensorPane(MotionSensor(device.name, device.room))
      }
    case PhotometerType =>
      device match {
        case photometer: SimulatedPhotometer =>
          PhotometerPane(photometer)
        case _ =>
          PhotometerPane(Photometer(device.name, device.room))
      }
    case _ => this.errUnexpected(UnexpectedDeviceType, device.deviceType.toString)
  }
}

/** SENSORS' PANES*/
private case class HygrometerPane(override val device: SimulatedHygrometer)extends GUIDevice(device){
  require (device.deviceType == HygrometerType)
  private val MAX = 100
  private val MIN = 0
  /**Calculating initial avg humidity*/
  addFeatures(("Humidity:",new DeviceFeature(device.name,"Humidity",device.DEFAULT_VALUE toString,new SliderSetter(MIN,MAX){
    GUI.humAvgUpdate(device.id,device.DEFAULT_VALUE)},updateType = null){
    override def userUpdate(devName : String, cmdMsg :String, newValue:String): Future[Unit] ={
      device.valueChanged(newValue toDouble)
      GUI.humAvgUpdate(device.id,newValue toDouble)
      Promise[Unit].success(() => Unit).future
    }
  }))
}

private case class MotionSensorPane(override val device: SimulatedMotionSensor)extends GUIDevice(device){
  require (device.deviceType == MotionSensorType)
  private val EMPTY = "EMPTY"
  private val NOT_EMPTY = "NOT EMPTY"
  private var status = EMPTY
  addFeatures(("isEmpty:",new BinaryFeature(device.name,"Empty",Msg.motionDetected,"NOT EMPTY",Msg.motionDetected){
    override def userUpdate(devName : String, cmdMsg :String, newValue:String): Future[Unit] ={
      status match {
        case EMPTY =>
          device.valueChanged(currentVal = true)
          Promise[Unit].success(() => Unit).future
          status = NOT_EMPTY;
        case _ =>
          device.valueChanged(currentVal = false)
          Promise[Unit].success(() => Unit).future
          status = EMPTY}
      text = status
      Promise[Unit].success(() => Unit).future
    }
  }))
  override def updateDevice(cmdString: String, newVal: String): Unit = {}
}
private case class PhotometerPane(override val device: SimulatedPhotometer)extends GUIDevice(device){
  require (device.deviceType == PhotometerType)
  private val MAX = 100
  private val MIN = 0
  addFeatures(("Luminosity:",new DeviceFeature(device.name,"Luminosity",device.DEFAULT_VALUE toString,new SliderSetter(MIN,MAX){
    GUI.humAvgUpdate(device.id,device.DEFAULT_VALUE)},updateType = null){
    override def userUpdate(devName : String, cmdMsg :String, newValue:String): Future[Unit] ={
      device.valueChanged(newValue toDouble)
      GUI.humAvgUpdate(device.id,newValue toDouble)
      Promise[Unit].success(() => Unit).future
    }
  }))
  override def updateDevice(cmdString: String, newVal: String): Unit = {}
}
private case class ThermometerPane(override val device: SimulatedThermometer) extends GUIDevice(device){
  require (device.deviceType == ThermometerType)
  private val MAX = 50
  private val MIN = -20
  /**Calculating initial avg temp*/
  GUI.tempAvgUpdate(device.id,device.DEFAULT_VALUE)
  addFeatures(("Temperature:",new DeviceFeature(device.name,"Temperature",device.DEFAULT_VALUE toString,new SliderSetter(MIN,MAX){
    GUI.tempAvgUpdate(device.id,device.DEFAULT_VALUE)},updateType = null){
    override def userUpdate(devName : String, cmdMsg :String, newValue:String): Future[Unit] ={
      device.valueChanged(newValue toDouble)
      GUI.tempAvgUpdate(device.id,newValue toDouble)
      Promise[Unit].success(() => Unit).future
    }
  }))
  override def updateDevice(cmdString: String, newVal: String): Unit = {}
}

/** DEVICES' PANE*/
private case class AirConditionerPane(override val device: SimulatedAirConditioner) extends GUIDevice(device){
  require (device.deviceType == AirConditionerType)
  private val temp = DeviceFeature(device.name,"Temperature",device.value toString,SliderSetter(device.minValue,device.maxValue),Msg.setTemperature)
  addFeatures(("Temperature:",temp))
  override def updateDevice(cmdString: String,newVal:String): Unit = {
    super.updateDevice(cmdString,newVal)
    cmdString match {
      case Msg.setTemperature => device.setValue(newVal toInt); temp.setVal(newVal);
      case _ => this.errUnexpected(UnexpectedMessage, "This device can only receive temperature updates")
    }
  }
}
private case class BoilerPane(override val device: SimulatedBoiler) extends GUIDevice(device){
  require (device.deviceType == BoilerType)
  private val waterTemp = DeviceFeature(device.name,"Water temperature",device.value toString,SliderSetter(device.minValue,device.maxValue),Msg.setHumidity)
  addFeatures(("Water temperature:",waterTemp))
  override def updateDevice(cmdString: String,newVal:String): Unit = {
    super.updateDevice(cmdString, newVal)
    cmdString match {
      case Msg.setTemperature => device.setValue(newVal toInt); waterTemp.setVal(newVal)
      case _ => this.errUnexpected(UnexpectedMessage, "This device can only receive temperature updates")
    }
  }
}
private case class DehumidifierPane(override val device: SimulatedDehumidifier) extends GUIDevice(device){
  require (device.deviceType == DehumidifierType)
  private val humidity =  DeviceFeature(device.name,"Humidity",device.value toString,SliderSetter(device.minValue,device.maxValue),Msg.setHumidity)
  addFeatures(("Humidity %:",humidity))
  override def updateDevice(cmdString: String,newVal:String): Unit = {
    super.updateDevice(cmdString, newVal)
    cmdString match {
      case Msg.setHumidity => device.setValue(newVal toInt); humidity.setVal(newVal)
      case _ => this.errUnexpected(UnexpectedMessage, "This device can only receive humidity updates")
    }
  }
}
private case class DishWasherPane(override val device: SimulatedDishWasher) extends GUIDevice(device){
  require (device.deviceType == DishWasherType)
  private val washProgram =  DeviceFeature(device.name,"Washing program",device.getWashingProgram toString,ListSetter(Seq(DishWasherProgram.DIRTY,DishWasherProgram.FAST,DishWasherProgram.FRAGILE)map(_ toString)),Msg.setProgram)
  private val extras =  DeviceFeature(device.name,"Extra","Extra",ListSetter(Seq(DishWasherExtra.SuperDirty,DishWasherExtra.SuperHygiene,DishWasherExtra.SuperSteam)map(_ toString)),Msg.addExtra)
  addFeatures(("Washing program:",washProgram),("Extras: ",extras))
  override def updateDevice(cmdString: String,newVal:String): Unit = {
      super.updateDevice(cmdString, newVal)
      cmdString match {
        case Msg.washingType => device.setWashingProgram(DishWasherProgram(newVal)); washProgram.setVal(newVal)
        case Msg.addExtra => device.addExtra(DishWasherExtra(newVal)); extras.setVal(newVal)
        case _ => this.errUnexpected(UnexpectedMessage, "This device can only receive washing type or extra updates")
      }
  }
}
private case class LightPane(override val device: SimulatedLight) extends GUIDevice(device) {
  require(device.deviceType == LightType)
  private val intensity =  DeviceFeature(device.name,"Intensity",device.value toString,SliderSetter(device.minValue,device.maxValue),Msg.setIntensity)
  addFeatures(("Intensity:",intensity))
  override def updateDevice(cmdString: String,newVal:String): Unit = {
    super.updateDevice(cmdString, newVal)
    cmdString match {
      case Msg.setIntensity => device.setValue(newVal toInt); intensity.setVal(newVal)
      case _ => this.errUnexpected(UnexpectedMessage, "This device can only receive intensity updates")
    }
  }
}
private case class OvenPane(override val device: SimulatedOven) extends GUIDevice(device){
  require (device.deviceType == OvenType)
  private val ovenTemp =  DeviceFeature(device.name,"Oven temperature",device.value toString, SliderSetter(device.minValue,device.maxValue),Msg.setTemperature)
  private val ovenMode = DeviceFeature(device.name,"Oven mode",device.getOvenMode toString, ListSetter(Seq(OvenMode.CONVENTIONAL,OvenMode.DEFROSTING,OvenMode.GRILL,OvenMode.LOWER,
    OvenMode.UPPER,OvenMode.VENTILATED)map(_ toString)),Msg.setMode)
  addFeatures(("Oven temperature:",ovenTemp),("Oven mode: ",ovenMode))
  override def updateDevice(cmdString: String,newVal:String): Unit = {
    super.updateDevice(cmdString, newVal)
    cmdString match {
      case Msg.setTemperature => device.setValue(newVal toInt); ovenTemp.setVal(newVal)
      case Msg.setMode => device.setOvenMode(OvenMode(newVal)); ovenMode.setVal(newVal)
      case _ => this.errUnexpected(UnexpectedMessage, "This device can only receive temperature or mode updates")
    }
  }
}
private case class ShutterPane(override val device: SimulatedShutter) extends GUIDevice(device){
  private val mode = BinaryFeature(device.name,"CLOSED",Msg.close,"OPEN",Msg.open)
  require (device.deviceType == ShutterType)
  addFeatures(("Shutter status:",mode))
  override def updateDevice(cmdString: String,newVal:String): Unit = {
    super.updateDevice(cmdString, newVal)
    cmdString match {
      case Msg.open => device.open(); mode.setVal("OPEN")
      case Msg.close => device.close(); mode.setVal("CLOSED")
      case _ => this.errUnexpected(UnexpectedMessage, "This device can only receive close or open updates")
    }
  }
}
private case class StereoPane(override val device: SimulatedStereoSystem) extends GUIDevice(device){
  private val volume = DeviceFeature(device.name,"Volume",device.value toString,SliderSetter(device.minValue,device.maxValue),Msg.setVolume)
  private val muted = BinaryFeature(device.name,"NOT MUTED",Msg.mute,"MUTED",Msg.mute)
  addFeatures(("Volume:",volume),("Stereo status:",muted))
  override def updateDevice(cmdString: String,newVal:String): Unit = {
    super.updateDevice(cmdString, newVal)
    cmdString match {
      case Msg.setVolume =>
        device.setValue(newVal toInt)
        volume.setVal(newVal)
        if (newVal.toInt == 0)
          muted.setVal("MUTED")
        else if (muted.getVal == "MUTED") muted.setVal("NOT MUTED")
      case Msg.mute => device.setValue(device.minValue); muted.setVal("MUTED")
      case _ => this.errUnexpected(UnexpectedMessage, "This device can only receive volume updates")
    }
  }
}
private case class TVPane(override val device: SimulatedTV) extends GUIDevice(device){
  require (device.deviceType == TvType)
  private val volume = DeviceFeature(device.name,"Volume",device.value toString,SliderSetter(device.minValue,device.maxValue),Msg.setVolume)
  private val muted = BinaryFeature(device.name,"NOT MUTED",Msg.mute,"MUTED",Msg.mute)
  addFeatures(("Volume:",volume),("TV status:",muted))
  override def updateDevice(cmdString: String,newVal:String): Unit = {
    super.updateDevice(cmdString,newVal)
    cmdString match{
    case Msg.setVolume => device.setValue(newVal toInt); volume.setVal(newVal)
    case Msg.mute => device.setValue(device.minValue); muted.setVal("MUTED")
    case _ =>this.errUnexpected(UnexpectedMessage,"This device can only receive volume updates")
    }
  }
}
private case class WashingMachinePane(override val device: SimulatedWashingMachine) extends GUIDevice(device){
  private val workMode = DeviceFeature(device.name,"Working mode",device.getWashingType toString,ListSetter(Seq(WashingType.RAPID,WashingType.MIX,WashingType.WOOL)map(_ toString)),Msg.washingType)
  private val extras = DeviceFeature(device.name,"Extras","Extra",ListSetter(Seq(WashingMachineExtra.SpecialColors,WashingMachineExtra.SuperDirty,WashingMachineExtra.SuperDry)map(_ toString)),Msg.addExtra)
  private val rpm =  DeviceFeature(device.name,"RMP",device.getRPM toString,ListSetter(Seq(RPM.FAST,RPM.MEDIUM,RPM.SLOW)map(_ toString)),Msg.RPM)

  require (device.deviceType == WashingMachineType)
  addFeatures(("Working mode:",workMode),("Extras:",extras),("RPM:",rpm))
  override def updateDevice(cmdString: String,newVal:String): Unit = {
    super.updateDevice(cmdString,newVal)
    cmdString match {
      case Msg.setMode => device.setWashingType(WashingType(newVal)); workMode.setVal(newVal)
      case Msg.addExtra => device.addExtra(WashingMachineExtra(newVal)); extras.setVal(newVal)
      case Msg.RPM => device.setRPM(RPM(newVal)); rpm.setVal(newVal)
      case _ => this.errUnexpected(UnexpectedMessage, "This device can only receive close or open updates")
    }
  }
}

/** Slider .
 *
 * @param mini min slider value
 * @param maxi max slider value
 */
case class SliderSetter(mini : Int, maxi: Int) extends Slider {
  min = mini
  max = maxi
   def getVal :String = value toString
   def setVal(v:String): Unit = value = v toInt
}

/** ComboBox able to communicate with coordinator.
 *
 * @param items to be displayed in [[ComboBox]]
 */
case class ListSetter(items: Seq[String]) extends ComboBox(items){
   def getVal : String = selection.item
   def setVal(v: String): Unit = selection.item = v
}


/** Login page */
object LoginPage{

  val idText : TextField = new TextField(Constants.LoginTextSize)
  val pswText : PasswordField = new PasswordField(Constants.LoginTextSize)
  new Frame(){
    override def closeOperation(): Unit = {
      super.closeOperation()
      Application.closeApplication()
    }

    title = "Login to HOME!"
    contents = new BoxPanel(Orientation.Vertical) {
      contents ++= Seq(
        new FlowPanel() {
          contents ++= Seq(
            new Label("Username:"),
            idText,
          )
        },
        new FlowPanel() {
          contents ++= Seq(
            new Label("Password:"),
            pswText,
          )},
        new FlowPanel() {
          contents ++= Seq(
            new Button("Login"){
              reactions +={
                case ButtonClicked(_) =>
                  if(UserHandler.login(idText.text.trim, pswText.password.mkString("").trim)) {close(); GUI.top;}
                  else Dialog.showMessage(title="Login failed",message = "Wrong credentials!",messageType = Dialog.Message.Error)
              }
            },
            new Button("Register"){
              reactions +={
                case ButtonClicked(_) => if (UserHandler.register(idText.text.trim,pswText.password.mkString("").trim))
                {Dialog.showMessage(title="Registration completed!",message = "You can now login with your new credentials",messageType = Dialog.Message.Info)}
                else
                {Dialog.showMessage(title="Registration failed",message = "Couldn't complete registration",messageType = Dialog.Message.Error)}

              }
            },
            new Button("Cancel") {
              reactions += {
                case ButtonClicked(_) => close(); Application.closeApplication();
              }
            })
        }
      )
    }
    this.peer.setLocationRelativeTo(null)
    this.visible = true
  }
}