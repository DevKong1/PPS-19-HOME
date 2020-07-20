package HOME


import java.awt.{Dimension, GraphicsEnvironment}

import scala.collection.mutable.ListBuffer
import scala.swing._
import scala.swing.event.{ButtonClicked, Event, SelectionChanged}

sealed trait Room {
  def devices : Set[Device]
  def name : String
}

class GUIDevice() extends BorderPanel{
  ???
}
object newDevice {

}

//TODO: RECONSIDER HOW TO MODEL GUIROOM
 class GUIRoom(override val name:String) extends BoxPanel(Orientation.Vertical) with Room {
  override def devices: Set[Device] = ???
  //private val x : Dimension = WindowSize()

  val buttonDimension = new Dimension(300,70)
  var tryListDevice = new ListBuffer[Device]()
  if(name != "+") {
    var a: Device = Light("Lamp1", name)
    var b: Device = Light("Lamp2", name)
    tryListDevice += a
    tryListDevice += b
  }

  /*val button1 = new Button("Add device") {
    addDevice()
  }*/

  _contents += new BoxPanel(Orientation.Vertical) {
    for( c <- tryListDevice) {
      contents += addButton(c.name)
    }
    contents += new BorderPanel {
      add(addButton("Add device"), BorderPanel.Position.South)
    }
  }

  def addDevice(): Unit = {
    import Dialog._
    val name = showInput(null,
      "Room name:",
      "Add room",
      Message.Plain,
      Swing.EmptyIcon,
      Nil, "")
  }

  override def equals(that: Any): Boolean = that match{
    case that: GUIRoom=> this.name equals that.name
    case _ => false
  }

  def apply(roomName: String): GUIRoom =  new GUIRoom(roomName)
    object deviceAddForm {

    }

  def addButton(buttonName : String) : Button = buttonName match {
    case buttonName => new Button(buttonName) {
      minimumSize = buttonDimension
      maximumSize = buttonDimension
      preferredSize = buttonDimension
      name = buttonName

      reactions += {
        if(name == "Add device") {
          case ButtonClicked(b) =>
            addDevice()
            println("Click me: " + b.name)
        } else {
          case ButtonClicked(b) =>
            println("Ciao")
        }
      }

    }
    case _ => null
  }
}

object GUI extends SimpleSwingApplication {
  private val ADD = "+"
  var rooms: Set[GUIRoom] = Set(new GUIRoom("Home"), new GUIRoom("Kitchen"), new GUIRoom("Bedroom"),new GUIRoom(ADD))

  val tp: TabbedPane = new TabbedPane {
    for(i <- rooms) yield {pages += new TabbedPane.Page(i.name,i)}
  }
    def top: MainFrame = new MainFrame {
      title = "Home!"
      println("Welcome")
      reactions += {
        case SelectionChanged(_) =>
          for {
            isLast <- checkLast()
            name <-  getRoomName
          } yield {
            tp.pages.remove(tp.pages.length-1)
            rooms+= new GUIRoom(name)
            tp.pages+=new TabbedPane.Page(name, new GUIRoom(name))
            tp.pages+=new TabbedPane.Page(ADD,new GUIRoom(ADD))
            //TODO: Merge with controller controller.addRoom(name)
          }
      }
      //used to set items in the main window inside a vertical BoxPanel
      contents = new BoxPanel(Orientation.Vertical) {
        contents ++= Seq(tp)

      }
      listenTo(tp.selection)
      size = WindowSize()

      object checkLast {
        def apply(): Option[Boolean] = {
          tp.selection.page.title match {
            case "+" => Some(true)
            case _ => None
          }
        }
      }

      private def getRoomName: Option[String] = {
        import Dialog._
        val name = showInput(tp,
          "Room name:",
          "Add room",
          Message.Plain,
          Swing.EmptyIcon,
          Nil, "")
        println(name.isDefined)
        //TODO: THINK OF A MORE FUNCTIONAL WAY TO IMPLEMENT INPUT CHECK
        if (name.isDefined && name.get.trim.length>0 && !name.get.equals(ADD) && !tp.pages.contains(new TabbedPane.Page(name.get, new GUIRoom(name.get)))) name else {
          if(name.isDefined) {
            showMessage(tp, "Room already existing or incorrect room name", Message.Error toString)
          }
          None
        }

      }
    }
  }
  object WindowSize {
    private val SCREEN = GraphicsEnvironment.getLocalGraphicsEnvironment.getMaximumWindowBounds
    private val FRAME_PROP = 0.55

    val height: Int = SCREEN.height * FRAME_PROP toInt
    val width: Int = SCREEN.width * FRAME_PROP toInt

    def apply(): Dimension = {
      new Dimension(width, height)
    }

  }


