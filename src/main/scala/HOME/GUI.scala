package HOME


import java.awt.{Dimension, GraphicsEnvironment}

import scala.collection.mutable.ListBuffer
import scala.swing
import scala.swing._
import scala.swing.event.{ButtonClicked, SelectionChanged}

//TODO: RECONSIDER HOW TO MODEL GUIROOM

case class GUIRoom(override val name:String) extends Panel with Room{
  var a : Device = new SimulatedLight("Lamp1", name, LightType, 1)
  var b : Device = new SimulatedLight("Lamp2", name, LightType, 1)
  var devices = new ListBuffer[Device]()
  devices += a
  devices += b

  for( c <- devices) {
    _contents += Button("" + c.name+""+c.room)(println("Clicked device: " + c.name))
    _contents += Swing.HStrut(20)
  }

  def equals(that: GUIRoom): Boolean = that.name match{
    case this.name => true
    case _ => false
  }
   def apply(roomName: String): GUIRoom =  GUIRoom(roomName)
}

sealed trait Room {
  def devices : ListBuffer[Device]
  def name : String
}

object SimpleGUI extends SimpleSwingApplication {

  var rooms : Set[GUIRoom] = Set(GUIRoom("Kitchen"), GUIRoom("Bedroom"))
  val tp : TabbedPane= new TabbedPane {
    for(i <- rooms) yield {pages += new TabbedPane.Page(i.name,i)}
  }
  def top: MainFrame = new MainFrame {
    title = "Home!"
    println("Welcome")
    reactions += {
      case SelectionChanged(tp) => {
        for{
          isAddRoom <- checkLast()
          name <- roomName()
        } yield {
          /*
            Controlla se il tab cliccato è l'ultimo, se si deve controllare che il nome della stanza non ci sia già
            e che non sia "+", se rispetta tutto allora aggiungo la stanza al set e aggiungo un tab
           */
        }
      }
    }
    //used to set items in the main window inside a vertical BoxPanel
    contents = new BoxPanel(Orientation.Vertical) {
      contents ++= Seq(tp)

    }
    listenTo(tp.selection)
    size =  WindowSize()

  }
  object checkLast {
    def apply(): Option[Boolean] = {
       tp.selection.page.title match{
        case "+" => Some(true)
        case _ => None
      }
    }
  }

  object roomName extends Dialog {
    private val label = new Label("Room name:")
    private val nameText = new TextField() {
      focusable = true
    }
    private val confirmBtn = new Button("Confirm") {
      reactions += {
        case ButtonClicked(_) =>{
          /*for (i <- nameText.text; j <- rooms.) yield {
          TODO: Controlla che il nome sia "sensato" e che non sia "+" oppure il nome di un'altra stanza, se è così ritorna un option[String] con il nome
          }*/
        }
      }
    }
    private val cancelBtn = new Button("Cancel")
    private val inputBox = new BoxPanel(Orientation.Horizontal) {
      contents ++= Seq(label, nameText)
    }
    private val btnBox = new BoxPanel(Orientation.Horizontal) {
      contents ++= Seq(confirmBtn, cancelBtn)
    }

    def apply(): Option[String] = {
      contents = new BoxPanel(Orientation.Vertical) {
        contents ++= Seq(inputBox, btnBox)
      }
      Some("ciao")
    }
    listenTo(confirmBtn.mouse.clicks)
  }
}

object WindowSize {
  private val SCREEN = GraphicsEnvironment.getLocalGraphicsEnvironment.getMaximumWindowBounds
  private val FRAME_PROP = 0.55

  val height : Int = SCREEN.height * FRAME_PROP toInt
  val width : Int= SCREEN.width * FRAME_PROP toInt

  def apply(): Dimension = {
    new Dimension(width ,height)
  }

}