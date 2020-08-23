package HOME

import java.awt.{Dimension, GraphicsEnvironment}
import java.io.FileWriter
import java.text.SimpleDateFormat
import java.util.Calendar
import io.github.nremond.SecureHash

import scala.io.Source
import scala.language.postfixOps
import scala.swing.{ComboBox, Dialog, Swing}
import scala.util.Try

/** Static object used to get size of GUI components
 *
 */
object WindowSize {
  import WindowSizeType._
  private val SCREEN = GraphicsEnvironment.getLocalGraphicsEnvironment.getMaximumWindowBounds
  private val FRAME_PROP = 0.55
  private val WIN_PROP_H = 0.2
  private val WIN_PROP_W = 0.5

  val height: Int = SCREEN.height * FRAME_PROP toInt
  val width: Int = SCREEN.width * FRAME_PROP toInt

  /**
   *
   * @param windType type of window
   * @return a size based on windType
   */
  def apply(windType : WindowSizeType.Value): Dimension = windType match{
    case MainW => new Dimension(width, height)
    case DialogInput => new Dimension(width*WIN_PROP_W toInt,height*WIN_PROP_H toInt)
    case AddProfile => new Dimension(width*0.4 toInt, height*0.2 toInt)
  }
}

/** Enumeration used to bind every Component to its size
 *
 */
object WindowSizeType extends Enumeration {
  type Type = Value
  val MainW, DialogInput, AddProfile = Value
}

object MapDeviceCommands {
  private var commands: Set[String] = Set.empty

  def apply(dev: Device): Unit = dev.deviceType match {
    case LightType => commands = Set.empty
      commands += "setIntensity"
    case AirConditionerType | BoilerType => commands = Set.empty
      commands += "setTemperature"
    case DehumidifierType => commands = Set.empty
      commands += "setHumidity"
    case ShutterType => commands = Set.empty
      commands += "close"
    case TvType | StereoSystemType => commands = Set.empty
      commands += "setVolume"
      commands += "mute"
    case WashingMachineType => commands = Set.empty
      commands += "washingType"
      commands += "RPM"
      commands += "addExtra"
      //commands += "removeExtra"
    case DishWasherType => commands = Set.empty
      commands += "setProgram"
      commands += "addExtra"
      //commands += "removeExtra"
    case OvenType => commands = Set.empty
      commands += "setTemperature"
      commands += "setMode"
    case _ =>
  }

  def getCommands : Set[String] = {
    commands ++= Set("off")
    commands
  }
}

object DateTime {

  def getDate : String = {
    val cal = Calendar.getInstance()
    val date = cal.get(Calendar.DATE )
    val month = cal.get(Calendar.MONTH ) + 1
    val year = cal.get(Calendar.YEAR )

    date+"/"+month+"/"+year
  }

  def getCurrentTime : String = {
    val today = Calendar.getInstance().getTime

    // create the date/time formatters
    val minuteFormat = new SimpleDateFormat("mm")
    val hourFormat = new SimpleDateFormat("hh")
    val amPmFormat = new SimpleDateFormat("a")

    val currentHour = hourFormat.format(today)
    val currentMinute = minuteFormat.format(today)
    val amOrPm = amPmFormat.format(today)

    currentHour+":"+currentMinute+" "+ amOrPm
  }
}

/** Handles user registration and login
 *
 * Works with a single login file where a whitespace is the separator between id and password
 * es: "username password".
 * Creates a single password file in "User" directory.
 * Works with pbkdf2 as hash algorithm.
 * */
object UserHandler{
  private val SPLIT = " "

  /** Registers a user with a valid id
   *
   * @param id to register, must be unique.
   * @param psw to register
   * @return registration completed/failed
   *
   */
  def register(id:String,psw:String): Boolean = {
    if (!CheckNonNull(id,psw)) return false
    ResourceOpener.open(Source.fromFile(Constants.LoginPath)) { file => {
      for (entry <- file.getLines) {
        val fileId = entry.get_id
        if (fileId equals id) return false
      }
    }}
    ResourceOpener.open(new FileWriter(Constants.LoginPath,true)) { writer => {
      writer.write(id+SPLIT+SecureHash.createHash(psw)+"\n")
    }}
    true
  }

  /** Check for credential match in password file.
   *
   * @param id to login
   * @param psw to login
   * @return whether login is successfull or not
   */
  def login(id:String,psw:String) : Boolean = {
    if (!CheckNonNull(id,psw)) return false
    ResourceOpener.open(Source.fromFile(Constants.LoginPath)) { file => {
      for (entry <- file.getLines()){
        val fileId = entry.get_id
        val filePsw = entry.get_psw
        if(SecureHash.validatePassword(psw,filePsw) && fileId == id) {
          return true
        }
      }
    }}
    false
  }

  /** Gets id and password from a "login valid" string, es: "username-password"
   *
   * @param s string cointaining id and psw
   */
  implicit class LoginHelper(s: String) {
    private val ID = 0
    private val PSW = 1
    def get_id: String = s.split(SPLIT)(ID)
    def get_psw:String = s.split(SPLIT)(PSW)
  }
}

/** Checks that a variadic number of [[String]] parameters are not null
 *
 */
object CheckNonNull {
  /**
   *
   * @param value params to check
   * @return whether all value are not null
   */
  def apply(value: String*): Boolean = value.map(_.trim.length>0).reduce(_ && _)
}

case class StringComboBox(items: Seq[String]) extends ComboBox[String](items: Seq[String])

object AlertMessage {

  def alertIsCorrectName(x: String): Boolean = x match {
    case "" => Dialog.showMessage(null, "Insert a valid name", "Error", Dialog.Message.Error, Swing.EmptyIcon)
      false
    case _ => Profile.getProfileNames.contains(x) match {
      case true => Dialog.showMessage(null, "This name already exist", "Error", Dialog.Message.Error, Swing.EmptyIcon)
        false
      case _ => true
    }
  }

  def alertIsCorrectValue(value: String): Boolean = value match {
    case "" => Dialog.showMessage(null, "Insert a value", "Error", Dialog.Message.Error, Swing.EmptyIcon)
      false
    case _ => isDouble(value) match {
      case false => Dialog.showMessage(null, "Insert a correct numeric value", "Error", Dialog.Message.Error, Swing.EmptyIcon)
        false
      case _ => true
    }
  }

  def isDouble(x: String): Boolean = Try(x.toDouble).isSuccess
}