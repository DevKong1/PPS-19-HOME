package HOME

import HOME.MyClass._

import scala.language.implicitConversions

object constants {
  def default_profile_name: String = "DEFAULT"
}

case class MyClass(_class: Any) {
  def getSimpleClassName: String = _class.getClass.getSimpleName.split("\\$").last

  def errUnexpected[A](item: Unexpected, value: String): A =
    throw new IllegalArgumentException("Unexpected " + item.item + ": " + value)
}

object MyClass{
  implicit def toMyClass(_class: Any): MyClass = MyClass(_class)
}

//helper object used by various devices to set the output strength
object ValueChecker {
  def apply(min: Int, max: Int)(value: Int): Int = value match {
    case x if x > max => max
    case x if x < min => min
    case _ => value
  }
}

sealed trait Unexpected {
  var item: String
}
case object UnexpectedTopic extends Unexpected {
  override var item: String = "topic"
}
case object UnexpectedMessage extends Unexpected {
  override var item: String = "message"
}
case object UnexpectedRoom extends Unexpected {
  override var item: String = "room"
}
case object UnexpectedDevice extends Unexpected {
  override var item: String = "device"
}
case object UnexpectedDeviceType extends Unexpected {
  override var item: String = "deviceType"
}
case object UnexpectedResult extends Unexpected {
  override var item: String = "result"
}
case object UnexpectedProfile extends Unexpected {
  override var item: String = "profile"
}

trait WashingType
object WashingType {

  case object MIX extends WashingType
  case object WOOL extends WashingType
  case object RAPID extends WashingType

  def apply(washingType: String): WashingType = washingType match{
    case "MIX" => MIX
    case "WOOL" => WOOL
    case "RAPID" => RAPID
    case _ => this.errUnexpected(UnexpectedMessage, washingType)
  }
}

trait RPM
object RPM {

  case object SLOW extends RPM
  case object MEDIUM extends RPM
  case object FAST extends RPM

  def apply(rpm: String): RPM = rpm match{
    case "SLOW" => SLOW
    case "MEDIUM" => MEDIUM
    case "FAST" => FAST
    case _ => this.errUnexpected(UnexpectedMessage, rpm)
  }
}

trait GenericExtra

trait WashingMachineExtra extends GenericExtra
object WashingMachineExtra {
  case object SuperDry extends WashingMachineExtra
  case object SuperDirty extends WashingMachineExtra
  case object SpecialColors extends WashingMachineExtra

  def apply(extra: String): WashingMachineExtra = extra match{
    case "SuperDry" => SuperDry
    case "SuperDirty" => SuperDirty
    case "SpecialColors" => SpecialColors
    case _ => this.errUnexpected(UnexpectedMessage, extra)
  }
}

trait DishWasherProgram
object DishWasherProgram {

  case object FAST extends DishWasherProgram
  case object DIRTY extends DishWasherProgram
  case object FRAGILE extends DishWasherProgram

  def apply(dishWasherProgram: String): DishWasherProgram = dishWasherProgram match{
    case "FAST" => FAST
    case "DIRTY" => DIRTY
    case "FRAGILE" => FRAGILE
    case _ => this.errUnexpected(UnexpectedMessage, dishWasherProgram)
  }
}

trait DishWasherExtra extends GenericExtra
object DishWasherExtra {

  case object SuperSteam extends DishWasherExtra
  case object SuperDirty extends DishWasherExtra
  case object SuperHygiene extends DishWasherExtra

  def apply(dishWasherExtra: String): DishWasherExtra = dishWasherExtra match{
    case "SuperSteam" => SuperSteam
    case "SuperDirty" => SuperDirty
    case "SuperHygiene" => SuperHygiene
    case _ => this.errUnexpected(UnexpectedMessage, dishWasherExtra)
  }
}

trait OvenMode
object OvenMode {

  case object CONVENTIONAL extends OvenMode
  case object UPPER extends OvenMode
  case object LOWER extends OvenMode
  case object VENTILATED extends OvenMode
  case object GRILL extends OvenMode
  case object DEFROSTING extends OvenMode

  def apply(ovenMode: String): OvenMode = ovenMode match{
    case "CONVENTIONAL" => CONVENTIONAL
    case "UPPER" => UPPER
    case "LOWER" => LOWER
    case "VENTILATED" => VENTILATED
    case "GRILL" => GRILL
    case "DEFROSTING" => DEFROSTING
    case _ => this.errUnexpected(UnexpectedMessage, ovenMode)
  }
}
//TODO add a checkAndRemove pimping the Iterable