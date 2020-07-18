package HOME

import scala.language.implicitConversions

object Constants {
  val testSleepTime :Int = 50 //millis, time the matcher waits to evaluate 'eventually' expression in tests
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

////////////////////////////////
/// Specific Device Messages ///
////////////////////////////////

import HOME.MyClass._

trait SpecificDeviceMsg {
  def deviceType: DeviceType
  def command: String
  def value: String
}

object SpecificDeviceMsg {

  class SpecificDeviceMsgWithValue(override val deviceType: DeviceType,override val command: String,override val value: String) extends SpecificDeviceMsg
  class SpecificDeviceMsgWithoutValue(override val deviceType: DeviceType,override val command: String,override val value: String = "") extends SpecificDeviceMsg

  def apply(msg: String): SpecificDeviceMsg = msg.split('_').length match {
    case 2 => new SpecificDeviceMsgWithoutValue(DeviceType(msg.split('_')(0)), msg.split('_')(1))
    case 3 => new SpecificDeviceMsgWithValue(DeviceType(msg.split('_')(0)), msg.split('_')(1), msg.split('_')(2))
    case _ => this.errUnexpected(UnexpectedMessage, msg)
  }
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


trait Extra

object Extra {
  case object SuperDry extends Extra
  case object SuperDirty extends Extra
  case object SpecialColors extends Extra

  def apply(extra: String): Extra = extra match{
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
//TODO add a checkAndRemove pimping the Iterable