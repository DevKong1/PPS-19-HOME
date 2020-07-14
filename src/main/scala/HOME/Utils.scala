package HOME

case  class  MyClass(_class: Any) {
  def getSimpleClassName: String = _class.getClass.getSimpleName.split("\\$").last
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