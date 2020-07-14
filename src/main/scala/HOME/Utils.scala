package HOME

case  class  MyClass(_class: Any) {
  def getSimpleClassName: String = _class.getClass.getSimpleName.split("\\$").last
}

object MyClass{
  implicit def toMyClass(_class: Any): MyClass = MyClass(_class)
}
