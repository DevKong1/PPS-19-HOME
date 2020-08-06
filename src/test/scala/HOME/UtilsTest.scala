package HOME

import org.scalatest.funsuite.AnyFunSuite

class UtilsTest extends AnyFunSuite {
    import MyClass._

    Rooms.addRoom("Living room")
    val aaa: SimulatedLight = Light("AAA","Living room")

    test("The implicit conversion works as intended") {
        assert(aaa.deviceType.getSimpleClassName == "LightType")
    }
}
