package HOME

import org.scalatest.funsuite.AnyFunSuite

class UtilsTest extends AnyFunSuite {
    import MyClass._
    val aaa: SimulatedLight = Light("AAA","salotto")

    test("The implicit comversion works as intended") {
        assert(aaa.deviceType.getSimpleClassName == "LightType")
    }
}
