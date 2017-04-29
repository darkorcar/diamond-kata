import org.scalatest.FunSuite

class DiamondPrinterTest extends FunSuite {

  import DiamondPrinter._

  test("should return a string given char input") {

    assert(singleLine('A', 0) === "A")

  }

  test("should return a string of characters descending from given char to 'A' and back, depending on level") {

    assert(singleLine('B', 1) === "B-B")
    assert(singleLine('C', 1) === "-B-B-")
    assert(singleLine('C', 2) === "C---C")

  }

  test("should return a list of strings presenting diamond") {

    assert(printDiamond('C') === List(
      "--A--",
      "-B-B-",
      "C---C",
      "-B-B-",
      "--A--")
    )
  }
}


