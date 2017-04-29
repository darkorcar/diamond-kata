
object DiamondPrinter {

  val SEPARATOR: Char = ' '

  def singleLine(c: Char, level: Int): String = {

    def accumulateCharacters(char: Char, x: List[Char]): String = char match {
      case 'A' =>
        if(level == 0) {
          (x.reverse ::: List('A') ::: x).mkString
        }
        else {
         (x.reverse ::: List(SEPARATOR) ::: x).mkString
        }
      case cc =>
        if(cc - 'A' == level) {
          accumulateCharacters((cc - 1).toChar, cc::x)
        } else {
          accumulateCharacters((cc - 1).toChar, SEPARATOR::x)
        }
    }

    accumulateCharacters(c, List())
  }

  def printDiamond(c: Char): List[String] = {

    def accumulateLines(lines: List[String], level: Int): List[String] = level match {
      case 0 =>
        singleLine(c, 0)::lines
      case x =>
        accumulateLines(singleLine(c, x)::lines, x - 1)
    }

    val diamondDepth = c - 65

    val firstHalf: List[String] = accumulateLines(List(), diamondDepth)

    val secondHalf = firstHalf.reverse.tail

    firstHalf:::secondHalf
  }


  def main(args: Array[String]): Unit = {
    DiamondPrinter.printDiamond('D')
      .foreach( println(_) )
  }

}
