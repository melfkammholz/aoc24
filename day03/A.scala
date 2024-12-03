package day03

import scala.io.Source

object A {
  def main(args: Array[String]): Unit = {
    var inp = Source.stdin.getLines().mkString

    val isNum: String => Boolean = _.forall(Character.isDigit)
    var res = 0
    while !inp.isEmpty do
      inp match {
        case s"mul($x,$y)$r" if isNum(x) && isNum(y) =>
          res += x.toInt * y.toInt
          inp = r
        case _ => inp = inp.tail
      }

    println(res)  // 159892596
  }
}

