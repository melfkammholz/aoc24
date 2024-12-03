package day03

import scala.io.Source

object C {
  def main(args: Array[String]): Unit = {
    var inp = Source.stdin.getLines().mkString

    val isNum: String => Boolean = _.forall(Character.isDigit)
    var res = 0
    var use = true
    while !inp.isEmpty do
      inp match {
        case s"do()$r" =>
          use = true
          inp = r
        case s"don't()$r" =>
          use = false
          inp = r
        case s"mul($x,$y)$r" if isNum(x) && isNum(y) =>
          if use then
            res += x.toInt * y.toInt
          inp = r
        case _ => inp = inp.tail
      }

    println(res)  // 92626942
  }
}

