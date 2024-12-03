package day03

import scala.io.Source

object C {
  def main(args: Array[String]): Unit = {
    val mulPat = "mul\\(([0-9]+),([0-9]+)\\)(.*)".r
    var inp = Source.stdin.getLines().mkString

    var res = 0
    var use = true
    var done = false
    while !done do
      inp match {
        case s"do()$r" =>
          use = true
          inp = r
        case s"don't()$r" =>
          use = false
          inp = r
        case mulPat(y, z, r) =>
          if use then
            res += y.toInt * z.toInt
          inp = r
        case _ if !inp.isEmpty() => inp = inp.tail
        case _ => done = true
      }

    println(res)  // 92626942
  }
}

