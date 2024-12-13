package day13

import scala.io.Source

object A:
  def main(args: Array[String]): Unit =
    val ls = Source.stdin.getLines.toList

    var res = 0l
    ls.grouped(4).foreach:
      case s"Button A: X+$x1, Y+$y1"
        :: s"Button B: X+$x2, Y+$y2"
        :: s"Prize: X=$x, Y=$y"
        :: _ =>

        val (a1, b1, a2, b2, a, b) = (x1.toLong, y1.toLong, x2.toLong, y2.toLong, x.toLong, y.toLong)

        val d = a1 * b2 - a2 * b1
        val u = a * b2 - a2 * b
        val v = a1 * b - a * b1

        res += (if u % d == 0 && v % d == 0 then 3 * (u / d) + v / d else 0)
      case _ =>

    println(res)  // 40369

