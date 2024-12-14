package day14

import scala.io.Source

extension (x: Int)
  infix def %%(m: Int) = (x % m + m) % m

object A:
  def main(args: Array[String]): Unit =
    val (w, h): (Int, Int) = (101, 103)
    val t = 100

    var cnt = Array.fill(4)(0)
    Source.stdin.getLines.foreach:
      case s"p=$px,$py v=$vx,$vy" =>
        val (x, y, dx, dy) = (px.toInt, py.toInt, vx.toInt, vy.toInt)
        val (tx, ty) = ((x + t * dx) %% w, (y + t * dy) %% h)
        if tx != w / 2 && ty != h / 2 then
          val bx = ((tx - w / 2).sign + 1) / 2
          val by = ((ty - h / 2).sign + 1) / 2
          cnt((bx << 1) | by) += 1

    println(cnt.product)  // 226236192

