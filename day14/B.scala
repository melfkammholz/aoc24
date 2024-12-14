package day14

import scala.io.Source

extension (x: Int)
  infix def %%(m: Int) = (x % m + m) % m

object B:
  def main(args: Array[String]): Unit =
    val (w, h): (Int, Int) = (101, 103)

    val rs = Source.stdin.getLines.toArray.map:
      case s"p=$px,$py v=$vx,$vy" =>
        (px.toInt, py.toInt, vx.toInt, vy.toInt)

    val res = (0 until w * h).minBy: t =>
      var (cx, cy) = (0.0, 0.0)
      for (x, y, dx, dy) <- rs do
        cx += (x + t * dx) %% w
        cy += (y + t * dy) %% h
      cx /= rs.size
      cy /= rs.size

      var (vx, vy) = (0.0, 0.0)
      for (x, y, dx, dy) <- rs do
        val (tx, ty) = ((x + t * dx) %% w, (y + t * dy) %% h)
        vx += (cx - tx) * (cx - tx)
        vy += (cy - ty) * (cy - ty)

      vx + vy

    println(res)  // 8168

