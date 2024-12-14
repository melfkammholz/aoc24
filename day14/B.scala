package day14

import scala.math.{pow,sqrt}
import scala.io.Source

object B:
  def main(args: Array[String]): Unit =
    val (w, h): (Int, Int) = (101, 103)

    val rs = Source.stdin.getLines.toArray.map:
      case s"p=$px,$py v=$vx,$vy" =>
        (px.toInt, py.toInt, vx.toInt, vy.toInt)

    var u = Double.MaxValue
    var res = 0
    (0 until w * h).map
    for t <- 0 until w * h do
      var (cx, cy) = (0.0, 0.0)
      for (x, y, dx, dy) <- rs do
        cx += ((x + t * dx) % w + w) % w
        cy += ((y + t * dy) % h + h) % h
      cx /= rs.size
      cy /= rs.size

      var (vx, vy) = (0.0, 0.0)
      for (x, y, dx, dy) <- rs do
        val tx = ((x + t * dx) % w + w) % w
        val ty = ((y + t * dy) % h + h) % h
        vx += (cx - tx) * (cx - tx)
        vy += (cy - ty) * (cy - ty)
      vx /= rs.size
      vy /= rs.size

      if vx + vy < u then
        u = vx + vy
        res = t

    println(res)  // 8168

