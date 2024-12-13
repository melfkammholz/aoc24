package day12

import scala.collection.mutable.*
import scala.io.Source

object B {
  def main(args: Array[String]): Unit = {
    val g = Source.stdin.getLines.map(_.split("")).toArray
    val (h, w) = (g.size, g.head.size)
    val inb: (Int, Int) => Boolean = (y, x) =>
      (0 until h).contains(y) && (0 until w).contains(x)

    val dirs = List((-1, 0), (0, 1), (1, 0), (0, -1))
    val seen = Array.fill(h, w)(false)

    lazy val side: (Int, Int, Int, Int, Set[(Int, Int, Int, Int)]) => Unit = (y, x, dy, dx, s) =>
      val (ny, nx) = (y + dy, x + dx)
      val (my, mx) = (y - dx, x + dy)

      val ok = inb(ny, nx) && g(y)(x) == g(ny)(nx)
            && (!inb(my, mx) || g(ny)(nx) != g(my)(mx))

      if ok then
        side(ny, nx, dy, dx, s)
      else
        s.add((y, x, dy, dx))

    lazy val dfs: (Int, Int, Set[(Int, Int, Int, Int)]) => Int = (y, x, s) =>
      if seen(y)(x) then
        0
      else
        seen(y)(x) = true
        var a = 1
        for (dy, dx) <- dirs do
          val (ny, nx) = (y + dy, x + dx)
          if inb(ny, nx) && g(y)(x) == g(ny)(nx) then
            a += dfs(ny, nx, s)
          else
            side(y, x, dx, -dy, s)
        a

    var res = 0
    for y <- 0 until h; x <- 0 until w do
      val s = Set[(Int, Int, Int, Int)]()
      res += dfs(y, x, s) * s.size

    println(res)  // 830516
  }
}

