package day12

import scala.collection.mutable.*
import scala.io.Source

object A {
  def main(args: Array[String]): Unit = {
    val g = Source.stdin.getLines.map(_.split("")).toArray
    val (h, w) = (g.size, g.head.size)
    val inb: (Int, Int) => Boolean = (y, x) =>
      (0 until h).contains(y) && (0 until w).contains(x)

    val dirs = List((-1, 0), (0, 1), (1, 0), (0, -1))
    val seen = Array.fill(h, w)(false)

    lazy val dfs: (Int, Int) => (Int, Int) = (y, x) =>
      if seen(y)(x) then
        (0, 0)
      else
        seen(y)(x) = true
        var (a, p) = (1, 0)
        for (dy, dx) <- dirs do
          val (ny, nx) = (y + dy, x + dx)
          if inb(ny, nx) && g(y)(x) == g(ny)(nx) then
            val (na, np) = dfs(ny, nx)
            a += na
            p += np
          else
            p += 1
        (a, p)

    var res = 0
    for y <- 0 until h; x <- 0 until w do
      val (a, p) = dfs(y, x)
      res += a * p

    println(res)  // 1361494
  }
}

