package day12

import scala.collection.mutable.*
import scala.math.{log10, pow}
import scala.io.Source

def main(args: Array[String]): Unit = {
  val g = Source.stdin.getLines.map(_.split("")).toArray
  val (h, w) = (g.size, g.head.size)

  val dirs = List((-1, 0), (0, 1), (1, 0), (0, -1))
  val seen = Array.fill(h, w)(false)

  lazy val dfs: (Int, Int) => (Int, Int) = (y, x) =>
    if seen(y)(x) then
      (0, 0)
    else
      seen(y)(x) = true
      var (a, p) = (1, 0)
      for (dy, dx) <- dirs do
        val ny = y + dy
        val nx = x + dx
        if (0 until h).contains(ny) && (0 until w).contains(nx) then
          if g(y)(x) == g(ny)(nx) then
            val (na, np) = dfs(ny, nx)
            a += na
            p += np
          else
            p += 1
        else
          p += 1
      (a, p)

  var res = 0
  for
    y <- 0 until h
    x <- 0 until w
  do
    val (a, p) = dfs(y, x)
    res += a * p
  println(res)
}

