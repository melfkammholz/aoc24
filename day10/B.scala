package day10

import scala.io.Source

object B {
  def main(args: Array[String]): Unit = {
    val g = Source.stdin.getLines.map(_.split("").map(_.toInt)).toArray
    val (m, n) = (g.length, g.head.length)

    val dirs = List((-1, 0), (0, 1), (1, 0), (0, -1))

    var res = 0
    lazy val dfs: (Int, Int) => Unit = (y, x) =>
      if g(y)(x) == 9 then
        res += 1
      else
        for
          (dy, dx) <- dirs
          if (0 until m).contains(y + dy) && (0 until n).contains(x + dx)
          if g(y)(x) + 1 == g(y + dy)(x + dx)
        do
          dfs(y + dy, x + dx)

    for
      y <- 0 until m
      x <- 0 until n
      if g(y)(x) == 0
    do
      dfs(y, x)
    println(res)  // 1541
  }
}

