package day20

import scala.collection.mutable.*
import scala.io.Source


object B:
  def main(args: Array[String]): Unit =
    val g = Source.stdin.getLines.map(_.toCharArray).toArray
    val (h, w) = (g.size, g.head.size)

    val sy = g.indexWhere(_.contains('S'))
    val sx = g(sy).indexOf('S')
    val ey = g.indexWhere(_.contains('E'))
    val ex = g(ey).indexOf('E')

    val dirs = List((-1, 0), (0, 1), (1, 0), (0, -1))
    val dist = Array.fill(h, w)(Int.MaxValue)
    dist(sy)(sx) = 0

    val nextDir = (y: Int, x: Int) => (p: (Int, Int)) =>
      val (dy, dx) = p
      g(y + dy)(x + dx) != '#' && dist(y + dy)(x + dx) == Int.MaxValue

    var (y, x) = (sy, sx)
    while ((y, x) != (ey, ex)) do
      dirs.find(nextDir(y, x)) match
        case Some((dy, dx)) =>
          dist(y + dy)(x + dx) = dist(y)(x) + 1
          y += dy
          x += dx
        case None =>

    var res = 0
    for
      y <- 0 until h
      x <- 0 until w
      if g(y)(x) != '#'
      dy <- -20 to 20
      dx <- -20 + dy.abs to 20 - dy.abs
      if (0 until h).contains(y + dy) && (0 until w).contains(x + dx)
      if g(y + dy)(x + dx) != '#'
      dt = dist(y + dy)(x + dx) - dist(y)(x) - dy.abs - dx.abs
      if dt >= 100
    do
      res += 1

    println(res)  // 1020507

