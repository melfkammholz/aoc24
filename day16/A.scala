package day16

import scala.collection.mutable.*
import scala.io.Source
import scala.math.Ordering.Implicits._

object A:
  def main(args: Array[String]): Unit =
    val g = Source.stdin.getLines.toArray
    val (h, w) = (g.size, g.head.size)

    val sy = g.indexWhere(_.contains('S'))
    val sx = g(sy).indexWhere(_.equals('S'))
    val ey = g.indexWhere(_.contains('E'))
    val ex = g(ey).indexWhere(_.equals('E'))

    val dist = Array.fill(h, w)(Int.MaxValue)
    dist(sy)(sx) = 0

    val q = PriorityQueue[(Int, Int, Int, Int, Int)]()
    q.enqueue((0, sy, sx, 0, 1))
    while q.nonEmpty do
      val (p, y, x, dy, dx) = q.dequeue
      if p <= dist(y)(x) then
        for
          (nd, ny, nx, ndy, ndx) <- (dist(y)(x) + 1, y + dy, x + dx, dy, dx)
                                 :: (dist(y)(x) + 1001, y - dx, x + dy, -dx, dy)
                                 :: (dist(y)(x) + 1001, y + dx, x - dy, dx, -dy)
                                 :: Nil
          if g(ny)(nx) != '#'
          if nd < dist(ny)(nx)
        do
          dist(ny)(nx) = nd
          q.enqueue((-nd, ny, nx, ndy, ndx))

    println(dist(ey)(ex))  // 93436

