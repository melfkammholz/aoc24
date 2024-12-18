package day18

import scala.collection.mutable.*
import scala.io.Source

object B:
  def main(args: Array[String]): Unit =
    val (h, w) = (71, 71)
    var dirs = List((-1, 0), (0, 1), (1, 0), (0, -1))

    val ps = Source.stdin.getLines.toList.map:
      case s"$x,$y" => (y.toInt, x.toInt)

    val solve: Int => Boolean = n =>
      val bad = Map[Int, Set[Int]]().withDefault(_ => Set())
      ps.take(n).foreach: (y, x) =>
        bad(y) = bad(y) + x

      val seen = Array.fill(h, w)(false)
      seen(0)(0) = true
      val q = Queue[(Int, Int)]()
      q.enqueue((0, 0))
      while q.nonEmpty && !seen(h - 1)(w - 1) do
        val (y, x) = q.dequeue
        for
          (dy, dx) <- dirs
          (ny, nx) = (y + dy, x + dx)
          if (0 until h).contains(ny) && (0 until w).contains(nx)
          if !bad(ny).contains(nx)
          if !seen(ny)(nx)
        do
          seen(ny)(nx) = true
          q.enqueue((ny, nx))
      seen(h - 1)(w - 1)

    var (l, r) = (1024, ps.size)
    while l < r do
      val m = l + (r - l) / 2
      if solve(m) then
        l = m + 1
      else
        r = m
    println(ps(r - 1).swap.toArray.mkString(","))
