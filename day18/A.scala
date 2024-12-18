package day18

import scala.collection.mutable.*
import scala.io.Source

object A:
  def main(args: Array[String]): Unit =
    val (h, w) = (71, 71)
    var dirs = List((-1, 0), (0, 1), (1, 0), (0, -1))

    var sp = 1024
    val bad = Map[Int, Set[Int]]().withDefault(_ => Set())
    val ps = Source.stdin.getLines.foreach:
      case s"$x,$y" =>
        if sp > 0 then
          bad(y.toInt) = bad(y.toInt) + x.toInt
          sp -= 1

    val d = Array.fill(h, w)(Int.MaxValue)
    d(0)(0) = 0
    val q = Queue[(Int, Int)]()
    q.enqueue((0, 0))
    while q.nonEmpty && d(h - 1)(w - 1) == Int.MaxValue do
      val (y, x) = q.dequeue
      for
        (dy, dx) <- dirs
        (ny, nx) = (y + dy, x + dx)
        if (0 until h).contains(ny) && (0 until w).contains(nx)
        if !bad(ny).contains(nx)
        if d(ny)(nx) == Int.MaxValue
      do
        d(ny)(nx) = d(y)(x) + 1
        q.enqueue((ny, nx))

    println(d(h - 1)(w - 1))  // 340

