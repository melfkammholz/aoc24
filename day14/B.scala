// Original solution can be found at
// https://github.com/melfkammholz/aoc24/blob/main/day14/C.scala
//
// This solution is less efficient but it uses stuff I wanted use in Scala.

package day14

import scala.io.Source

import cats.effect.unsafe.implicits._
import cats.effect._
import fs2.Stream
import smile.clustering.dbscan


// extension (x: Int)
//   infix def %%(m: Int) = (x % m + m) % m

object B extends IOApp.Simple:
  def run: IO[Unit] =
    val (w, h) = (101, 103)

    val rs = Source.stdin.getLines.toArray.map:
      case s"p=$px,$py v=$vx,$vy" =>
        (px.toInt, py.toInt, vx.toInt, vy.toInt)

    val maxCluster: Int => Int = t =>
      val data = rs.map: (x, y, dx, dy) =>
        Array((x + t * dx) %% w, (y + t * dy) %% h).map(_.toDouble)
      val cs = dbscan(data, 1, 1.0d)
      if cs.k > 0 then cs.size.init.max else 0

    val n = Runtime.getRuntime.availableProcessors
    val res = Stream.range(0, w * h)
      .chunkN(w * h / n)
      .parEvalMapUnordered(n)(chunk => IO {
        chunk.map(t => (maxCluster(t), t))
      })
      .unchunks
      .reduce((a, b) => if a._1 < b._1 then b else a)
      .map(_._2)
      .compile
      .last

    res.flatMap(m => IO.println(m.get))  // 8168

