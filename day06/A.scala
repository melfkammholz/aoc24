package day06

import scala.collection.mutable.Set
import scala.io.Source

object A {
  def main(args: Array[String]): Unit = {
    val g = Source.stdin.getLines.toArray
    val m = g.length
    val n = g.head.length

    var y = g.indexWhere(_.contains('^'))
    var x = g(y).indexOf('^')
    var dy = -1
    var dx = 0

    val inBounds: (Int, Int) => Boolean =
      g.indices.contains(_) && g.head.indices.contains(_)

    val seen = Set[(Int, Int)]()
    while inBounds(y, x) do
      // ouch
      while g(y)(x) == '#' do
        y -= dy; x -= dx
        val tmp = -dy; dy = dx; dx = tmp
        y += dy; x += dx

      seen.add((y, x))

      y += dy; x += dx

    println(seen.size)  // 5208
  }
}

