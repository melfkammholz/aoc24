package day06

import scala.collection.mutable.Set
import scala.io.Source

object B {
  def main(args: Array[String]): Unit = {
    val g = Source.stdin.getLines.map(_.toCharArray()).toArray()
    val m = g.length
    val n = g.head.length

    val sy = g.indexWhere(_.contains('^'))
    val sx = g(sy).indexOf('^')

    val inBounds: (Int, Int) => Boolean =
      g.indices.contains(_) && g.head.indices.contains(_)

    var res = 0
    for
      oy <- g.indices
      ox <- g(oy).indices
      if g(oy)(ox) == '.'
    do
      g(oy)(ox) = '#'

      var (y, x) = (sy, sx)
      var (dy, dx) = (-1, 0)

      var loop = false
      val seen = Set[(Int, Int, Int, Int)]()
      while inBounds(y, x) && !loop do
        // ouch
        while g(y)(x) == '#' do
          y -= dy; x -= dx
          val tmp = -dy; dy = dx; dx = tmp
          y += dy; x += dx

        loop = seen.contains((y, x, dy, dx))
        if !loop then
          seen.add((y, x, dy, dx))
          y += dy; x += dx

      res += (if loop then 1 else 0)
      g(oy)(ox) = '.'

    println(res)  // 1972
  }
}

