package day08

import scala.collection.mutable.{Map, Set}
import scala.io.Source

object A {
  def main(args: Array[String]): Unit = {
    val g = Source.stdin.getLines.map(_.toCharArray).toArray

    var as = Map[Char, List[(Int, Int)]]().withDefault(_ => Nil)
    for
      y <- g.indices
      x <- g(y).indices
      if g(y)(x) != '.'
    do
      as(g(y)(x)) = (y, x) :: as(g(y)(x))

    var res = Set[(Int, Int)]()
    for
      y <- g.indices
      x <- g(y).indices
      sas <- as.values
      (ay, ax) <- sas
      if ax != x || ay != y
    do
      if sas.contains((y + 2 * (ay - y), x + 2 * (ax - x))) then
        res.add((y, x))

    println(res.size)  // 301
  }
}

