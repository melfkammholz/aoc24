package day08

import scala.collection.mutable.{Map, Set}
import scala.io.Source

object B {
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
      val ok = sas.exists { (by, bx) =>
        if ay == by && ax == bx then
          false
        else
          val col = (ax - x) * (by - y) - (bx - x) * (ay - y) == 0
          val dist = (bx - x) % (bx - ax) == 0 && (by - y) % (by - ay) == 0
          col && dist
      }
      if ok then res.add((y, x))

    println(res.size)  // 1019
  }
}

