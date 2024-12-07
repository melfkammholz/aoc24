package day07

import scala.collection.mutable.Set
import scala.io.Source

object A {
  def main(args: Array[String]): Unit = {
    var res = 0l

    lazy val check: (Long, Long, List[Long]) => Boolean = {
      case (r, a, Nil) => r == a
      case (r, a, x :: xs) => check(r, a + x, xs) || check(r, a * x, xs)
    }

    Source.stdin.getLines.toArray.foreach {
      case s"$a: $xs" =>
        val b = a.toLong
        val (y :: ys) = xs.split(" ").map(_.toLong).toList : @unchecked
        res += (if check(b, y, ys) then b else 0l)
    }

    println(res)  // 2664460013123
  }
}

