package day07

import scala.io.Source
import scala.math.pow

object B {
  def main(args: Array[String]): Unit = {
    var res = 0l

    lazy val gen: List[Long] => List[Long] = {
      case Nil => throw IllegalArgumentException()
      case x :: Nil => x :: Nil
      case x :: xs =>
        for
          y <- gen(xs)
          z <- x + y :: x * y :: y * pow(10l, x.toString.length).longValue + x :: Nil
        yield z
    }

    Source.stdin.getLines.toArray.foreach {
      case s"$a: $xs" =>
        val b = a.toLong
        val ys = xs.split(" ").map(_.toLong).toList
        res += (if gen(ys.reverse).contains(b) then b else 0)
    }

    println(res)  // 426214131924213
  }
}

