package day01

import scala.collection.mutable.ListBuffer
import scala.io.Source

object A {
  def main(args: Array[String]): Unit = {
    val xs = ListBuffer[Int]()
    val ys = ListBuffer[Int]()

    val inpf = "([0-9]+)[ ]*([0-9]+)".r
    for (line <- Source.stdin.getLines) {
      line match {
        case inpf(x, y) =>
          xs += x.toInt
          ys += y.toInt
      }
    }

    var res = 0
    for ((x, y) <- xs.sorted().zip(ys.sorted())) {
      res += (x - y).abs
    }
    println(res)  // 1530215
  }
}

