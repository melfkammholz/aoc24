package day04

import scala.io.Source

object A {
  def main(args: Array[String]): Unit = {
    val inp = Source.stdin.getLines().toArray

    val m = inp.length
    val n = inp.head.length
    val dirs = Array((-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1))
    val w = "XMAS"

    var res = 0
    for
      sy <- 0 until m
      sx <- 0 until n
      (dy, dx) <- dirs
    do
      val ok = (0 until w.length).forall { k =>
        val y = sy + k * dy
        val x = sx + k * dx
        (0 until m).contains(y) && (0 until n).contains(x) && w(k) == inp(y)(x)
      }
      res += (if ok then 1 else 0)

    println(res)  // 159892596
  }
}

