package day04

import scala.io.Source

object B {
  def main(args: Array[String]): Unit = {
    val inp = Source.stdin.getLines().toArray

    val m = inp.length
    val n = inp.head.length
    val w1 = Array("M.S", ".A.", "M.S")
    val w2 = Array("M.M", ".A.", "S.S")
    val w3 = Array("S.M", ".A.", "S.M")
    val w4 = Array("S.S", ".A.", "M.M")

    var res = 0
    for
      y <- 0 until m - 2
      x <- 0 until n - 2
      w <- List(w1, w2, w3, w4)
    do
      val ok = (0 until 3).forall { dy =>
        (0 until 3).forall { dx =>
          w(dy)(dx) == '.' || w(dy)(dx) == inp(y + dy)(x + dx)
        }
      }
      res += (if ok then 1 else 0)

    println(res)  // 2507
  }
}

