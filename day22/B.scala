package day22

import scala.collection.mutable.*
import scala.collection.View
import scala.io.Source


object B:
  def nextPRN(x: Long) =
    val y = (x << 6 ^ x) & 0xffffff
    val z = (y >> 5 ^ y) & 0xffffff
    (z << 11 ^ z) & 0xffffff

  def main(args: Array[String]): Unit =
    val ss = Source.stdin.getLines.map(_.toLong).toArray

    val count = Map[(Long, Long, Long, Long), Int]().withDefaultValue(0)
    val occ = Array.fill(ss.size)(Map[(Long, Long, Long, Long), Option[Long]]().withDefaultValue(None))
    for j <- 0 until ss.size do
      val s = ss(j)
      val v = View.iterate(s, 2001)(nextPRN).map(_ % 10).toArray
      val w = v.tail.zip(v).map(_ - _)
      for i <- 0 until w.size - 3 do
        val x = (w(i), w(i + 1), w(i + 2), w(i + 3))
        if occ(j)(x).isEmpty then
          count(x) += 1
          occ(j)(x) = Some(v(i + 3 + 1))

    var res = 0l
    val m = count.maxBy((_, c) => c)._2
    for seq <- count.filter(_._2 == m).keys do
      res = res.max(occ.map(_(seq).getOrElse(0l)).sum)

    println(res)  // 1690

