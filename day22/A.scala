package day22

import scala.collection.mutable.*
import scala.collection.View
import scala.io.Source


object A:
  def nextPRN(x: Long) =
    val y = (x << 6 ^ x) & 0xffffff
    val z = (y >> 5 ^ y) & 0xffffff
    (z << 11 ^ z) & 0xffffff

  def main(args: Array[String]): Unit =
    println(Source.stdin.getLines.map(_.toLong).map(View.iterate(_, 2001)(nextPRN).last).sum)

