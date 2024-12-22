package day21

import scala.collection.mutable.*
import scala.io.Source


object A:
  def main(args: Array[String]): Unit =
    val n = 25

    val numpad = Array(
      Array('7', '8', '9'),
      Array('4', '5', '6'),
      Array('1', '2', '3'),
      Array(' ', '0', 'A')
    )

    val dirpad = Array(
      Array(' ', '^', 'A'),
      Array('<', 'v', '>')
    )

    val paths = Map[Char, Map[Char, String]]()
    for
      pad <- List(numpad, dirpad)
      c <- pad.flatten
    do
      paths.getOrElseUpdate(c, Map())

    for pad <- List(numpad, dirpad) do
      val pos = (c: Char) =>
        val y = pad.indexWhere(_.contains(c))
        val x = pad(y).indexWhere(_ == c)
        (y, x)

      val keys = pad.flatten.filter(_ != ' ')
      val (by, bx) = pos(' ')
      for from <- keys; to <- keys do
        val (fy, fx) = pos(from)
        val (ty, tx) = pos(to)
        val (dy, dx) = (ty - fy, tx - fx)

        val y = "^" * (-dy) + "v" * dy
        val x = "<" * (-dx) + ">" * dx
        if ((dx > 0 || (by - fy, bx - fx) == (0, dx)) && (by - fy, bx - fx) != (dy, 0)) then
          paths(from)(to) = y + x + "A"
        else
          paths(from)(to) = x + y + "A"


    val dp = Map[Char, Map[Char, Array[Long]]]()
    val keys = dirpad.flatten.filter(_ != ' ')
    for from <- keys do
      dp(from) = Map()
      for to <- keys do
        dp(from)(to) = Array.fill(n)(0)
        dp(from)(to)(0) = paths(from)(to).length

    for d <- 1 until n do
      for from <- keys do
        for to <- keys do
          val ins = "A" + paths(from)(to)
          dp(from)(to)(d) = ins.zip(ins.tail).map(dp(_)(_)(d - 1)).sum


    var res = 0l
    Source.stdin.getLines.foreach: code =>
      val ins = "A" + ("A" + code).zip(code).map(paths(_)(_)).mkString
      var sco = ins.zip(ins.tail).map(dp(_)(_)(n - 1)).sum
      res += sco * code.init.toInt

    println(res)  // 263617786809000

