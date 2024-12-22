package day21

import scala.collection.mutable.*
import scala.io.Source


object A:
  def main(args: Array[String]): Unit =
    val n = 2

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

    val dirs = List((-1, 0, '^'), (0, 1, '>'), (1, 0, 'v'), (0, -1, '<'))
    val paths = Map[Char, Map[Char, String]]()

    // initialize graph
    for
      pad <- List(numpad, dirpad)
      (h, w) = (pad.size, pad.head.size)
      y <- 0 until h
      x <- 0 until w
      if pad(y)(x) != ' '
      (dy, dx, c) <- dirs
      (ny, nx) = (y + dy, x + dx)
      if (0 until h).contains(ny) && (0 until w).contains(nx)
      if pad(ny)(nx) != ' '
    do
      if !paths.contains(pad(y)(x)) then
        paths(pad(y)(x)) = Map()
      paths(pad(y)(x))(pad(ny)(nx)) = c.toString

    for pad <- List(numpad, dirpad) do
      val keys = pad.flatten.filter(_ != ' ')

      // self loops
      for key <- keys do
        paths(key)(key) = ""

      // Bellman-Ford
      for
        _ <- 0 until (pad.size + pad.head.size)
        from <- keys
        to <- keys
        via <- keys
      do
        val ma = paths(from).get(via)
        val mb = paths(via).get(to)
        val mc = paths(from).get(to)

        (ma, mb) match
          case (Some(a), Some(b)) =>
            if a.length + b.length < mc.map(_.length).getOrElse(Int.MaxValue) then
              paths(from)(to) = a + b
          case _ =>


    val dp = Map[Char, Map[Char, Array[Int]]]()
    val keys = dirpad.flatten.filter(_ != ' ')
    for from <- keys do
      dp(from) = Map()
      for to <- keys do
        dp(from)(to) = Array.fill(n)(0)
        dp(from)(to)(0) = paths(from)(to).length + 1

    for d <- 1 until n do
      for from <- keys do
        for to <- keys do
          val ins = "A" + paths(from)(to) + "A"
          dp(from)(to)(d) = ins.zip(ins.tail).map(dp(_)(_)(d - 1)).sum


    var res = 0l
    Source.stdin.getLines.foreach: code =>
      val ins = "A" + ("A" + code).zip(code).map(paths(_)(_) + "A").mkString
      var sc = ins.zip(ins.tail).map(dp(_)(_)(n - 1)).sum
      println((sc, code.init.toInt))
      res += sc * code.init.toInt

    println(res)

