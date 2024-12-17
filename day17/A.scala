package day17

import scala.collection.mutable.*
import scala.io.Source

object A:
  def main(args: Array[String]): Unit =
    val regs = Map[Char, Int]()
    val rslv = Map[Int, Function0[Int]](
      0 -> (() => 0),
      1 -> (() => 1),
      2 -> (() => 2),
      3 -> (() => 3),
      4 -> (() => regs('A')),
      5 -> (() => regs('B')),
      6 -> (() => regs('C')),
      7 -> (() => throw RuntimeException("invalid program"))
    )
    var prog = Array[Int]()

    Source.stdin.getLines.foreach:
      case s"Register $c: $x" =>
        regs(c(0)) = x.toInt
      case s"Program: $p" =>
        prog = p.split(",").map(_.toInt).toArray
      case _ =>

    var res = Array[Int]()
    var pc = 0
    while pc + 1 < prog.size do
      println(regs)
      (prog(pc), prog(pc + 1)) match
        case (0, c) => regs('A') /= (1 << rslv(c)())
        case (1, l) => regs('B') ^= l
        case (2, c) => regs('B') = rslv(c)() % 8
        case (3, _) if regs('A') == 0 =>
        case (3, l) => pc = l - 2
        case (4, _) => regs('B') ^= regs('C')
        case (5, c) => res = res :+ rslv(c)() % 8
        case (6, c) => regs('B') = regs('A') / (1 << rslv(c)())
        case (7, c) => regs('C') = regs('A') / (1 << rslv(c)())
      pc += 2

    println(res.mkString(","))
