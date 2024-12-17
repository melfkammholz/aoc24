package day17

import scala.collection.mutable.*
import scala.io.Source

extension [T](a: Array[T])
  infix def isSuffixOf(b: Array[T]): Boolean =
    a.size <= b.size && b.slice(b.size - a.size, b.size).sameElements(a)

object B:
  def main(args: Array[String]): Unit =
    var prog = Array[Long]()
    Source.stdin.getLines.foreach:
      case s"Register $c: $x" =>
      case s"Program: $p" =>
        prog = p.split(",").map(_.toLong).toArray
      case _ =>

    lazy val run: Long => Array[Long] = a =>
      val regs = Map[Char, Long]()
      regs('A') = a

      val rslv = Map[Long, Function0[Long]](
        0l -> (() => 0l),
        1l -> (() => 1l),
        2l -> (() => 2l),
        3l -> (() => 3l),
        4l -> (() => regs('A')),
        5l -> (() => regs('B')),
        6l -> (() => regs('C')),
        7l -> (() => throw RuntimeException("invalid program"))
      )

      var res = Array[Long]()
      var pc = 0
      while pc + 1 < prog.size do
        (prog(pc), prog(pc + 1)) match
          case (0l, c) => regs('A') /= (1 << rslv(c)())
          case (1l, l) => regs('B') ^= l
          case (2l, c) => regs('B') = rslv(c)() % 8
          case (3l, _) if regs('A') == 0 =>
          case (3l, l) => pc = l.toInt - 2
          case (4l, _) => regs('B') ^= regs('C')
          case (5l, c) => res = res :+ rslv(c)() % 8
          case (6, c) => regs('B') = regs('A') / (1 << rslv(c)())
          case (7, c) => regs('C') = regs('A') / (1 << rslv(c)())
        pc += 2
      res

    lazy val solve: Long => Option[Long] = a =>
      var res: Option[Long] = None
      var z = 0
      while z < 8 && res.isEmpty do
        val b = a << 3 | z
        val out = run(b)
        if out isSuffixOf prog then
          if prog.size == out.size then
            res = Some(b)
          else
            solve(b) match
              case Some(c) => res = Some(c)
              case None =>
        z += 1
      res

    println(solve(0l).get)  // 190615597431823

