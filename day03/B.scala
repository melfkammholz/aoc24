package day03

import scala.io.Source

import cats.parse.{Parser => P, Numbers}

enum Instruction:
  case Do
  case Dont
  case Mul(x: Int, y: Int)

object B {
  def main(args: Array[String]): Unit = {
    var inp = Source.stdin.getLines().mkString

    val mulP =
      for
        _ <- P.string("mul(")
        x <- Numbers.digits.map(_.toInt)
        _ <- P.char(',')
        y <- Numbers.digits.map(_.toInt)
        _ <- P.char(')')
      yield Instruction.Mul(x, y)
    val doP = P.string("do()").as(Instruction.Do)
    val dontP = P.string("don't()").as(Instruction.Dont)
    val insP = mulP | doP | dontP
    val nextInsP = P.recursive[Option[Instruction]] { recurse =>
      insP.backtrack.map(Some(_)) | P.anyChar *> (recurse | P.pure(None))
    }

    val insL = nextInsP.rep0.parse(inp).map(_._2.flatten).getOrElse(Nil)

    var res = 0
    var use = true
    insL.foreach {
      case Instruction.Do => use = true
      case Instruction.Dont => use = false
      case Instruction.Mul(x, y) if use => res += x * y
      case _ =>
    }
    println(res)
  }
}

