package day03

import scala.io.Source

import cats.parse.{Parser => P, Numbers}

enum Instruction:
  case Do
  case Dont
  case Mul(x: Int, y: Int)

object C {
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
    val nextInsP = (P.anyChar.repUntil0(insP) *> insP).?

    // TODO write a proper parser
    var res = 0
    var use = true
    while !inp.isEmpty() do
      nextInsP.parse(inp) match {
        case Right((r, Some(ins))) =>
          ins match {
            case Instruction.Do => use = true
            case Instruction.Dont => use = false
            case Instruction.Mul(x, y) if use => res += x * y
            case _ =>
          }
          inp = r
        case _ =>
          inp = ""
      }
    println(res)
  }
}

