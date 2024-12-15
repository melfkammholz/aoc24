package day15

import scala.collection.mutable.*
import scala.io.Source

object A:
  def main(args: Array[String]): Unit =
    var c = false
    var g = ArrayBuffer[ArrayBuffer[Char]]()
    val ms = StringBuilder()

    Source.stdin.getLines.foreach:
      case "" => c = true
      case l if c => ms ++= l
      case l => g.append(ArrayBuffer.from(l))

    lazy val move: (Int, Int, Int, Int) => Boolean = (y, x, dy, dx) =>
      val (ny, nx) = (y + dy, x + dx)
      if g(ny)(nx) == 'O' then
        val ok = move(ny, nx, dy, dx)
        if ok then
          g(ny)(nx) = 'O'
        ok
      else if g(ny)(nx) == '.' then
        g(ny)(nx) = 'O'
        true
      else
        false

    var y = g.indexWhere(_.contains('@'))
    var x = g(y).indexWhere(_.equals('@'))

    val dirs = Map('^' -> (-1, 0), '>' -> (0, 1), 'v' -> (1, 0), '<' -> (0, -1))
    ms.foreach: m =>
      val (dy, dx) = dirs(m)
      if move(y, x, dy, dx) then
        g(y)(x) = '.'
        y = y + dy
        x = x + dx
        g(y)(x) = '@'

    var res = 0
    for y <- g.indices; x <- g(y).indices do
      if g(y)(x) == 'O' then
        res += y * 100 + x
    println(res)

