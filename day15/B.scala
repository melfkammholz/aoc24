package day15

import scala.collection.mutable.*
import scala.io.Source

object B:
  def main(args: Array[String]): Unit =
    var c = false
    var g = ArrayBuffer[ArrayBuffer[Char]]()
    val ms = StringBuilder()

    Source.stdin.getLines.foreach:
      case "" => c = true
      case l if c => ms ++= l
      case l =>
        val r = ArrayBuffer[Char]()
        l.foreach:
          case '#' => r.append('#', '#')
          case 'O' => r.append('[', ']')
          case '.' => r.append('.', '.')
          case '@' => r.append('@', '.')
        g.append(r)

    lazy val check: (Int, Int, Int, Int) => Boolean = (y, x, dy, dx) =>
      val (ny, nx) = (y + dy, x + dx)
      if dx.abs > 0 then
        if g(ny)(nx) == '[' || g(ny)(nx) == ']' then
          check(ny, nx, dy, dx)
        else
          g(ny)(nx) == '.'
      else
        if g(ny)(nx) == '[' then
          check(ny, nx, dy, dx) && check(ny, nx + 1, dy, dx)
        else if g(ny)(nx) == ']' then
          check(ny, nx, dy, dx) && check(ny, nx - 1, dy, dx)
        else
          g(ny)(nx) == '.'

    lazy val move: (Int, Int, Int, Int) => Boolean = (y, x, dy, dx) =>
      if check(y, x, dy, dx) then
        val (ny, nx) = (y + dy, x + dx)
        if dx.abs > 0 then
          if g(ny)(nx) == '[' || g(ny)(nx) == ']' then
            move(ny, nx, dy, dx)
        else
          if g(ny)(nx) == '[' then
            move(ny, nx, dy, dx)
            move(ny, nx + 1, dy, dx)
          else if g(ny)(nx) == ']' then
            move(ny, nx, dy, dx)
            move(ny, nx - 1, dy, dx)
        g(ny)(nx) = g(y)(x)
        g(y)(x) = '.'
        true
      else
        false

    var y = g.indexWhere(_.contains('@'))
    var x = g(y).indexWhere(_.equals('@'))

    val dirs = Map('^' -> (-1, 0), '>' -> (0, 1), 'v' -> (1, 0), '<' -> (0, -1))
    ms.foreach: m =>
      val (dy, dx) = dirs(m)
      if move(y, x, dy, dx) then
        y = y + dy
        x = x + dx

    var res = 0
    for y <- g.indices; x <- g(y).indices do
      if g(y)(x) == '[' then
        res += y * 100 + x
    println(res)

