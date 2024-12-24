package day24

import scala.collection.mutable.*
import scala.io.Source


object A:
  def main(args: Array[String]): Unit =
    val m = Map[String, Long]()

    val adj = Map[String, Set[String]]()
    val reqs = Map[String, Set[String]]()
    val ops = Map[String, (String, String, String)]()

    Source.stdin.getLines.foreach:
      case s"$v: $b" =>
        m(v) = b.toLong
      case s"$a $op $b -> $c" =>
        adj.getOrElseUpdate(a, Set())
        adj.getOrElseUpdate(b, Set())
        adj.getOrElseUpdate(c, Set())
        adj(a) += c
        adj(b) += c

        reqs.getOrElseUpdate(c, Set())
        reqs(c) += a
        reqs(c) += b
        ops(c) = (a, op, b)
      case _ =>

    while reqs.nonEmpty do
      for
        v <- m.keys
        if adj.contains(v) && adj(v).nonEmpty
      do
        for w <- adj(v) do
          if reqs.contains(w) then
            reqs(w).remove(v)
            if reqs(w).isEmpty then
              val (a, op, b) = ops(w)
              op match
                case "AND" => m(w) = m(a) & m(b)
                case "OR" => m(w) = m(a) | m(b)
                case "XOR" => m(w) = m(a) ^ m(b)
              reqs.remove(w)

    val res = m.keys.filter(_.startsWith("z")).foldLeft(0l):
      case (r, s"z$b") => r | (m("z" + b) << b.toInt)
    println(res)
