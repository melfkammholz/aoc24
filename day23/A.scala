package day23

import scala.collection.mutable.*
import scala.io.Source


object A:
  def main(args: Array[String]): Unit =
    val adj = Map[String, Set[String]]()
    Source.stdin.getLines.foreach: line =>
      val e = line.split("-")
      adj.getOrElseUpdate(e(0), Set())
      adj.getOrElseUpdate(e(1), Set())
      adj(e(0)) += e(1)
      adj(e(1)) += e(0)

    val vs = adj.keys.toBuffer
    val res = {
      for
        i <- 0 until vs.size - 2
        j <- i + 1 until vs.size - 1
        k <- j + 1 until vs.size
        if adj(vs(i)).contains(vs(j))
          && adj(vs(j)).contains(vs(k))
          && adj(vs(k)).contains(vs(i))
        if vs(i).startsWith("t")
          || vs(j).startsWith("t")
          || vs(k).startsWith("t")
      yield 1
    }.sum

    println(res)  // 1344

