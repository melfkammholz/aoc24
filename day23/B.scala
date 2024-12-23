package day23

import scala.collection.mutable.*
import scala.io.Source


object B:
  def main(args: Array[String]): Unit =
    val adj = Map[String, Set[String]]()
    Source.stdin.getLines.foreach: line =>
      val e = line.split("-")
      adj.getOrElseUpdate(e(0), Set())
      adj.getOrElseUpdate(e(1), Set())
      adj(e(0)) += e(1)
      adj(e(1)) += e(0)

    val seen = Set[(Set[String], Set[String])]()
    lazy val solve: (Set[String], Set[String]) => Unit = (c, ws) =>
      if !seen.contains((c, ws)) && !ws.isEmpty then
        for w <- ws do
          solve(c + w, c.map(adj).foldLeft(adj(w))(_ intersect _))
        seen.add((c, ws))

    adj.keys.foreach(v => solve(Set(v), adj(v)))
    println(seen.maxBy(_._1.size)._1.toBuffer.sorted.mkString(","))  // ab,al,cq,cr,da,db,dr,fw,ly,mn,od,py,uh

