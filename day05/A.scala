package day05

import scala.collection.mutable.{ListBuffer, Map}
import scala.io.Source

object A {
  def main(args: Array[String]): Unit = {
    var res = 0
    val adj = Map[Int, ListBuffer[Int]]().withDefault(_ => ListBuffer())
    Source.stdin.getLines.foreach {
      case s"$a|$b" =>
        adj(a.toInt) = adj(a.toInt) += b.toInt
      case "" =>
      case upd =>
        val ps = upd.split(",").map(_.toInt)
        val ok =
          ps.indices.forall(i =>
            (i + 1 until ps.length).forall(j => adj(ps(i)).contains(ps(j))))
        res += (if ok then ps(ps.length / 2) else 0)
    }
    println(res)  // 4609
  }
}

