package day05

import scala.collection.mutable.{ListBuffer, Map}
import scala.io.Source

object B {
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
        res += (if ok then 0 else ps.sortWith(adj(_).contains(_))(ps.length / 2))
    }
    println(res)  // 5723
  }
}

