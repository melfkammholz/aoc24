import scala.collection.mutable.{ListBuffer, Map}
import scala.io.Source

@main
def main(): Unit = {
  val xs = ListBuffer[Int]()
  val ys = ListBuffer[Int]()

  val inpf = "([0-9]+)[ ]*([0-9]+)".r
  for (line <- Source.stdin.getLines) {
    line match {
      case inpf(x, y) =>
        xs += x.toInt
        ys += y.toInt
    }
  }

  val cnt = Map().withDefault(_ => 0)
  for (y <- ys) {
    cnt(y) += 1
  }

  var res = 0
  for (x <- xs) {
    res += x * cnt(x)
  }
  println(res)  // 26800609
}
