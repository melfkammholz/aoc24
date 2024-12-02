import scala.io.Source

@main
def main(): Unit = {
  var res = 0

  for line <- Source.stdin.getLines do
    val rep = line.split(" ").map(_.toInt)

    val inRange: ((Int, Int)) => Boolean = t =>
      val (a, b) = t
      (1 to 3).contains(b - a)

    val zrep = rep.zip(rep.tail)
    val okInc = zrep.forall(inRange)
    val okDec = zrep.forall(t => inRange(t.swap))

    res += (if okInc || okDec then 1 else 0)

  println(res)  // 269
}

