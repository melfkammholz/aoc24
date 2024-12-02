import scala.io.Source

@main
def main(): Unit = {
  var res = 0

  for (line <- Source.stdin.getLines) {
    val rep = line.split(" ").map(_.toInt)

    val inRange: ((Int, Int)) => Boolean = t =>
      val (a, b) = t
      (1 to 3).contains(b - a)

    val ok = (-1 until rep.length).exists { i =>
      val drep = rep.take(i) ++ rep.drop(i + 1)
      val zdrep = drep.zip(drep.tail)
      val okInc = zdrep.forall(inRange)
      val okDec = zdrep.forall(t => inRange(t.swap))
      okInc || okDec
    }
    res += { if ok then 1 else 0 }
  }

  println(res)  // 337
}
