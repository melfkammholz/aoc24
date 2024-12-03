import scala.io.Source

@main
def main(): Unit = {
  val mulPat = ".*?mul\\(([0-9]+),([0-9]+)\\)(.*)".r
  var inp = Source.stdin.getLines().mkString

  var res = 0
  var done = false
  while !done do
    inp match {
      case mulPat(a, b, r) =>
        res += a.toInt * b.toInt
        inp = r
      case _ => done = true
    }

  println(res)  // 159892596
}

