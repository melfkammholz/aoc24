package day11

import scala.collection.mutable.*
import scala.math.{log10, pow}
import scala.io.Source

def main(args: Array[String]): Unit = {
  val xs = Source.stdin.getLines.next.split(" ").map(_.toLong)
  val n = 75

  val dp = Array.fill(n)(Map.empty[Long, Long])
  lazy val solve: (Long, Int) => Long =
    case (_, s) if s == n => 1
    case (0, s) => solve(1, s + 1)
    case (x, s) =>
      lazy val d = log10(x).longValue + 1
      if !dp(s).contains(x) then
        if d % 2 == 0 then
          val p = pow(10, d / 2).longValue
          dp(s)(x) = solve(x / p, s + 1) + solve(x % p, s + 1)
        else
          dp(s)(x) = solve(2024 * x, s + 1)
      dp(s)(x)

  var res = 0l
  for x <- xs do
    res += solve(x, 0)
  println(res)  // 225253278506288
}

