package day09

import scala.io.Source

object B {
  def main(args: Array[String]): Unit = {
    var dsk = Source.stdin.getLines().next.split("").map(_.toInt)
    var cpy = dsk.clone

    val frags = Array.fill(10)(List.empty[Int])
    for i <- dsk.indices.filter(_ % 2 == 0) do
      frags(dsk(i)) = i :: frags(dsk(i))

    val next: Int => Option[Int] = l =>
      (l to 1 by -1)
        .filter(frags(_).headOption.isDefined)
        .map(s => (frags(s), s))
        .maxOption
        .map(_._2)
        .map { s =>
          var (res :: tail) = frags(s) : @unchecked
          frags(s) = tail
          res
        }

    val sum: (Long, Long) => Long = (a, b) => (b - 1) * b / 2 - (a - 1) * a / 2

    var chksm = 0l
    var c = 0
    for l <- 0 until dsk.size by 2 do
      if dsk(l) == 0 then
        c += cpy(l)
      else
        chksm += sum(c, c + dsk(l)) * ((l + 1) / 2)
        c += dsk(l)
        dsk(l) = 0
      while l + 1 < dsk.size && dsk(l + 1) > 0 do
        next(dsk(l + 1)) match {
          case None =>
            c += dsk(l + 1)
            dsk(l + 1) = 0
          case Some(j) =>
            dsk(l + 1) -= dsk(j)
            chksm += sum(c, c + dsk(j)) * ((j + 1) / 2)
            c += dsk(j)
            dsk(j) = 0
        }

    println(chksm)  // 6382582136592
  }
}

