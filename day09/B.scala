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

    var chksm = 0l
    var c = 0
    for l <- 0 until dsk.size do
      if dsk(l) == 0 then
        c += cpy(l)
      else
        if l % 2 == 0 then
          while dsk(l) > 0 do
            dsk(l) -= 1
            chksm += c * ((l + 1) / 2)
            c += 1
        else
          while dsk(l) > 0 do
            next(dsk(l)) match {
              case None =>
                c += dsk(l)
                dsk(l) = 0
              case Some(j) =>
                while dsk(j) > 0 do
                  dsk(l) -= 1
                  dsk(j) -= 1
                  chksm += c * ((j + 1) / 2)
                  c += 1
            }

    println(chksm)  // 6382582136592
  }
}

