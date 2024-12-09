package day09

import scala.collection.mutable.Set
import scala.io.Source

object B {
  def main(args: Array[String]): Unit = {
    var dsk = Source.stdin.getLines().next.split("").map(_.toInt)
    var cpy = dsk.clone

    val frags = Array.ofDim[List[Int]](10).mapInPlace(_ => List.empty[Int])
    for i <- dsk.indices.filter(_ % 2 == 0) do
      frags(dsk(i)) = i :: frags(dsk(i))

    val next: Int => Option[Int] = l =>
      var a: Option[Int] = None
      for s <- l to 1 by -1 do
        frags(s) match {
          case Nil =>
          case x :: _ =>
            if a.isEmpty then
              a = Some(s)
            else
              if frags(a.get).head < x then
                a = Some(s)
        }

      if a.isEmpty then
        None
      else
        var res = frags(a.get).head
        frags(a.get) = frags(a.get).tail
        Some(res)

    val seen = Set[Int]()
    var chksm = 0l
    var c = 0
    var l = 0
    while l < dsk.size do
      if seen.contains(l) then
        c += cpy(l)
      else
        seen.add(l)
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
                seen.add(j)
                while dsk(j) > 0 do
                  dsk(l) -= 1
                  dsk(j) -= 1
                  chksm += c * ((j + 1) / 2)
                  c += 1
            }
      l += 1
    println(chksm)
  }
}

