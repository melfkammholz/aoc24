package day09

import scala.collection.mutable.{Map, Set}
import scala.io.Source

object A {
  def main(args: Array[String]): Unit = {
    var dsk = Source.stdin.getLines().next.split("").map(_.toInt)

    var chksm = 0l

    var i = 0
    var j = (dsk.length + 1) / 2 - 1
    var c = 0
    var l = 0
    var r = dsk.size - 1
    while l <= r do
      if l % 2 == 0 then
        while dsk(l) > 0 do
          dsk(l) -= 1
          chksm += c * i
          c += 1
        i += 1
      else
        while dsk(l) > 0 && dsk(r) > 0 do
          dsk(l) -= 1
          dsk(r) -= 1
          chksm += c * j
          c += 1
          if dsk(r) == 0 then
            j -= 1
            r -= 2
      l += 1

    println(chksm)  // 6353658451014
  }
}

