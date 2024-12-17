package day16

import scala.collection.mutable.*
import scala.io.Source
import scala.math.Ordering.Implicits._
import scala.math.Numeric


extension [T](a: Array[Array[T]])
  def apply(v: Vec2[Int]): T = a(v.x)(v.y)
  def update(v: Vec2[Int], x: T): Unit = a(v.x)(v.y) = x


class Vec2[T](val x: T, val y: T)(using num: Numeric[T]) extends Ordered[Vec2[T]]:
  import scala.math.Ordered.orderingToOrdered
  import num._

  infix def +(other: Vec2[T]): Vec2[T] = Vec2[T](x + other.x, y + other.y)

  override def compare(that: Vec2[T]) = (x, y) compare (that.x, that.y)

  override def toString: String = s"Vec2($x, $y)"
  override def hashCode: Int = (x, y).hashCode
  override def equals(that: Any): Boolean = that match
    case that @ Vec2(ox, oy) => x == ox && y == oy
    case _ => false

object Vec2:
  def apply[T](x: T, y: T)(using Numeric[T]) = new Vec2(x, y)
  def unapply[T](v: Vec2[T]): Option[(T, T)] = Some((v.x, v.y))


enum Dir extends Ordered[Dir]:
  case North, East, South, West

  override def compare(that: Dir) = toInt compare that.toInt

  def right: Dir = this match
    case North => East
    case East => South
    case South => West
    case West => North

  def left: Dir = this match
    case North => West
    case East => North
    case South => East
    case West => South

  def toVec2: Vec2[Int] = this match
    case North => Vec2(0, -1)
    case East => Vec2(1, 0)
    case South => Vec2(0, 1)
    case West => Vec2(-1, 0)

  def toInt: Int = this match
    case North => 0
    case East => 1
    case South => 2
    case West => 3


object B:
  def main(args: Array[String]): Unit =
    val g = Source.stdin.getLines.map(_.toCharArray).toArray
    val (h, w) = (g.size, g.head.size)

    val sy = g.indexWhere(_.contains('S'))
    val sx = g(sy).indexWhere(_.equals('S'))
    val ey = g.indexWhere(_.contains('E'))
    val ex = g(ey).indexWhere(_.equals('E'))


    val dist = Array.fill(w, h)(Map[Dir, Int]().withDefaultValue(Int.MaxValue))
    dist(sx)(sy)(Dir.East) = 0

    val q = PriorityQueue[(Int, Vec2[Int], Dir)]()
    q.enqueue((0, Vec2(sx, sy), Dir.East))
    while q.nonEmpty do
      val (p, v, d) = q.dequeue
      if p <= dist(v)(d) then
        for
          (c, w, nd) <- List((1, v + d.toVec2, d), (1000, v, d.left), (1000, v, d.right))
          if g(w) != '#'
          if dist(v)(d) + c < dist(w)(nd)
        do
          dist(w)(nd) = dist(v)(d) + c
          q.enqueue((dist(w)(nd), w, nd))


    val res = Set[Vec2[Int]]()
    lazy val dfs: (Vec2[Int], Dir, Set[Vec2[Int]]) => Unit = (v, d, p) =>
      if (v == Vec2(ex, ey) && dist(v)(d) == dist(v).values.min) then
        res.addAll(p)
      else
        for
          (c, w, nd) <- List((1, v + d.toVec2, d), (1000, v, d.left), (1000, v, d.right))
          if g(w) != '#'
          if dist(v)(d) + c == dist(w)(nd)
        do
          p.add(w)
          dfs(w, nd, p)
          p.remove(w)

    dfs(Vec2(sx, sy), Dir.East, Set(Vec2(sx, sy)))

    println(res.size)  // 486

