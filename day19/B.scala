package day19

import scala.collection.mutable.*
import scala.io.Source


class Node:
  var idx = -1
  var length = -1
  var link: Node = null
  val children = Map[Char, Node]()

  override def toString =
    s"Node(idx=$idx, link=${link.hashCode}, children=$children)"

  def insert(s: String, idx: Int) =
    var curr = this
    for c <- s do
      if !curr.children.contains(c) then
        curr.children += (c -> Node())
      curr = curr.children(c)
    curr.idx = idx
    curr.length = s.size
    this


object Node:
  def apply() = new Node()


class AhoCorasick:
  val root = Node()

  def this(ws: Array[String]) =
    this()
    ws.zip(0 until ws.size).foreach(root.insert)
    setupLinks()

  override def toString = s"AhoCorasick(root=$root)"

  def setupLinks() =
    var d = 0
    val q = Queue[(Node, Char, Node)]()
    q.enqueue((root, '_', root))
    while q.nonEmpty do
      for _ <- 0 until q.size do
        val (par, ch, node) = q.dequeue
        if d < 2 then
          node.link = root
        else
          var link = par.link
          while link != root && !link.children.contains(ch) do
            link = link.link
          node.link = link.children.getOrElse(ch, root)
        for (nch, next) <- node.children do
          q.enqueue((node, nch, next))
      d += 1

  def search(s: String) =
    var res = ArrayBuffer[(Int, Int)]()
    var curr = root

    var i = 0
    while i < s.size do
      while i < s.size && curr.children.contains(s(i)) do
        val next = curr.children(s(i))
        var link = next
        while link != link.link do
          if link.idx >= 0 then
            res.append((i - link.length + 1, link.idx))
          link = link.link
        curr = next
        i += 1

      if curr == root then
        i += 1
      curr = curr.link

    res


object AhoCorasick:
  def apply(ws: Array[String]) = new AhoCorasick(ws)


object B:
  def main(args: Array[String]): Unit =
    val (tsStr :: _ :: ds) = Source.stdin.getLines.toList : @unchecked
    val ts = tsStr.split(", ")

    val root = AhoCorasick(ts)
    var res = 0l
    for d <- ds do
      val dp = Array.fill(d.size + 1)(0l)
      dp(0) = 1
      for (i, j) <- root.search(d) do
        dp(i + ts(j).size) += dp(i)
      res += dp.last

    println(res)  // 769668867512623

