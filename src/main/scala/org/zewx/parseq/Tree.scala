package org.zewx.parseq

case class Path[I](elements: Seq[I])

/**
  * @tparam I - identifier type
  * @tparam T - value type
  */
trait Tree[I, T]

class Branch[I, T](id: I, children: Seq[Tree[I, T]]) extends Tree[I, T]

case class Par[I, T](id: I, children: Seq[Tree[I, T]]) extends Branch[I, T](id, children)

case class Seq[I, T](id: I, children: Seq[Tree[I, T]]) extends Branch[I, T](id, children)

case class Leaf[I, T](id: I, value: T) extends Tree[I, T]
