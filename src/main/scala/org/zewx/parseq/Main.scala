package org.zewx.parseq

object Main {
  def main(args: Array[String]): Unit = {
    import cats.syntax.functor._
    import cats.instances.string._
    import PTree._

    val originalTree: PTree[String] = seq(
      par(
        leaf("11"), leaf("12")
      ),
      par(
        leaf("21"), leaf("22")
      )
    )

    println(originalTree.map(s => s.toInt * 10))

    import cats.instances.int._
    val treeWithId: PTree[(Seq[Int], String)] = originalTree.numerate(1, 1)
    println(treeWithId)
  }
}
