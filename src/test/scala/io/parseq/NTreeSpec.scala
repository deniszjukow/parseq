package io.parseq

import cats.instances.int._
import org.scalatest.{FeatureSpec, GivenWhenThen}
import io.parseq.ParSeq._
import cats.data.NonEmptyList.{of => path}

class NTreeSpec extends FeatureSpec with GivenWhenThen {

  feature("user primes a proto tree") {

    import cats.instances.int._
    import cats.instances.string._

    scenario("user primes an empty proto tree") {
      Given("a empty seq node")
      val tree = PTree.empty[String]

      When("map is called")
      val prime = tree.prime

      Then("the resulting tree is empty")
      assert(prime === NTree.empty)
    }

    scenario("user primes a non empty proto tree") {
      Given("a empty seq node")
      val tree = PTree.leaf("one")

      When("map is called")
      val prime = tree.prime

      Then("the resulting tree is empty")
      assert(prime === NTree.leaf(path(0), "one"))
    }

    scenario("user primes a non empty proto seq tree") {
      Given("a empty seq node")
      val tree = {
        import PTree._
        seq(leaf("one"), leaf("two"))
      }

      When("map is called")
      val prime = tree.prime

      Then("the resulting tree is empty")
      assert(prime === {
        import NTree._
        seq(path(0), "", Seq(
          leaf(path(0, 0), "one"),
          leaf(path(0, 1), "two")
        ))
      })
    }

    scenario("user primes a non empty proto par tree") {
      Given("a empty seq node")
      val tree = {
        import PTree._
        par(leaf("one"), leaf("two"))
      }

      When("map is called")
      val prime = tree.prime

      Then("the resulting tree is empty")
      assert(prime === {
        import NTree._
        par(path(0), "", Seq(
          leaf(path(0, 0), "one"),
          leaf(path(0, 1), "two")
        ))
      })
    }
  }

  import cats.instances.string._
  import cats.syntax.semigroup._
  import io.parseq.{PTree => p, ParSeq => n}

  val (leftPar, rightPar) = (p.par(p.leaf("x"), p.leaf("y")).prime, p.par(p.leaf("a"), p.leaf("b")).prime)

  val (leftSeq, rightSeq) = (p.seq(p.leaf("x"), p.leaf("y")).prime, p.seq(p.leaf("a"), p.leaf("b")).prime)

  val (leftLeaf, rightLeaf) = (p.leaf("x").prime, p.leaf("y").prime)

  val empty: NTree[ParSeq, Int, String] = n.empty

  feature("par is combined with another tree") {

    scenario("1.1 combine a par tree with another par tree (the trees have the same shape)") {
      Given("two par trees")
      val l = n.par(path(0), "", Seq(
        n.leaf(path(0, 0), "x"),
        n.leaf(path(0, 1), "y")
      ))

      val r = n.par(path(0), "", Seq(
        n.leaf(path(0, 0), "a"),
        n.leaf(path(0, 1), "b")
      ))

      When("the trees are combined")
      val c = l |+| r

      Then("the trees are merged into a par tree having the same shape (leaf nodes are merged)")
      assert(c === n.par(path(0), "", Seq(
        n.leaf(path(0, 0), "xa"),
        n.leaf(path(0, 1), "yb")
      )))
    }

    scenario("1.2 combine a par tree with another par tree (the trees have different shape)") {
      Given("two par trees")
      val l = n.par(path(0), "", Seq(
        n.leaf(path(0, 0), "x"),
        n.leaf(path(0, 1), "y")
      ))

      val r = n.par(path(1), "", Seq(
        n.leaf(path(1, 0), "a"),
        n.leaf(path(1, 1), "b")
      ))

      When("the trees are combined")
      val c = l |+| r

      Then("the trees are merged into a par tree having the same shape (leaf nodes are merged)")
      assert(c === n.par(path(0), "", Seq(
        n.par(path(0, 0), "", Seq(
          n.leaf(path(0, 0, 0), "x"),
          n.leaf(path(0, 0, 1), "y"))),
        n.par(path(0, 1), "", Seq(
          n.leaf(path(0, 1, 0), "a"),
          n.leaf(path(0, 1, 1), "b"))
        ))))
    }

    scenario("2.1 combine a par tree with a seq tree (the trees have the same shape)") {
      Given("a pair of par and seq trees")
      val l = n.par(path(0), "", Seq(
        n.leaf(path(0, 0), "x"),
        n.leaf(path(0, 1), "y")
      ))

      val r = n.seq(path(0), "", Seq(
        n.leaf(path(0, 0), "a"),
        n.leaf(path(0, 1), "b")
      ))

      When("the trees are combined")
      val c = l |+| r

      Then("the trees are merged into a seq tree having the same shape (leaf nodes are merged)")
      assert(c === n.seq(path(0), "", Seq(
        n.leaf(path(0, 0), "xa"),
        n.leaf(path(0, 1), "yb")
      )))
    }

    scenario("2.2 combine a par tree with a seq tree (the trees have different shape)") {
      Given("two par trees")
      val l = n.par(path(0), "", Seq(
        n.leaf(path(0, 0), "x"),
        n.leaf(path(0, 1), "y")
      ))

      val r = n.seq(path(1), "", Seq(
        n.leaf(path(1, 0), "a"),
        n.leaf(path(1, 1), "b")
      ))

      When("the trees are combined")
      val c = l |+| r

      Then("the trees are merged into a par tree having the same shape (leaf nodes are merged)")
      assert(c === n.seq(path(0), "", Seq(
        n.par(path(0, 0), "", Seq(
          n.leaf(path(0, 0, 0), "x"),
          n.leaf(path(0, 0, 1), "y"))),
        n.seq(path(0, 1), "", Seq(
          n.leaf(path(0, 1, 0), "a"),
          n.leaf(path(0, 1, 1), "b")))
      )))
    }

    scenario("3.1 combine a par tree with a leaf node having existing id") {
      Given("a par tree and a leaf node")
      val l = n.par(path(0), "", Seq(
        n.leaf(path(0, 0), "x"),
        n.leaf(path(0, 1), "y")
      ))

      val r = n.leaf(path(0), "a")

      When("the trees is combined with the leaf node")
      val c = l |+| r

      Then("the trees are merged into a par tree (left leaf node of the left tree is merged with the right leaf node)")
      assert(c === n.par(path(0), "a", Seq(
        n.leaf(path(0, 0), "x"),
        n.leaf(path(0, 1), "y")
      )))
    }

    scenario("3.2 combine a par tree with a leaf node having non-existing id") {
      Given("a par tree and a leaf node")
      val l = n.par(path(0), "", Seq(
        n.leaf(path(0, 0), "x"),
        n.leaf(path(0, 1), "y")
      ))

      val r = n.leaf(path(1), "a")

      When("the trees is combined with the leaf node")
      val c = l |+| r

      Then("the trees are merged into a par tree (left leaf node is merged with the right leaf node)")
      assert(c === n.par(path(0), "", Seq(
        n.par(path(0, 0), "", Seq(
          n.leaf(path(0, 0, 0), "x"),
          n.leaf(path(0, 0, 1), "y"))),
        n.leaf(path(0, 1), "a")
      )))
    }

    scenario("4. combine a par tree with an empty tree") {
      Given("a par tree and a leaf node")
      val l = n.par(path(0), "", Seq(
        n.leaf(path(0, 0), "x"),
        n.leaf(path(0, 1), "y")
      ))

      val r = n.empty

      When("the trees is combined with the leaf node")
      val c = l |+| r

      Then("the tree does not change")
      assert(c === l)
    }
  }

  feature("seq is combined with another tree") {
    scenario("5.1 combine a seq tree with a par tree (the trees have the same shape)") {
      Given("a pair of seq and par trees")
      val l = n.seq(path(0), "", Seq(
        n.leaf(path(0, 0), "x"),
        n.leaf(path(0, 1), "y")
      ))

      val r = n.par(path(0), "", Seq(
        n.leaf(path(0, 0), "a"),
        n.leaf(path(0, 1), "b")
      ))

      When("the trees are combined")
      val c = l |+| r

      Then("the trees are merged into a seq tree having the same shape (leaf nodes are merged)")
      assert(c === n.seq(path(0), "", Seq(
        n.leaf(path(0, 0), "xa"),
        n.leaf(path(0, 1), "yb")
      )))
    }

    scenario("5.2 combine a seq tree with a par tree (the trees have different shape)") {
      Given("two par trees")
      val l = n.seq(path(0), "", Seq(
        n.leaf(path(0, 0), "x"),
        n.leaf(path(0, 1), "y")
      ))

      val r = n.par(path(1), "", Seq(
        n.leaf(path(1, 0), "a"),
        n.leaf(path(1, 1), "b")
      ))

      When("the trees are combined")
      val c = l |+| r

      Then("the trees are merged into a par tree having the same shape (leaf nodes are merged)")
      assert(c === n.seq(path(0), "", Seq(
        n.seq(path(0, 0), "", Seq(
          n.leaf(path(0, 0, 0), "x"),
          n.leaf(path(0, 0, 1), "y"))),
        n.par(path(0, 1), "", Seq(
          n.leaf(path(0, 1, 0), "a"),
          n.leaf(path(0, 1, 1), "b")))
      )))
    }

    scenario("6.1 combine a seq tree with another seq tree (the trees have the same shape)") {
      Given("two seq trees")
      val l = n.seq(path(0), "", Seq(
        n.leaf(path(0, 0), "x"),
        n.leaf(path(0, 1), "y")
      ))

      val r = n.seq(path(0), "", Seq(
        n.leaf(path(0, 0), "a"),
        n.leaf(path(0, 1), "b")
      ))

      When("the trees are combined")
      val c = l |+| r

      Then("the trees are merged into a seq tree having the same shape (leaf nodes are merged)")
      assert(c === n.seq(path(0), "", Seq(
        n.leaf(path(0, 0), "xa"),
        n.leaf(path(0, 1), "yb")
      )))
    }

    scenario("6.2 combine a par tree with a seq tree (the trees have different shape)") {
      Given("two par trees")
      val l = n.seq(path(0), "", Seq(
        n.leaf(path(0, 0), "x"),
        n.leaf(path(0, 1), "y")
      ))

      val r = n.seq(path(1), "", Seq(
        n.leaf(path(1, 0), "a"),
        n.leaf(path(1, 1), "b")
      ))

      When("the trees are combined")
      val c = l |+| r

      Then("the trees are merged into a par tree having the same shape (leaf nodes are merged)")
      assert(c === n.seq(path(0), "", Seq(
        n.seq(path(0, 0), "", Seq(
          n.leaf(path(0, 0, 0), "x"),
          n.leaf(path(0, 0, 1), "y"))),
        n.seq(path(0, 1), "", Seq(
          n.leaf(path(0, 1, 0), "a"),
          n.leaf(path(0, 1, 1), "b")))
      )))
    }

    scenario("7.1 combine a par tree with a leaf node having existing id") {
      Given("a seq tree and a leaf node")
      val l = n.seq(path(0), "", Seq(
        n.leaf(path(0, 0), "x"),
        n.leaf(path(0, 1), "y")
      ))

      val r = n.leaf(path(0), "a")

      When("the trees is combined with the leaf node")
      val c = l |+| r

      Then("the trees are merged into a seq tree (left leaf node of the left tree is merged with the right leaf node)")
      assert(c === n.seq(path(0), "a", Seq(
        n.leaf(path(0, 0), "x"),
        n.leaf(path(0, 1), "y")
      )))
    }

    scenario("7.2 combine a par tree with a leaf node having non-existing id") {
      Given("a par tree and a leaf node")
      val l = n.seq(path(0), "", Seq(
        n.leaf(path(0, 0), "x"),
        n.leaf(path(0, 1), "y")
      ))

      val r = n.leaf(path(1), "a")

      When("the trees is combined with the leaf node")
      val c = l |+| r

      Then("the trees are merged into a seq tree (left leaf node is merged with the right leaf node)")
      assert(c === n.seq(path(0), "", Seq(
        n.seq(path(0, 0), "", Seq(
          n.leaf(path(0, 0, 0), "x"),
          n.leaf(path(0, 0, 1), "y"))),
        n.leaf(path(0, 1), "a")
      )))
    }

    scenario("8. combine a par tree with an empty tree") {
      Given("a par tree and a leaf node")
      val l = n.seq(path(0), "", Seq(
        n.leaf(path(0, 0), "x"),
        n.leaf(path(0, 1), "y")
      ))

      val r = n.empty

      When("the trees is combined with the leaf node")
      val c = l |+| r

      Then("the tree does not change")
      assert(c === l)
    }
  }


  feature("leaf is combined with another tree") {
    scenario("9.1 combine a leaf with a par tree (the trees have the same shape)") {
      Given("a pair of leaf and par trees")
      val l = n.leaf(path(0), "x")

      val r = n.par(path(0), "", Seq(
        n.leaf(path(0, 0), "a"),
        n.leaf(path(0, 1), "b")
      ))

      When("the trees are combined")
      val c = l |+| r

      Then("the trees are merged into a par tree (left leaf node is merged with the left leaf node of the right tree)")
      assert(c === n.par(path(0), "x", Seq(
        n.leaf(path(0, 0), "a"),
        n.leaf(path(0, 1), "b")
      )))
    }

    scenario("9.2 combine a seq tree with a par tree (the trees have different shape)") {
      Given("a pair of leaf and par trees")
      val l = n.leaf(path(0), "x")

      val r = n.par(path(1), "", Seq(
        n.leaf(path(1, 0), "a"),
        n.leaf(path(1, 1), "b")
      ))

      When("the trees are combined")
      val c = l |+| r

      Then("the trees are merged into a par tree (left leaf node is prepended as a child to with the right tree)")
      assert(c === n.par(path(0), "", Seq(
        n.leaf(path(0, 0), "x"),
        n.par(path(0, 1), "", Seq(
          n.leaf(path(0, 1, 0), "a"),
          n.leaf(path(0, 1, 1), "b")))
      )))
    }

    scenario("10.1 combine a leaf node with a seq tree (the trees have the same shape)") {
      Given("a leaf node and a seq tree")
      val l = n.leaf(path(0), "x")

      val r = n.seq(path(0), "", Seq(
        n.leaf(path(0, 0), "a"),
        n.leaf(path(0, 1), "b")
      ))

      When("the trees are combined")
      val c = l |+| r

      Then("the trees are merged into a seq tree having the same shape (leaf nodes are merged)")
      assert(c === n.seq(path(0), "x", Seq(
        n.leaf(path(0, 0), "a"),
        n.leaf(path(0, 1), "b")
      )))
    }

    scenario("10.2 combine a leaf node with a seq tree (the trees have different shape)") {
      Given("a pair of leaf and par trees")
      val l = n.leaf(path(0), "x")

      val r = n.seq(path(1), "", Seq(
        n.leaf(path(1, 0), "a"),
        n.leaf(path(1, 1), "b")
      ))

      When("the trees are combined")
      val c = l |+| r

      Then("the trees are merged into a par tree (left leaf node is prepended as a child to with the right tree)")
      assert(c === n.seq(path(0), "", Seq(
        n.leaf(path(0, 0), "x"),
        n.seq(path(0, 1), "", Seq(
          n.leaf(path(0, 1, 0), "a"),
          n.leaf(path(0, 1, 1), "b")))
      )))
    }

    scenario("11.1 combine a leaf node with another leaf node having the same id") {
      Given("two leaf nodes")
      val l = n.leaf(path(0), "x")

      val r = n.leaf(path(0), "a")

      When("the trees is combined with the leaf node")
      val c = l |+| r

      Then("the nodes are merged")
      assert(c === n.leaf(path(0), "xa"))
    }

    scenario("11.2 combine a leaf node with another leaf node having a different id") {
      Given("a par tree and a leaf node")
      val l = n.leaf(path(0), "x")

      val r = n.leaf(path(1), "a")

      When("the trees is combined with the leaf node")
      val c = l |+| r

      Then("the trees are merged into a seq tree (left leaf node is merged with the right leaf node)")
      assert(c === n.par(path(0), "", Seq(
        n.leaf(path(0, 0), "x"),
        n.leaf(path(0, 1), "a")
      )))
    }

    scenario("12. combine a leaf node with an empty tree") {
      Given("a par tree and a leaf node")
      val l = n.leaf(path(0), "x")

      val r = n.empty

      When("the trees is combined with the leaf node")
      val c = l |+| r

      Then("the tree does not change")
      assert(c === l)
    }
  }

  feature("empty tree is combined with another tree") {

    scenario("13. combine an empty tree with a par tree") {
      Given("an empty tree and a par tree")
      val l = n.empty[Int, String]

      val r = n.par(path(0), "", Seq(
        n.leaf(path(0, 0), "a"),
        n.leaf(path(0, 1), "b")
      ))

      When("the trees are combined")
      val c = l |+| r

      Then("the trees does not change")
      assert(c === r)
    }

    scenario("14. combine an empty tree with a seq tree") {
      Given("an empty tree and a seq tree")
      val l = n.empty[Int, String]

      val r = n.seq(path(0), "", Seq(
        n.leaf(path(0, 0), "a"),
        n.leaf(path(0, 1), "b")
      ))

      When("the trees are combined")
      val c = l |+| r

      Then("the tree does not change")
      assert(c === r)
    }

    scenario("15. combine a par tree with a leaf node having existing id") {
      Given("an empty tree and a leaf node")
      val l = n.empty[Int, String]

      val r = n.leaf(path(0), "a")

      When("the trees are combined")
      val c = l |+| r

      Then("the tree does not change")
      assert(c === r)
    }

    scenario("16. combine two empty trees") {
      Given("a two empty trees")
      val l = n.empty[Int, String]

      val r = n.empty[Int, String]

      When("the trees is combined with the leaf node")
      val c = l |+| r

      Then("the tree does not change")
      assert(c === l)
    }
  }

  feature("more complex example") {

    scenario("more complex scenario") {
      Given("an empty tree and a par tree")
      import cats.instances.map._
      val l = n.seq(path(0), Map.empty[String, Int], Seq(
        n.par(path(0, 0), Map.empty[String, Int], Seq(
          n.leaf(path(0, 0, 0), Map("a" -> 1)),
          n.leaf(path(0, 0, 1), Map("b" -> 2))
        )),
        n.par(path(0, 1), Map.empty[String, Int], Seq(
          n.leaf(path(0, 1, 0), Map("c" -> 3)),
          n.leaf(path(0, 1, 1), Map("d" -> 4))
        ))
      ))

      val r = fromPath(path(0, 1, 0), Map("c" -> 10, "e" -> 5))

      When("the trees are combined")
      val c = l |+| r

      Then("the trees does not change")
      assert(c === n.seq(path(0), Map.empty[String, Int], Seq(
        n.par(path(0, 0), Map.empty[String, Int], Seq(
          n.leaf(path(0, 0, 0), Map("a" -> 1)),
          n.leaf(path(0, 0, 1), Map("b" -> 2))
        )),
        n.par(path(0, 1), Map.empty[String, Int], Seq(
          n.leaf(path(0, 1, 0), Map("c" -> 13, "e" -> 5)),
          n.leaf(path(0, 1, 1), Map("d" -> 4))
        ))
      )))
    }
  }

  feature("it should be possible to create a tree from path and value") {

    scenario("tree is created from path and value") {
      Given("a path and value")
      val value = "a"

      When("fromLeaf is called")
      val tree = ParSeq.fromPath(path(0, 1, 2), value)

      Then("the tree is created")
      assert(tree === n.neutral(path(0), "", Seq(
        n.neutral[Int, String](path(0, 1), "", Seq(
          n.leaf(path(0, 1, 2), "a")
        ))
      )))
    }
  }
}
