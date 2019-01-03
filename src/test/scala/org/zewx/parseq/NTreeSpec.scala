package org.zewx.parseq

import org.scalatest.{FeatureSpec, GivenWhenThen}
import cats.instances.int._
import cats.data.NonEmptyList.one
import BranchType._


class NTreeSpec extends FeatureSpec with GivenWhenThen {

  feature("user primes a proto tree") {

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
      assert(prime === NTree.leaf(one(0), "one"))
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
        seq(0,
          leaf((0, 0), "one"),
          leaf((0, 1), "two")
        )
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
        par(0,
          leaf((0, 0), "one"),
          leaf((0, 1), "two")
        )
      })
    }
  }

  import cats.syntax.semigroup._
  import cats.instances.string._
  import org.zewx.parseq.{PTree => p}
  import org.zewx.parseq.{NTree => n}

  val (leftPar, rightPar) = (p.par(p.leaf("x"), p.leaf("y")).prime, p.par(p.leaf("a"), p.leaf("b")).prime)

  val (leftSeq, rightSeq) = (p.seq(p.leaf("x"), p.leaf("y")).prime, p.seq(p.leaf("a"), p.leaf("b")).prime)

  val (leftLeaf, rightLeaf) = (p.leaf("x").prime, p.leaf("y").prime)

  val empty: NTree[BranchType, Int, String] = n.empty

  feature("par is combined with another tree") {

    scenario("1.1 combine a par tree with another par tree (the trees have the same shape)") {
      Given("two par trees")
      val l = n.par(0,
        n.leaf((0, 0), "x"),
        n.leaf((0, 1), "y"))

      val r = n.par(0,
        n.leaf((0, 0), "a"),
        n.leaf((0, 1), "b"))

      When("the trees are combined")
      val c = l |+| r

      Then("the trees are merged into a par tree having the same shape (leaf nodes are merged)")
      assert(c === n.par(0,
        n.leaf((0, 0), "xa"),
        n.leaf((0, 1), "yb")
      ))
    }

    scenario("1.2 combine a par tree with another par tree (the trees have different shape)") {
      Given("two par trees")
      val l = n.par(0,
        n.leaf((0, 0), "x"),
        n.leaf((0, 1), "y"))

      val r = n.par(1,
        n.leaf((1, 0), "a"),
        n.leaf((1, 1), "b"))

      When("the trees are combined")
      val c = l |+| r

      Then("the trees are merged into a par tree having the same shape (leaf nodes are merged)")
      assert(c === n.par(0,
        n.par[Int, String]((0, 0),
          n.leaf((0, 0, 0), "x"),
          n.leaf((0, 0, 1), "y")),
        n.par[Int, String]((0, 1),
          n.leaf((0, 1, 0), "a"),
          n.leaf((0, 1, 1), "b"))
      ))
    }

    scenario("2.1 combine a par tree with a seq tree (the trees have the same shape)") {
      Given("a pair of par and seq trees")
      val l = n.par(0,
        n.leaf((0, 0), "x"),
        n.leaf((0, 1), "y"))

      val r = n.seq(0,
        n.leaf((0, 0), "a"),
        n.leaf((0, 1), "b"))

      When("the trees are combined")
      val c = l |+| r

      Then("the trees are merged into a seq tree having the same shape (leaf nodes are merged)")
      assert(c === n.seq(0,
        n.leaf((0, 0), "xa"),
        n.leaf((0, 1), "yb")
      ))
    }

    scenario("2.2 combine a par tree with a seq tree (the trees have different shape)") {
      Given("two par trees")
      val l = n.par(0,
        n.leaf((0, 0), "x"),
        n.leaf((0, 1), "y"))

      val r = n.seq(1,
        n.leaf((1, 0), "a"),
        n.leaf((1, 1), "b"))

      When("the trees are combined")
      val c = l |+| r

      Then("the trees are merged into a par tree having the same shape (leaf nodes are merged)")
      assert(c === n.seq(0,
        n.par[Int, String]((0, 0),
          n.leaf((0, 0, 0), "x"),
          n.leaf((0, 0, 1), "y")),
        n.seq[Int, String]((0, 1),
          n.leaf((0, 1, 0), "a"),
          n.leaf((0, 1, 1), "b"))
      ))
    }

    scenario("3.1 combine a par tree with a leaf node having existing id") {
      Given("a par tree and a leaf node")
      val l = n.par(0,
        n.leaf((0, 0), "x"),
        n.leaf((0, 1), "y"))

      val r = n.leaf(0, "a")

      When("the trees is combined with the leaf node")
      val c = l |+| r

      Then("the trees are merged into a par tree (left leaf node of the left tree is merged with the right leaf node)")
      assert(c === n.par(0,
        n.leaf((0, 0), "xa"),
        n.leaf((0, 1), "y")
      ))
    }

    scenario("3.2 combine a par tree with a leaf node having non-existing id") {
      Given("a par tree and a leaf node")
      val l = n.par(0,
        n.leaf((0, 0), "x"),
        n.leaf((0, 1), "y"))

      val r = n.leaf(1, "a")

      When("the trees is combined with the leaf node")
      val c = l |+| r

      Then("the trees are merged into a par tree (left leaf node is merged with the right leaf node)")
      assert(c === n.par(0,
        n.par[Int, String]((0, 0),
          n.leaf((0, 0, 0), "x"),
          n.leaf((0, 0, 1), "y")),
        n.leaf((0, 1), "a")
      ))
    }

    scenario("4. combine a par tree with an empty tree") {
      Given("a par tree and a leaf node")
      val l = n.par(0,
        n.leaf((0, 0), "x"),
        n.leaf((0, 1), "y"))

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
      val l = n.seq(0,
        n.leaf((0, 0), "x"),
        n.leaf((0, 1), "y"))

      val r = n.par(0,
        n.leaf((0, 0), "a"),
        n.leaf((0, 1), "b"))

      When("the trees are combined")
      val c = l |+| r

      Then("the trees are merged into a seq tree having the same shape (leaf nodes are merged)")
      assert(c === n.seq(0,
        n.leaf((0, 0), "xa"),
        n.leaf((0, 1), "yb")
      ))
    }

    scenario("5.2 combine a seq tree with a par tree (the trees have different shape)") {
      Given("two par trees")
      val l = n.seq(0,
        n.leaf((0, 0), "x"),
        n.leaf((0, 1), "y"))

      val r = n.par(1,
        n.leaf((1, 0), "a"),
        n.leaf((1, 1), "b"))

      When("the trees are combined")
      val c = l |+| r

      Then("the trees are merged into a par tree having the same shape (leaf nodes are merged)")
      assert(c === n.seq(0,
        n.seq[Int, String]((0, 0),
          n.leaf((0, 0, 0), "x"),
          n.leaf((0, 0, 1), "y")),
        n.par[Int, String]((0, 1),
          n.leaf((0, 1, 0), "a"),
          n.leaf((0, 1, 1), "b"))
      ))
    }

    scenario("6.1 combine a seq tree with another seq tree (the trees have the same shape)") {
      Given("two seq trees")
      val l = n.seq(0,
        n.leaf((0, 0), "x"),
        n.leaf((0, 1), "y"))

      val r = n.seq(0,
        n.leaf((0, 0), "a"),
        n.leaf((0, 1), "b"))

      When("the trees are combined")
      val c = l |+| r

      Then("the trees are merged into a seq tree having the same shape (leaf nodes are merged)")
      assert(c === n.seq(0,
        n.leaf((0, 0), "xa"),
        n.leaf((0, 1), "yb")
      ))
    }

    scenario("6.2 combine a par tree with a seq tree (the trees have different shape)") {
      Given("two par trees")
      val l = n.seq(0,
        n.leaf((0, 0), "x"),
        n.leaf((0, 1), "y"))

      val r = n.seq(1,
        n.leaf((1, 0), "a"),
        n.leaf((1, 1), "b"))

      When("the trees are combined")
      val c = l |+| r

      Then("the trees are merged into a par tree having the same shape (leaf nodes are merged)")
      assert(c === n.seq(0,
        n.seq[Int, String]((0, 0),
          n.leaf((0, 0, 0), "x"),
          n.leaf((0, 0, 1), "y")),
        n.seq[Int, String]((0, 1),
          n.leaf((0, 1, 0), "a"),
          n.leaf((0, 1, 1), "b"))
      ))
    }

    scenario("7.1 combine a par tree with a leaf node having existing id") {
      Given("a seq tree and a leaf node")
      val l = n.seq(0,
        n.leaf((0, 0), "x"),
        n.leaf((0, 1), "y"))

      val r = n.leaf(0, "a")

      When("the trees is combined with the leaf node")
      val c = l |+| r

      Then("the trees are merged into a seq tree (left leaf node of the left tree is merged with the right leaf node)")
      assert(c === n.seq(0,
        n.leaf((0, 0), "xa"),
        n.leaf((0, 1), "y")
      ))
    }

    scenario("7.2 combine a par tree with a leaf node having non-existing id") {
      Given("a par tree and a leaf node")
      val l = n.seq(0,
        n.leaf((0, 0), "x"),
        n.leaf((0, 1), "y"))

      val r = n.leaf(1, "a")

      When("the trees is combined with the leaf node")
      val c = l |+| r

      Then("the trees are merged into a seq tree (left leaf node is merged with the right leaf node)")
      assert(c === n.seq(0,
        n.seq[Int, String]((0, 0),
          n.leaf((0, 0, 0), "x"),
          n.leaf((0, 0, 1), "y")),
        n.leaf((0, 1), "a")
      ))
    }

    scenario("8. combine a par tree with an empty tree") {
      Given("a par tree and a leaf node")
      val l = n.seq(0,
        n.leaf((0, 0), "x"),
        n.leaf((0, 1), "y"))

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
      val l = n.leaf[BranchType, Int, String](0, "x")

      val r = n.par(0,
        n.leaf((0, 0), "a"),
        n.leaf((0, 1), "b"))

      When("the trees are combined")
      val c = l |+| r

      Then("the trees are merged into a par tree (left leaf node is merged with the left leaf node of the right tree)")
      assert(c === n.par(0,
        n.leaf((0, 0), "xa"),
        n.leaf((0, 1), "b")
      ))
    }

    scenario("9.2 combine a seq tree with a par tree (the trees have different shape)") {
      Given("a pair of leaf and par trees")
      val l = n.leaf[BranchType, Int, String](0, "x")

      val r = n.par(1,
        n.leaf((1, 0), "a"),
        n.leaf((1, 1), "b"))

      When("the trees are combined")
      val c = l |+| r

      Then("the trees are merged into a par tree (left leaf node is prepended as a child to with the right tree)")
      assert(c === n.par(0,
        n.leaf((0, 0), "x"),
        n.par[Int, String]((0, 1),
          n.leaf((0, 1, 0), "a"),
          n.leaf((0, 1, 1), "b"))
      ))
    }

    scenario("10.1 combine a leaf node with a seq tree (the trees have the same shape)") {
      Given("a leaf node and a seq tree")
      val l = n.leaf[BranchType, Int, String](0, "x")

      val r = n.seq(0,
        n.leaf((0, 0), "a"),
        n.leaf((0, 1), "b"))

      When("the trees are combined")
      val c = l |+| r

      Then("the trees are merged into a seq tree having the same shape (leaf nodes are merged)")
      assert(c === n.seq(0,
        n.leaf((0, 0), "xa"),
        n.leaf((0, 1), "b")
      ))
    }

    scenario("10.2 combine a leaf node with a seq tree (the trees have different shape)") {
      Given("a pair of leaf and par trees")
      val l = n.leaf[BranchType, Int, String](0, "x")

      val r = n.seq(1,
        n.leaf((1, 0), "a"),
        n.leaf((1, 1), "b"))

      When("the trees are combined")
      val c = l |+| r

      Then("the trees are merged into a par tree (left leaf node is prepended as a child to with the right tree)")
      assert(c === n.seq(0,
        n.leaf((0, 0), "x"),
        n.seq[Int, String]((0, 1),
          n.leaf((0, 1, 0), "a"),
          n.leaf((0, 1, 1), "b"))
      ))
    }

    scenario("11.1 combine a leaf node with another leaf node having the same id") {
      Given("two leaf nodes")
      val l = n.leaf[BranchType, Int, String](0, "x")

      val r = n.leaf(0, "a")

      When("the trees is combined with the leaf node")
      val c = l |+| r

      Then("the nodes are merged")
      assert(c === n.leaf(0, "xa"))
    }

    scenario("11.2 combine a leaf node with another leaf node having a different id") {
      Given("a par tree and a leaf node")
      val l = n.leaf[BranchType, Int, String](0, "x")

      val r = n.leaf(1, "a")

      When("the trees is combined with the leaf node")
      val c = l |+| r

      Then("the trees are merged into a seq tree (left leaf node is merged with the right leaf node)")
      assert(c === n.seq(0,
        n.leaf((0, 0), "x"),
        n.leaf((0, 1), "a")
      ))
    }

    scenario("12. combine a leaf node with an empty tree") {
      Given("a par tree and a leaf node")
      val l = n.leaf[BranchType, Int, String](0, "x")

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
      val l = n.empty[BranchType, Int, String]

      val r = n.par(0,
        n.leaf((0, 0), "a"),
        n.leaf((0, 1), "b"))

      When("the trees are combined")
      val c = l |+| r

      Then("the trees does not change")
      assert(c === r)
    }

    scenario("14. combine an empty tree with a seq tree") {
      Given("an empty tree and a seq tree")
      val l = n.empty[BranchType, Int, String]

      val r = n.seq(0,
        n.leaf((0, 0), "a"),
        n.leaf((0, 1), "b"))

      When("the trees are combined")
      val c = l |+| r

      Then("the tree does not change")
      assert(c === r)
    }

    scenario("15. combine a par tree with a leaf node having existing id") {
      Given("an empty tree and a leaf node")
      val l = n.empty[BranchType, Int, String]

      val r = n.leaf(0, "a")

      When("the trees are combined")
      val c = l |+| r

      Then("the tree does not change")
      assert(c === r)
    }

    scenario("16. combine two empty trees") {
      Given("a two empty trees")
      val l = n.empty[BranchType, Int, String]

      val r = n.empty[BranchType, Int, String]

      When("the trees is combined with the leaf node")
      val c = l |+| r

      Then("the tree does not change")
      assert(c === l)
    }
  }
}
