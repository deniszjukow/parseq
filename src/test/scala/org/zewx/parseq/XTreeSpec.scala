package org.zewx.parseq

import org.scalatest.{FeatureSpec, GivenWhenThen}
import org.zewx.parseq.XTree.{leaf, par, seq}
import cats.syntax.functor._

class XTreeSpec extends FeatureSpec with GivenWhenThen {

  feature("user calls map on a tree") {

    scenario("user calls map on an empty sequential tree") {
      Given("a empty seq node")
      val tree = seq[String]()

      When("map is called")
      val mapped = tree.map(_.toInt * 10)

      Then("the resulting tree is empty")
      assert(mapped === seq[String]())
    }

    scenario("user calls map on an empty parallel tree") {
      Given("a empty seq node")
      val tree = par[String]()

      When("map is called")
      val mapped = tree.map(_.toInt * 10)

      Then("the resulting tree is empty")
      assert(mapped === par[String]())
    }

    scenario("user calls map on a non-empty tree") {
      Given("a non-empty tree")
      val tree = seq(
        par(
          leaf("11"), leaf("12")
        ),
        par(
          leaf("21"), leaf("22")
        )
      )

      When("map is called")
      val mapped = tree.map(_.toInt * 10)

      Then("the resulting tree should have the same shape")
      assert(mapped === seq(
        par(
          leaf(110), leaf(120)
        ),
        par(
          leaf(210), leaf(220)
        )
      ))
    }
  }

  feature("user numerates a tree") {
    Given("a non-empty tree")
    val tree = seq(
      par(
        leaf("11"), leaf("12")
      ),
      par(
        leaf("21"), leaf("22")
      )
    )

    When("user numerates the tree")
    import cats.instances.int._
    val numerated = tree.numerate(1, 1)

    Then("the resulting tree is enumerated and has the same shape as the original tree")
    assert(numerated === seq(
      par(
        leaf((List(1, 1), "11")),
        leaf((List(1, 2), "12"))
      ),
      par(
        leaf((List(2, 1), "21")),
        leaf((List(2, 2), "22"))
      )
    ))
  }
}
