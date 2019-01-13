package io.parseq

import cats.Functor
import cats.data.NonEmptyList.{of => path}
import org.scalatest.{FeatureSpec, GivenWhenThen}

class NTreeFunctorSpec extends FeatureSpec with GivenWhenThen {

  feature("user primes a proto tree") {

    import cats.syntax.functor._
    implicit val functor: Functor[NTree.NTreeWrapper[Unit, Id]#M] = NTree.NTreeWrapper[Unit, Int].treeFunctor
    import NTree._

    scenario("empty tree is mapped into empty tree") {
      val fa = empty[Unit, Int, String]
      val fb = fa.map(s => s.toInt)
      assert(fb === empty)
    }

    scenario("leaf is mapped into a leaf") {
      val fa = leaf[Unit, Int, String](path(0), "1")
      val fb = fa.map(s => s.toInt * 10)
      assert(fb === leaf[Unit, Int, Int](path(0), 10))
    }

    scenario("branch is mapped into a branch") {
      val fa = branch((), path(0), "1", Seq(
        leaf(path(0, 0), "2")
      ))
      val fb = fa.map(s => s.toInt * 10)
      assert(fb === branch((), path(0), 10, Seq(
        leaf(path(0, 0), 20)
      )))
    }
  }
}
