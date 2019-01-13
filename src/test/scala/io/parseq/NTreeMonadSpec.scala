package io.parseq

import cats.Monad
import cats.data.NonEmptyList.{of => path}
import org.scalatest.{FeatureSpec, GivenWhenThen}

class NTreeMonadSpec extends FeatureSpec with GivenWhenThen {

  feature("user primes a proto tree") {

    import cats.instances.int._
    import cats.syntax.flatMap._
    implicit val monoid: Monad[NTree.NTreeWrapper[Unit, Id]#M] = NTree.NTreeWrapper[Unit, Int].treeMonad
    import NTree._

    scenario("empty tree is flatMapped into empty tree") {
      val fa = empty[Unit, Int, String]
      val fb = fa.flatMap(s => branch((), path(0), "", Seq(leaf(path(0, 0), s), leaf(path(0, 1), s))))
      assert(fb === empty)
    }

    scenario("leaf is flatMapped") {
      val fa = leaf[Unit, Int, String](path(0), "1")
      val fb = fa.flatMap(s =>
        branch((), path(0), "", Seq(
          leaf(path(0, 0), s.toInt * 10),
          leaf(path(0, 1), s.toInt * 20))))
      assert(fb === branch((), path(0), "", Seq(
        leaf(path(0, 0), 10),
        leaf(path(0, 1), 20))))
    }
  }
}
