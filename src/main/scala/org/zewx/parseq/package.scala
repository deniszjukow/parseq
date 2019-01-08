package org.zewx

import cats.Functor

import scala.language.higherKinds

package object parseq {

  trait FunctionSplitter[F[_]] {
    def apply[A, B](f: A => F[B]): (A => F[A], A => B)
  }

  class UnsafeFunctionSplitter[F[_]: Functor] extends FunctionSplitter[F] {
    def apply[A, B](f: A => F[B]): (A => F[A], A => B) = {
      def u(a: A): F[A] = implicitly[Functor[F]].map(f(a))(x => x.asInstanceOf)
      def v(a: A): B = a.asInstanceOf[B]
      (u, v)
    }
  }
}
