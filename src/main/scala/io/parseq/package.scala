package io

import cats.data.NonEmptyList
import io.parseq.ParSeq._

import scala.language.higherKinds

package object parseq {
  type Id = Int
  type Path[+I] = NonEmptyList[I]
  type CtrlName = String
  type Data = Map[String, String]
  type Elem = (CtrlName, Data)
  type Tree[A] = ParSeqTree[Id, A]
  type Chain = List[(Id, Data)]
  type Factory = CtrlName => Controller
}
