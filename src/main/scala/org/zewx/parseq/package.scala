package org.zewx

import cats.data.NonEmptyList
import org.zewx.parseq.ParSeq.ParSeqTree

import scala.language.higherKinds

package object parseq {
  type Id = Int
  type Path[+I] = NonEmptyList[I]
  type CtrlName = String
  type Data = Map[String, String]
  type Elem = (CtrlName, Data)
  type Tree[A] = ParSeqTree[Id, A]
  type Chain = List[(Id, Data)]
  type Factory = CtrlName => Ctrl
}
