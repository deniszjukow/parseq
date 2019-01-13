package io.parseq

sealed trait Response {
  def isEmpty: Boolean = false

  def orElse[A](f: => Response): Response = Response.orElse(this)(f)
}

case object EmptyRes extends Response {
  override def isEmpty: Boolean = true
}

case class RequiredPropertyRes(path: Path[Id], property: String) extends Response

case class RequiredNodeRes(kind: ParSeq, path: Path[Id], ctrlName: CtrlName) extends Response

object Response {

  val empty: Response = EmptyRes

  def requiredProperty(path: Path[Id], property: String) =
    RequiredPropertyRes(path, property)

  def requiredNode(kind: ParSeq, path: Path[Id], ctrlName: CtrlName) =
    RequiredNodeRes(kind, path, ctrlName)

  def orElse[A](res: Response)(f: => Response): Response = if (res.isEmpty) f else res
}
