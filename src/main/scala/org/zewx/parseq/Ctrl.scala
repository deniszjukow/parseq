package org.zewx.parseq

case class Context(path: Path[Id], ctrlName: CtrlName, data: Data, children: Seq[Tree[Elem]])

trait Ctrl {

  def selfCheck(ctx: Context): Response

  def postCheck(ctx: Context): Response
}

object Ctrl {
  val empty: Ctrl = EmptyCtrl
}

case object EmptyCtrl extends Ctrl {
  override def selfCheck(ctx: Context): Response = Response.empty

  override def postCheck(ctx: Context): Response = Response.empty
}
