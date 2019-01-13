package io.parseq

case class Context(path: Path[Id], ctrlName: CtrlName, data: Data, children: Seq[Tree[Elem]])

trait Controller {
  def selfCheck(ctx: Context): Response
  def postCheck(ctx: Context): Response
}

object Controller {
  val empty: Controller = EmptyCtrl
}

case object EmptyCtrl extends Controller {
  override def selfCheck(ctx: Context): Response = Response.empty
  override def postCheck(ctx: Context): Response = Response.empty
}
