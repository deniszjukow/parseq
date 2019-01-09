package org.zewx.parseq

import cats.instances.map._
import cats.instances.string._
import cats.instances.tuple._
import cats.instances.int._

case class TreeValidator(factory: Factory) {

  def enrich(tree: Tree[Elem]): Tree[Context] = {

    NTree.nTreeMonoid[ParSeq, Id, Elem]

    NTree.map(tree.zipWithChildren.zipWithPath) {
      case (path, (children, (name, data))) => Context(path, name, data, children)
    }
  }

  def apply(tree: Tree[Elem]): Response = {
    def go(t: Tree[Context]): Response = t match {
      case NBranch(_, _, ctx, nodes) =>
        val ctrl = factory(ctx.ctrlName)
        ctrl.selfCheck(ctx)
          .orElse(nodes.foldLeft(Response.empty) { case (acc, node) =>
            if (!acc.isEmpty) acc else go(node)
          })
          .orElse(ctrl.postCheck(ctx))
      case NLeaf(_, ctx) =>
        val ctrl = factory(ctx.ctrlName)
        ctrl.selfCheck(ctx).orElse(ctrl.postCheck(ctx))
      case NEmpty =>
        Response.empty
    }

    go(enrich(tree))
  }
}
