package com.gutiory.bindeserializer.algebras

import scala.xml.Node


abstract class Nodes[F[_]] {

  def items(label: String): F[List[Node]]

  def itemsWithChildren(label: String, children: String): F[List[(Node, List[Node])]]

  def subItems(parent: String, children: String): F[List[Node]]

}
