package com.gutiory.bindeserializer.interpreters

import cats.Applicative
import cats.implicits._
import com.gutiory.bindeserializer.algebras.Nodes

import scala.xml.{Elem, Node, XML}

class NodesInterpreter[F[_]: Applicative](xmlSource: Either[String, String]) extends Nodes[F] {

  val elem: F[Elem] = xmlSource match {
    case Right(filename) => XML.load(filename).pure[F]
    case Left(text) => XML.loadString(text).pure[F]
  }

  override def items(label: String): F[List[Node]] =
    elem.map(e => (e \\ label).toList)

  override def itemsWithChildren(label: String, children: String): F[List[(Node, List[Node])]] =
    elem.map(e => (e \\ label).toList.map(n => (n, (n \ children).toList)))

  override def subItems(parent: String, children: String): F[List[Node]] =
    elem.map(e => (e \\ parent \\ children).toList)

}

