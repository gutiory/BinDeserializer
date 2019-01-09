package com.gutiory.bindeserializer.algebras

import com.gutiory.bindeserializer.models.XMLField

import scala.xml.Node

abstract class Fields[F[_]] {
  def parse(node: Node) : F[Option[XMLField]]

}
