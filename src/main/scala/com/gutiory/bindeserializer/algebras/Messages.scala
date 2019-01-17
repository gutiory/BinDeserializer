package com.gutiory.bindeserializer.algebras

import com.gutiory.bindeserializer.models.{DT, Message, Representation}

import scala.xml.Node

abstract class Messages[F[_]] {

  def parse: F[List[Message]]

  def parseRepresentation(node: Node) : F[Representation]

  def parseSimpleDT(node: Node, representations: List[Representation]): F[DT]

  def parseEnumDT(node: Node, values: List[Node]): F[DT]

  def parseArrayDT(node: Node, simples: List[DT]): F[DT]

  def parseStructDT(node: Node, fields: List[Node], simples: List[DT]): F[DT]

  def parseMessageDT(node: Node, fields: List[Node], types: List[DT]): F[Message]

}
