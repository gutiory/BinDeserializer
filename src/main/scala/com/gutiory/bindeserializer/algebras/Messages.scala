package com.gutiory.bindeserializer.algebras

import com.gutiory.bindeserializer.models.Message

import scala.xml.Node

abstract class Messages[F[_]] {

  def deserialize(byte: Byte) : F[Message]

  def parseXMLFile(xmlFile: String) : F[List[Message]]

  def parseMessage(node: Node): F[Message]
}
