package com.gutiory.bindeserializer.algebras

import com.gutiory.bindeserializer.models.{Basic, Enum, Message, Simple, XMLArray}

import scala.xml.Node

abstract class Messages[F[_]] {

  def deserialize(messageList: List[Message], basicDataMap: Map[String, Basic], simpleDataMap: Map[String, Simple],
                  enumeratedDataMap: Map[String, Enum], arrayDataMap: Map[String, XMLArray],
                  byteArray:Array[Byte]): F[String]

  def parseXMLFile(xmlFile: String) : F[List[Message]]

  def parseMessage(node: Node): F[Option[Message]]

}
