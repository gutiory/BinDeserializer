package com.gutiory.bindeserializer.interpreters

import com.gutiory.bindeserializer.algebras.Messages
import com.gutiory.bindeserializer.models.Message
import com.gutiory.bindeserializer.implicits.runtime._
import cats.syntax.flatMap._
import cats.syntax.functor._


import scala.xml.{Node, XML}

class MessagesInterpreter[F[_]] extends Messages[F]{
  override def deserialize(byte: Byte): F[Message] = ???

  override def parseXMLFile(xmlFile: String): F[List[Message]] = {
    val doc = XML.load("Topics_TrackServer_TACT.xml")
    val basicData = doc \\ "basicData"
    val enumeratedData = doc \\ "enumeratedData"
    val arrayData = doc \\ "arrayData"
    val structData = doc \\ "structData"
    val messageDataTypes = doc \\ "messageDataTypes"
    val messageData = doc \ "messageData"
    val fieldList = doc \\ "field"

    val basicMap = basicData.flatMap(fields.parse).map(field => field.name -> field).toMap
    val enumMap = enumeratedData.flatMap(fields.parse).map(field => field.name -> field).toMap
    val arrayMap = arrayData.flatMap(fields.parse).map(field => field.name -> field).toMap
    val structMap = structData.flatMap(fields.parse).map(field => field.name -> field).toMap
    val structFlatMap = structData.flatMap(fields.parse).map(field => field.name -> field).toMap


    import cats.implicits._

    messageData.map(parseMessage).toList.sequence

  }

    override def parseMessage(node: Node): F[Message] = ???
}
