package com.gutiory.bindeserializer.interpreters

import com.gutiory.bindeserializer.algebras.{Fields, Messages, Nodes}
import com.gutiory.bindeserializer.models._
import com.gutiory.bindeserializer.implicits.runtime._
import cats.implicits._

class MessagesInterpreter[F[_]:DeserializerMonadError](implicit nodes: Nodes[F], fields: Fields[F]) extends Messages[F]{

  override def parse: F[List[Message]] = for {
    repreNodes <- nodes.items("basicData")
    representations <- repreNodes.traverse(fields.parseRepresentation)
    simpleNodes <- nodes.items("simpleData")
    simpleDTs <- simpleNodes.traverse(n => fields.parseSimpleDT(n, representations))
    enumNodes <- nodes.itemsWithChildren("enumeratedData", "enumerator")
    enumDTs <- enumNodes.traverse(n => (fields.parseEnumDT _).tupled(n))
    arrayNodes <- nodes.items("arrayData")
    arrayDTs <- arrayNodes.traverse(n => fields.parseArrayDT(n, simpleDTs))
    structNodes <- nodes.itemsWithChildren("structData", "structField")
    structDTs <- structNodes.traverse(n => fields.parseStructDT(n._1, n._2, simpleDTs))
    messageNodes <- nodes.itemsWithChildren("messageData", "field")
    messages <- messageNodes.traverse(n => fields.parseMessageDT(n._1, n._2, simpleDTs ++ enumDTs ++ arrayDTs ++ structDTs))
  } yield messages

  override def beautifier(message: Message): F[String] = message.beauty(0).pure[F]

}
