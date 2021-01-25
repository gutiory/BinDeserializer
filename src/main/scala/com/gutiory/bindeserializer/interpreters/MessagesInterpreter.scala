package com.gutiory.bindeserializer.interpreters

import com.gutiory.bindeserializer.algebras.{Messages, Nodes}
import com.gutiory.bindeserializer.models._
import com.gutiory.bindeserializer.implicits.runtime._
import cats.implicits._

import scala.util.Try
import scala.xml.Node

class MessagesInterpreter[F[_]](implicit nodes: Nodes[F], F: DeserializerMonadError[F]) extends Messages[F]{

  override def parse: F[List[Message]] = for {
    repreNodes <- nodes.items("basicData")
    representations <- repreNodes.traverse(parseRepresentation)
    simpleNodes <- nodes.items("simpleData")
    simpleDTs <- simpleNodes.traverse(n => parseSimpleDT(n, representations))
    enumNodes <- nodes.itemsWithChildren("enumeratedData", "enumerator")
    enumDTs <- enumNodes.traverse(n => (parseEnumDT _).tupled(n))
    arrayNodes <- nodes.items("arrayData")
    arrayDTs <- arrayNodes.traverse(n => parseArrayDT(n, simpleDTs))
    structNodes <- nodes.itemsWithChildren("structData", "structField")
    structDTs <- structNodes.traverse(n => parseStructDT(n._1, n._2, simpleDTs))
    messageNodes <- nodes.itemsWithChildren("messageData", "field")
    messages <- messageNodes.traverse(n => parseMessageDT(n._1, n._2, simpleDTs ++ enumDTs ++ arrayDTs ++ structDTs))
  } yield messages

  override def parseRepresentation(node: Node): F[Representation] = node.label match {
    case "basicData" => toRepresentation(node)
    case _ => F.raiseError(UnknownNodeError)
  }

  override def parseSimpleDT(node: Node, representations: List[Representation]): F[DT] = node.label match {
    case "simpleData" => toSimple(node, representations)
    case _ => F.raiseError(UnknownNodeError)
  }

  override def parseEnumDT(node: Node, values: List[Node]): F[DT] = node.label match {
    case "enumeratedData" => toEnumerator(node, values)
    case _ => F.raiseError(UnknownNodeError)
  }

  override def parseArrayDT(node: Node, simples: List[DT]): F[DT] = node.label match {
    case "arrayData" => toArray(node, simples)
    case _ => F.raiseError(UnknownNodeError)
  }

  override def parseStructDT(node: Node, fields: List[Node], simples: List[DT]): F[DT] = node.label match {
    case "structData" => toStruct(node, fields, simples)
    case _ => F.raiseError(UnknownNodeError)
  }

  override def parseMessageDT(node: Node, fields: List[Node], types: List[DT]): F[Message] = node.label match {
    case "messageData" => toMessage(node, fields, types)
    case _ => F.raiseError(UnknownNodeError)
  }

  private def toRepresentation(node: Node): F[Representation] = for {
    nameKey <- node.getString("name")
    name <- F.fromOption(AllowedValues.representationNameList.find(_ == nameKey), InvalidRepresentationNameError(nameKey))
    size <- node.getInt("size")
  } yield Representation(name, size)

  private def toSimple(node: Node, representations: List[Representation]): F[DT] = for {
    nameKey <- node.getString("name")
    name <- F.fromOption(AllowedValues.simpleNameList.find(_ == nameKey), InvalidSimpleNameError(nameKey))
    representationKey <- node.getString("representation")
    representation <- F.fromOption(representations.find(_.name == representationKey), RepresentationNotFoundError(representationKey))
  } yield SimpleDT(name, representation)

  private def toEnumValue(node: Node): F[EnumValue] = for {
    name <- node.getString("name")
    value <- node.getInt("value")
  } yield EnumValue(name, value)

  private def toEnumerator(node: Node, values: List[Node]): F[DT] = for {
    name <- node.getString("name")
    vs <- values.traverse(toEnumValue)
  } yield EnumeratorDT(name, vs)

  private def toArray(node: Node, simples: List[DT]): F[DT] = for {
    name <- node.getString("name")
    typeKey <- node.getString("dataType")
    simpleTypes = simples.collect{ case b: SimpleDT => b }
    dataType <- F.fromOption(simpleTypes.find(_.name == typeKey), DataTypeNotFoundError(typeKey))
    cardinality <- node.getInt("cardinality")
  } yield ArrayDT(name, dataType, cardinality)

  private def toStructField(node: Node, simples: List[DT]): F[StructField] = for {
    name <- node.getString("name")
    typeKey <- node.getString("dataType")
    simpleTypes = simples.collect{ case b: SimpleDT => b }
    dataType <- F.fromOption(simpleTypes.find(_.name == typeKey), DataTypeNotFoundError(typeKey))
  } yield StructField(name, dataType)

  private def toStruct(node: Node, fields: List[Node], simples: List[DT]): F[DT] = for {
    name <- node.getString("name")
    fs <- fields.traverse(f => toStructField(f, simples))
  } yield StructDT(name, fs)

  private def toMessageField(node: Node, types: List[DT]): F[MessageField] = for {
    name <- node.getString("name")
    typeKey <- node.getString("dataType")
    ts = types.collect{
      case t: SimpleDT => t
      case t: EnumeratorDT => t
      case t: StructDT => t
      case t: ArrayDT => t
    }
    dataType <- F.fromOption(ts.find(_.name == typeKey), DataTypeNotFoundError(typeKey))
  } yield MessageField(name, dataType)

  private def toMessage(node: Node, fields: List[Node], types: List[DT]): F[Message] = for {
    name <- node.getString("name")
    id <- node.getString("id")
    fs <- fields.traverse(f => toMessageField(f, types))
  } yield Message(name, id, fs)

  implicit class NodeOps(self: Node)(implicit F: DeserializerMonadError[F]) {
    def getString(key: String): F[String] =
      F.fromOption(self.attribute(key).map(_.text), KeyNotFoundError(key))

    def getInt(key: String): F[Int] =
      F.fromOption(self.attribute(key).flatMap(g => Try(g.text.toInt).toOption), KeyNotFoundError(key))
  }

}