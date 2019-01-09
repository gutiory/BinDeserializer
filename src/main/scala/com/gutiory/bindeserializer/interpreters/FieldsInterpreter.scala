package com.gutiory.bindeserializer.interpreters

import cats.Applicative
import cats.implicits._
import com.gutiory.bindeserializer.algebras.Fields
import com.gutiory.bindeserializer.implicits.runtime.fields
import com.gutiory.bindeserializer.models._

import scala.xml.{Node, NodeSeq, XML}

class FieldsInterpreter[F[_]: Applicative] extends Fields[F]{
  override def parse(node: Node): F[Option[XMLField]] = parseNode(node).pure[F]

  def parseNode(node: Node): Option[XMLField] = node.label match {
    case "basicData" => nodeTextStr("name", node) match {
      case Some(name) =>  Option(Basic(name, nodeTextInt("size", node)))
      case None => None
    }
    case "simpleData" => nodeTextStr("name" ,node) match {
      case Some(name) => nodeTextStr("representation", node) match {
        case Some(representation) => Option(Simple(name, Option(representation)))
        case None => None
      }
      case None => None
    }
    case "enumerator" => nodeTextStr("name", node) match {
        case Some(name) => Option(Enum(name, nodeTextInt("value", node)))
        case None => None
      }
    case "enumeratedData" => nodeTextStr("name", node) match {
      case Some(name) => nodeTextStr("representation", node) match {
        case Some(representation) => Option(EnumList(name, Option(representation), (node \ "enumerator").map(parseNode).toList))
        case None => None
      }
      case None => None
    }
    case "arrayData" => nodeTextStr("name", node) match {
      case Some(name) => nodeTextInt("cardinality", node) match {
        case Some(size) => Option(XMLArray(name, nodeTextStr("dataType", node), size,
          node.attribute("countName").map{_.text}))
        case None => None
      }
      case None => None
    }
    case "structData" => nodeTextStr("name", node) match {
      case Some(name) =>
        Some(Struct(name, (node \ "structField").map(parseStructField).toList))
      case None => None
    }
    case "structField" => nodeTextStr("name", node) match {
      case Some(name) => Option(StructField(name, nodeTextStr("dataType", node)))
      case None => None
    }
    case "field" => nodeTextStr("name", node) match {
      case Some(name) => Option(MessageField(name, nodeTextStr("dataType", node)))
      case None => None

    }
    case _ => None//println("_ ==> " + node.label);None
  }

  def parseStructField(node: Node) : Option[StructField] =  node.label match {
    case "structField" => nodeTextStr("name", node) match {
      case Some(name) => Option(StructField(name, nodeTextStr("dataType", node)))
      case None => None
    }

    case _ => None
  }

  def nodeTextStr(key: String, node: Node) : Option[String] = {
    node.attribute(key).map(_.text)
  }

  def nodeTextInt(key: String, node: Node) : Option[Int] = {
    node.attribute(key).map(_.text.toInt)
  }
}

