package com.gutiory.bindeserializer.interpreters

import cats.Applicative
import cats.implicits._

import com.gutiory.bindeserializer.algebras.Fields
import com.gutiory.bindeserializer.models._

import scala.xml.Node

class FieldsInterpreter[F[+_]: Applicative] extends Fields[F]{
  override def parse(node: Node): F[Option[Field]] = node.label match {
    case "basicData" => nodeTextStr("name", node) match {
      case Some(name) =>  Some(Basic(name, nodeTextInt("size", node))).pure[F]
      case None => None.pure[F]
    }
    case "enumerator" =>
      nodeTextStr("name", node) match {
        case Some(name) => Some(Enum(name, nodeTextInt("value", node))).pure[F]
        case None => None.pure[F]
      }
    case "enumeratedData" => nodeTextStr("name", node) match {
      case Some(name) => Some(EnumList(name, (node \ "enumerator").map(parse).flatten.toList)).pure[F]
      case None => None.pure[F]
    }
    case "arrayData" => nodeTextStr("name", node) match {
      case Some(name) => nodeTextInt("cardinality", node) match {
        case Some(size) => Some(Array(name, nodeTextStr("dataType", node), size,
          node.attribute("countName").headOption.map{_.text})).pure[F]
        case None => None.pure[F]
      }
      case None => None.pure[F]
    }
    case "structData" => nodeTextStr("name", node) match {
      case Some(name) => Some(Struct(name, (node \ "structField").map(parse).flatten.toList)).pure[F]
      case None => None.pure[F]
    }
    case "structField" => nodeTextStr("name", node) match {
      case Some(name) => Some(StructField(name, nodeTextStr("dataType", node))).pure[F]
    }
    case _ => println("_");None.pure[F]
  }
  def nodeTextStr(key: String, node: Node) : Option[String] = {
    node.attribute(key).headOption.map(n => n.text)
  }

  def nodeTextInt(key: String, node: Node) : Option[Int] = {
    node.attribute(key).headOption.map(n => n.text.toInt)
  }
}
