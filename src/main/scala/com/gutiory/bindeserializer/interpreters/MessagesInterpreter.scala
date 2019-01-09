package com.gutiory.bindeserializer.interpreters

import scala.xml.{Node, NodeSeq, XML}
import com.gutiory.bindeserializer.algebras.Messages
import com.gutiory.bindeserializer.models._
import cats.Applicative
import cats.implicits._
import com.gutiory.bindeserializer.implicits.runtime._

import scala.collection.immutable

class MessagesInterpreter[F[_]:Applicative] extends Messages[F]{
  override def deserialize(messageList: List[Message], basicDataMap: Map[String, Basic], simpleDataMap: Map[String, Simple],
                           enumeratedDataMap: Map[String, Enum], arrayDataMap: Map[String, XMLArray],
                           byteArray:Array[Byte]): F[String] = {

    def deserializeMsg(fieldList: List[XMLField], sizeAcc: Int) : String = ???

    def getStructFieldSize(field: StructField, acum: Int) : Int = {
      field.dataType match {
        case Some(dataType) =>
          val size = if (basicDataMap.get(dataType).isDefined) getSize(basicDataMap(dataType))
          else if (simpleDataMap.get(dataType).isDefined) getSize(simpleDataMap(dataType))
          else if (enumeratedDataMap.get(dataType).isDefined) getSize(enumeratedDataMap(dataType))
          else if (arrayDataMap.get(dataType).isDefined) getSize(arrayDataMap(dataType))
          else 0
          size + acum
      }
    }

    def getField(dataType:String) : Option[XMLField] = {
      if (basicDataMap.get(dataType).isDefined) basicDataMap.get(dataType)
      else if (simpleDataMap.get(dataType).isDefined) simpleDataMap.get(dataType)
      else if (enumeratedDataMap.get(dataType).isDefined) enumeratedDataMap.get(dataType)
      else if (arrayDataMap.get(dataType).isDefined) arrayDataMap.get(dataType)
      else None
    }


    def getSize(field: XMLField) : Int = {
      field match {
        case b: Basic => b.numBits.getOrElse(4) // Int by default
        case s: Simple => s.representation match {
          case Some(repr) => basicDataMap.get(repr) match {
            case Some(basic) => basic.numBits.getOrElse(4)
            case None => 0
          }
          case None => 0
        }
        case s: Struct => s.fields.flatten.foldRight(0)(getStructFieldSize)
        case e: EnumList => e.representation match {
          case Some(repr) =>
            if (basicDataMap.get(repr).isDefined) getSize(basicDataMap(repr))
            else if (simpleDataMap.get(repr).isDefined) getSize(simpleDataMap(repr))
            else 0
          case None => 0
        }
        case a: XMLArray => a.dataType match {
          case Some(dataType) => getField(dataType) match {
            case Some(arrayField) => a.cardinality * getSize(arrayField)
            case None => 0
          }
          case None => 0
        }
      }
    }
    "TBD".pure[F]
  }

  override def parseXMLFile(xmlFile: String): F[List[Message]] = {
    val doc = XML.load(xmlFile)
    val basicData: NodeSeq = doc \\ "basicData"
    val simpleData: NodeSeq = doc \\ "simpleData"
    val enumeratedData = doc \\ "enumeratedData"
    val arrayData = doc \\ "arrayData"
    val structData = doc \\ "structData"
    val messageData: NodeSeq = doc \\ "messageData"
    val messageFieldData = doc \\ "messageData" \\ "field"

    val messageFieldMap = messageFieldData.flatMap(fields[Option].parse).flatten.map(field => field.name -> field).toMap
    val basicDataMap = basicData.flatMap(fields[Option].parse).flatten.map(field => field.name -> field).toMap
    val simpleDataMap = simpleData.flatMap(fields[Option].parse).flatten.map(field => field.name -> field).toMap
    val structDataMap = structData.flatMap(fields[Option].parse).flatten.map(field => field.name -> field).toMap
    val enumeratedDataMap = enumeratedData.flatMap(fields[Option].parse).flatten.map(field => field.name -> field).toMap
    val arrayDataMap = arrayData.flatMap(fields[Option].parse).flatten.map(field => field.name -> field).toMap

    def parseMessageFields(field: XMLField, message: List[XMLField]) : List[XMLField] = field match {
      case b: Basic => b :: message
      case s: Simple => s.representation match {
        case Some(representation) => basicDataMap.get(representation) match {
          case Some(b) => parseMessageFields(b, message)
          case None => message
        }
        case None => message
      }
      case s: Struct => s.fields.flatten.flatMap(f => parseMessageFields(f, message))
      case sf: StructField => sf.dataType match {
        case Some(dataType) =>
          if (basicDataMap.get(dataType).isDefined) parseMessageFields(basicDataMap(dataType), message)
          else if (simpleDataMap.get(dataType).isDefined) parseMessageFields(simpleDataMap(dataType), message)
          else if (enumeratedDataMap.get(dataType).isDefined) parseMessageFields(enumeratedDataMap(dataType), message)
          else message
        case None => message
      }
      case a: XMLArray => a :: message
      case e: EnumList => e :: message
      case _ => message
    }

    val res: immutable.Seq[Option[Message]] = messageData.map { msgNode =>
      parseMsg(msgNode) match {
        case Some(msg) =>
          val fieldList: NodeSeq = msgNode \\ "field"
          val msgFieldList: immutable.Seq[List[XMLField]] = fieldList.map(fl => nodeTextStr("dataType", fl) match {
            case Some(dataType) =>
              if (basicDataMap.get(dataType).isDefined) parseMessageFields(basicDataMap(dataType), Nil)
              else if (simpleDataMap.get(dataType).isDefined) parseMessageFields(simpleDataMap(dataType), Nil)
              else if (enumeratedDataMap.get(dataType).isDefined) parseMessageFields(enumeratedDataMap(dataType), Nil)
              else if (arrayDataMap.get(dataType).isDefined) parseMessageFields(arrayDataMap(dataType), Nil)
              else Nil
            case None => Nil
          })
          println("msgFieldList ==> " + msg.id + " ==> " + msgFieldList.flatten)
          Option(Message(msg.name, msg.id, msgFieldList.flatten.toList))
        case None => None
      }

    }
    val messageDD = messageData.flatMap(parseMsg)
    println(messageFieldMap)
    println(messageDD)

    println(res)
    res.flatten.toList.pure[F]
  }

  def parseMsg(node: Node) : Option[Message] = {
    node.label match {
      case "messageData" => nodeTextStr("name", node) match {
        case Some(name) => nodeTextStr("id", node) match {
          case Some(id) => Some(Message(name, id, List()))
          case None => None
        }
        case None => None
      }
      case _ => None
    }
  }

  override def parseMessage(node: Node): F[Option[Message]] = {
    val res = node.label match {
      case "messageData" => nodeTextStr("name", node) match {
        case Some(name) => nodeTextStr("id", node) match {
          case Some(id) => Some(Message(name, id, List()))
          case None => None
        }
        case None => None
      }
      case _ => None
    }
    res.pure[F]
  }

  def nodeTextStr(key: String, node: Node) : Option[String] = {
    node.attribute(key).map(_.text)
  }

}
