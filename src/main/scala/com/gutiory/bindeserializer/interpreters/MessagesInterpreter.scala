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




//import java.nio.ByteBuffer
//
//import scala.xml.{Node, NodeSeq, XML}
//import com.gutiory.bindeserializer.algebras.Messages
//import com.gutiory.bindeserializer.models._
//import cats.Applicative
//import cats.implicits._
//import com.gutiory.bindeserializer.implicits.runtime._
//
//import scala.collection.immutable
//
//class MessagesInterpreter[F[_]:Applicative] extends Messages[F]{
//  override def deserialize(messageList: List[Message], basicDataMap: Map[String, Basic], simpleDataMap: Map[String, Simple],
//                           enumeratedDataMap: Map[String, Enum], arrayDataMap: Map[String, XMLArray],
//                           byteArray:Array[Byte]): F[String] = {
//
//
//    /*messageList.map(msg => deserializeMsgLoop(msg.fields, 0))
//
//    def deserializeMsgLoop(fieldList: List[XMLField], output: String, sizeAcc: Int) : String = {
//      fieldList match {
//        case x::xs =>
//      }
//    }
//    */
//
//    def getStructFieldSize(field: StructField, acum: Either[String, Int]) : Either[String, Int] = {
//      field.dataType match {
//        case Some(dataType) =>
//          val size = if (basicDataMap.get(dataType).isDefined) getSize(basicDataMap(dataType))
//          else if (simpleDataMap.get(dataType).isDefined) getSize(simpleDataMap(dataType))
//          else if (enumeratedDataMap.get(dataType).isDefined) getSize(enumeratedDataMap(dataType))
//          else if (arrayDataMap.get(dataType).isDefined) getSize(arrayDataMap(dataType))
//          else Left("Data type not found")
//          size match {
//            case Right(value) => acum match {
//              case Right(acumR) => Right(value + acumR)
//              case Left(msg) => Left(msg)
//            }
//            case Left(msg) => Left(msg)
//          }
//      }
//    }
//
//    def getField(dataType:String) : Option[XMLField] = {
//      if (basicDataMap.get(dataType).isDefined) basicDataMap.get(dataType)
//      else if (simpleDataMap.get(dataType).isDefined) simpleDataMap.get(dataType)
//      else if (enumeratedDataMap.get(dataType).isDefined) enumeratedDataMap.get(dataType)
//      else if (arrayDataMap.get(dataType).isDefined) arrayDataMap.get(dataType)
//      else None
//    }
//
//
//    def getSize(field: XMLField) : Either[String, Int] = {
//      field match {
//        case b: Basic => Right(b.numBits.getOrElse(4)) // Int by default
//        case s: Simple => s.representation match {
//          case Some(repr) => basicDataMap.get(repr) match {
//            case Some(basic) => Right(basic.numBits.getOrElse(4))
//            case None => Left("Basic data not found")
//          }
//          case None => Left("Representation not found")
//        }
//        //case s: Struct => s.fields.flatten.foldRight(Either("",0))(getStructFieldSize _)
//        case e: EnumList => e.representation match {
//          case Some(repr) =>
//            if (basicDataMap.get(repr).isDefined) getSize(basicDataMap(repr))
//            else if (simpleDataMap.get(repr).isDefined) getSize(simpleDataMap(repr))
//            else Left("Representation not found")
//          case None => Left("Representation not found")
//        }
//        case a: XMLArray => a.dataType match {
//          case Some(dataType) => getField(dataType) match {
//            case Some(arrayField) =>  getSize(arrayField) match {
//              case Right(arraySize) => Right(a.cardinality * arraySize)
//              case Left(_) => Left("Array size not found")
//            }
//            case None => Left("Data Type field not found")
//          }
//          case None => Left("Data type not found")
//        }
//        case _ => Left("Data type not found")
//      }
//    }
//    "TBD".pure[F]
//  }
//
//  override def parseXMLFile(xmlFile: String): F[List[Message]] = {
//    val doc = XML.load(xmlFile)
//    val basicData: NodeSeq = doc \\ "basicData"
//    val simpleData: NodeSeq = doc \\ "simpleData"
//    val enumeratedData = doc \\ "enumeratedData"
//    val arrayData = doc \\ "arrayData"
//    val structData = doc \\ "structData"
//    val messageData: NodeSeq = doc \\ "messageData"
//    val messageFieldData = doc \\ "messageData" \\ "field"
//
//    val messageFieldMap = messageFieldData.flatMap(fields[Option].parse).flatten.map(field => field.name -> field).toMap
//    val basicDataMap = basicData.flatMap(fields[Option].parse).flatten.map(field => field.name -> field).toMap
//    val simpleDataMap = simpleData.flatMap(fields[Option].parse).flatten.map(field => field.name -> field).toMap
//    val structDataMap = structData.flatMap(fields[Option].parse).flatten.map(field => field.name -> field).toMap
//    val enumeratedDataMap = enumeratedData.flatMap(fields[Option].parse).flatten.map(field => field.name -> field).toMap
//    val arrayDataMap = arrayData.flatMap(fields[Option].parse).flatten.map(field => field.name -> field).toMap
//
//    def parseMessageFields(field: XMLField, message: List[XMLField]) : List[XMLField] = field match {
//      case b: Basic => b :: message
//      case s: Simple => s :: message
//      case s: Struct => s.fields.flatten.flatMap(f => parseMessageFields(f, message))
//      case sf: StructField => sf.dataType match {
//        case Some(dataType) =>
//          if (basicDataMap.get(dataType).isDefined) parseMessageFields(basicDataMap(dataType), message)
//          else if (simpleDataMap.get(dataType).isDefined) parseMessageFields(simpleDataMap(dataType), message)
//          else if (enumeratedDataMap.get(dataType).isDefined) parseMessageFields(enumeratedDataMap(dataType), message)
//          else message
//        case None => message
//      }
//      case a: XMLArray => a :: message
//      case e: EnumList => e :: message
//      case _ => message
//    }
//
//    val res: immutable.Seq[Option[Message]] = messageData.map { msgNode =>
//      parseMsg(msgNode) match {
//        case Some(msg) =>
//          val fieldList: NodeSeq = msgNode \\ "field"
//          val msgFieldList: immutable.Seq[List[XMLField]] = fieldList.map(fl => nodeTextStr("dataType", fl) match {
//            case Some(dataType) =>
//              if (basicDataMap.get(dataType).isDefined) parseMessageFields(basicDataMap(dataType), Nil)
//              else if (simpleDataMap.get(dataType).isDefined) parseMessageFields(simpleDataMap(dataType), Nil)
//              else if (enumeratedDataMap.get(dataType).isDefined) parseMessageFields(enumeratedDataMap(dataType), Nil)
//              else if (arrayDataMap.get(dataType).isDefined) parseMessageFields(arrayDataMap(dataType), Nil)
//              else Nil
//            case None => Nil
//          })
//          println("msgFieldList ==> " + msg.id + " ==> " + msgFieldList.flatten)
//          Option(Message(msg.name, msg.id, msgFieldList.flatten.toList))
//        case None => None
//      }
//
//    }
//    val messageDD = messageData.flatMap(parseMsg)
//    res.flatten.toList.pure[F]
//  }
//
//  def parseMsg(node: Node) : Option[Message] = {
//    node.label match {
//      case "messageData" => nodeTextStr("name", node) match {
//        case Some(name) => nodeTextStr("id", node) match {
//          case Some(id) => Some(Message(name, id, List()))
//          case None => None
//        }
//        case None => None
//      }
//      case _ => None
//    }
//  }
//
//  override def parseMessage(node: Node): F[Option[Message]] = {
//    val res = node.label match {
//      case "messageData" => nodeTextStr("name", node) match {
//        case Some(name) => nodeTextStr("id", node) match {
//          case Some(id) => Some(Message(name, id, List()))
//          case None => None
//        }
//        case None => None
//      }
//      case _ => None
//    }
//    res.pure[F]
//  }
//
//  def nodeTextStr(key: String, node: Node) : Option[String] = {
//    node.attribute(key).map(_.text)
//  }
//
//  override def byteToString(bytes: Array[Byte], size: Int, simpleField: Simple): Either[String, String] = {
//    val subBytes = bytes.slice(0,size)
//    simpleField.name match {
//      case "Char" => Right(ByteBuffer.wrap(subBytes).getChar.toString)
//      case "Int" => Right(ByteBuffer.wrap(subBytes).getInt.toString)
//      case "Unsigned_int" => Right(ByteBuffer.wrap(subBytes).getInt & 0xffff toString)
//      case "Unsigned_short" => Right(ByteBuffer.wrap(subBytes).getShort & 0xff toString)
//      case "Unsigned_char" => Right(ByteBuffer.wrap(subBytes).getChar.toString)
//      case "Bool" => Right((ByteBuffer.wrap(subBytes) == 0).toString)
//      case "Enum" => Right(ByteBuffer.wrap(subBytes).getInt.toString)
//      case "Float" => Right(ByteBuffer.wrap(subBytes).getFloat.toString)
//      case "Double" => Right(ByteBuffer.wrap(subBytes).getDouble.toString)
//      case _ => Left("Basic data type not supported")
//    }
//  }
//
//
//}
