package com.gutiory.bindeserializer

import scala.xml.XML
import implicits.runtime.{fields, _}
import cats.effect.IO

object BinDeserializer extends App {
  val doc = XML.load("Example.xml")
  val messageList = doc \ "messages"
  val basicData = doc \\ "basicData"
  val enumeratedData = doc \\ "enumeratedData"
  val arrayData = doc \\ "arrayData"
  val structData = doc \\ "structData"
  val messageDataTypes = doc \\ "messageDataTypes"
  val fieldList = doc \\ "field"

  println("messageList==============")
  println(messageList)
  println("basicData===============")
  println(basicData)
  println("messageDataTypes ============ ")
  println(messageDataTypes)
  println("enumeratedData ====================")
  println(enumeratedData)


  val basicMap = basicData.flatMap(fields.parse).map(field => field.name -> field).toMap
  println(basicMap)

  val enumMap = enumeratedData.map(fields.parse).filter(_ != None).map {
    case Some(field) => field.name -> field
  }.toMap
  println(enumMap)

  val arrayMap = arrayData.map(fields.parse).filter(_ != None).map {
    case Some(field) => field.name -> field
  }.toMap
  println(arrayMap)

  val structMap = structData.map(fields.parse).filter(_ != None).map {
    case Some(field) => field.name -> field
  }.toMap
  println(structMap)

  val structFlatMap = structData.flatMap(fields.parse).map(field => field.name -> field).toMap
  println(structFlatMap)
    //workflow.parseXML("NAV_STA_Messages.xml")

}

