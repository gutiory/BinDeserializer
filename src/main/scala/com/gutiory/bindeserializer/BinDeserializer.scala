package com.gutiory.bindeserializer

import cats.effect.IO
import com.gutiory.bindeserializer.interpreters.{MessagesInterpreter, NodesInterpreter, WritersCSVInterpreter}
import com.gutiory.bindeserializer.models.{Message, XMLFile}
import implicits.runtime._
import cats.implicits._

object BinDeserializer extends App {

  implicit val xmlFile: XMLFile = XMLFile("OneMessage.xml")
  val messagesParser = new MessagesInterpreter[IO]
  val writer = new WritersCSVInterpreter[IO]
  val bytes = Array[Byte](0,0,0,1)
  val result = for {
    messages <- messagesParser.parse
    messagesPlusArray = messages.map(m => (m, bytes))
    formatted <- messagesPlusArray.traverse(msgArray => writer.deserialize(msgArray._1, msgArray._2))
  } yield formatted

  println(result.unsafeRunSync())

}

