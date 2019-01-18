package com.gutiory.bindeserializer

import cats.effect.IO
import com.gutiory.bindeserializer.interpreters.{MessagesInterpreter, WritersCSVInterpreter}
import implicits.runtime._
import cats.implicits._

object BinDeserializer extends App {

  val messagesParser = new MessagesInterpreter[IO]
  val writer = new WritersCSVInterpreter[IO]

  val result = for {
    messages <- messagesParser.parse
    formatted <- messages.traverse(writer.deserialize)
  } yield formatted

  println(result.unsafeRunSync())

}

