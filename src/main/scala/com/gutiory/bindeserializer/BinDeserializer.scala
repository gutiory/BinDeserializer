package com.gutiory.bindeserializer

import cats.effect.IO
import com.gutiory.bindeserializer.interpreters.MessagesInterpreter
import implicits.runtime._
import cats.implicits._

object BinDeserializer extends App {

  val messagesParser = new MessagesInterpreter[IO]

  val result = for {
    messages <- messagesParser.parse
    formatted <- messages.traverse(messagesParser.beautifier)
  } yield formatted

  println(result.unsafeRunSync())

}

