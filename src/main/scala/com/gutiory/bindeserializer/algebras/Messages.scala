package com.gutiory.bindeserializer.algebras

import com.gutiory.bindeserializer.models.Message

abstract class Messages[F[_]] {

  def parse: F[List[Message]]

  def beautifier(message: Message): F[String]

  def byteToString(bytes: Array[Byte], size: Int, simpleField: Simple): Either[String, String]

}
