package com.gutiory.bindeserializer.algebras

import com.gutiory.bindeserializer.models.Message

abstract class Writers[F[_]] {

  def deserialize(msg: Message) : F[String]

}
