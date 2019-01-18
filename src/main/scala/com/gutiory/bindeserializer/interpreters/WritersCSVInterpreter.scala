package com.gutiory.bindeserializer.interpreters

import cats.Applicative
import cats.implicits._
import com.gutiory.bindeserializer.algebras.Writers
import com.gutiory.bindeserializer.models._

class WritersCSVInterpreter[F[_]: Applicative] extends Writers[F] {

  override def deserialize(msg: Message) : F[String] = s"${msg.toString}: size ${msg.size}".pure[F]

}

