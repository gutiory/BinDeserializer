package com.gutiory.bindeserializer.implicits

import cats._
import com.gutiory.bindeserializer.algebras.{Fields, Messages}
import com.gutiory.bindeserializer.interpreters.{FieldsInterpreter, MessagesInterpreter}
import com.gutiory.bindeserializer.modules.Workflow

object runtime {

  implicit def fields[F[_] : Applicative] : Fields[F] = new FieldsInterpreter[F]

  implicit def messages[F[_]: Applicative] : Messages[F] = new MessagesInterpreter[F]

  implicit def workflow[F[_]: Applicative](implicit M: Messages[F]) : Workflow[F] = Workflow.impl[F](M)
}
