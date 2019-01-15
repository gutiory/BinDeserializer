package com.gutiory.bindeserializer.implicits

import cats.MonadError
import com.gutiory.bindeserializer.algebras.{Fields, Nodes}
import com.gutiory.bindeserializer.interpreters.{FieldsInterpreter, NodesInterpreter}

object runtime {

  type DeserializerMonadError[F[_]] = MonadError[F, Throwable]

  implicit def nodes[F[_]: DeserializerMonadError]: Nodes[F] = new NodesInterpreter[F](Right("OneMessage.xml"))

  implicit def fields[F[_]: DeserializerMonadError]: Fields[F] = new FieldsInterpreter[F]

}
