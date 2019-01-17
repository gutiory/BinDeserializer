package com.gutiory.bindeserializer.implicits

import cats.MonadError
import com.gutiory.bindeserializer.algebras.Nodes
import com.gutiory.bindeserializer.interpreters.NodesInterpreter

object runtime {

  type DeserializerMonadError[F[_]] = MonadError[F, Throwable]

  implicit def nodes[F[_]: DeserializerMonadError]: Nodes[F] = new NodesInterpreter[F](Right("OneMessage.xml"))

}
