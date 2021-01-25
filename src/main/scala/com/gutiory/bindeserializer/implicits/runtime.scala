package com.gutiory.bindeserializer.implicits

import cats.MonadError
import com.gutiory.bindeserializer.algebras.Nodes
import com.gutiory.bindeserializer.interpreters.NodesInterpreter
import com.gutiory.bindeserializer.models.XMLFile

object runtime {

  implicit val xmlFile: String = "OneMessage.xml"
  type DeserializerMonadError[F[_]] = MonadError[F, Throwable]

  implicit def nodes[F[_]: DeserializerMonadError](implicit xmlFile: XMLFile) : Nodes[F] = new NodesInterpreter[F](Right(xmlFile.file))

}
