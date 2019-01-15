package com.gutiory.bindeserializer.utils

import cats.MonadError
import cats.effect.IO
import com.gutiory.bindeserializer.algebras.{Fields, Messages, Nodes}
import com.gutiory.bindeserializer.interpreters.{FieldsInterpreter, MessagesInterpreter, NodesInterpreter}

trait TestUtils {

  type TestTargetMonad[A] = IO[A]
  type TestMonadError[F[_]] = MonadError[TestTargetMonad, Throwable]

  def parser(xml: String): Messages[TestTargetMonad] = {
    implicit def nodes: Nodes[TestTargetMonad] = new NodesInterpreter[TestTargetMonad](Left(xml))
    implicit def fields: Fields[TestTargetMonad] = new FieldsInterpreter[TestTargetMonad]
    new MessagesInterpreter[TestTargetMonad]
  }

}
