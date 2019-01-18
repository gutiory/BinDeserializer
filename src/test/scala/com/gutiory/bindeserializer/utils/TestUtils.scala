package com.gutiory.bindeserializer.utils

import cats.MonadError
import cats.effect.IO
import com.gutiory.bindeserializer.algebras.{Messages, Nodes}
import com.gutiory.bindeserializer.interpreters.{MessagesInterpreter, NodesInterpreter, WritersCSVInterpreter}
import com.gutiory.bindeserializer.models._

trait TestUtils {

  type TestTargetMonad[A] = IO[A]
  type TestMonadError[F[_]] = MonadError[TestTargetMonad, Throwable]

  def msgInterpreter(xml: String): Messages[TestTargetMonad] = {
    implicit def nodes: Nodes[TestTargetMonad] = new NodesInterpreter[TestTargetMonad](Left(xml))
    new MessagesInterpreter[TestTargetMonad]
  }

  val writerCSVInterpreter = new WritersCSVInterpreter[TestTargetMonad]

  val msg1 = Message(
    name = "Message1",
    id = "ID_MESSAGE1",
    fields = List(
      MessageField(
        name = "field1",
        dataType = SimpleDT(
          name = "Int",
          representation = Representation(
            name = "Signed32",
            s = 32
          )
        )
      ),
      MessageField(
        name = "enumField1",
        dataType = EnumeratorDT(
          name = "EnumName1",
          values = List(
            EnumValue(name = "ENUM0", value = 0),
            EnumValue(name = "ENUM1", value = 1),
            EnumValue(name = "ENUM2", value = 2),
            EnumValue(name = "ENUM3", value = 3),
            EnumValue(name = "ENUM4", value = 4),
            EnumValue(name = "ENUM5", value = 5)
          )
        )
      ),
      MessageField(
        name = "struct1",
        dataType = StructDT(
          name = "StructData1",
          fields = List(
            StructField(
              name = "simpleField1",
              dataType = SimpleDT(
                name = "Int",
                representation = Representation(
                  name = "Signed32",
                  s = 32)
              )
            )
          )
        )
      ),
      MessageField(
        name = "array1",
        dataType = ArrayDT(
          name = "Array1",
          dataType = SimpleDT(
            name = "Char",
            representation = Representation(
              name = "Signed8",
              s = 8)
          ),
          cardinality = 8
        )
      )
    )
  )

}
