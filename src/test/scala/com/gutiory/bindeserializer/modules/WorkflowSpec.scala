package com.gutiory.bindeserializer.modules

import cats.Id
import com.gutiory.bindeserializer.implicits.runtime.messages
import com.gutiory.bindeserializer.models._
import org.scalamock.scalatest.MockFactory
import org.scalatest._
import org.scalatest.prop.Checkers

class WorkflowSpec extends FlatSpec
  with Matchers
  with MockFactory
  with OneInstancePerTest
  with Checkers{

  val workflow: Workflow[Id] = Workflow.impl[Id](messages)

  val messageOne = Message("Message1", "ID_MESSAGE1", List(Basic("Signed32", Some(32)), EnumList("EnumName1", Some("Enum"), List(Some(Enum("ENUM0", Some(0))), Some(Enum("ENUM1", Some(1))), Some(Enum("ENUM2", Some(2))), Some(Enum("ENUM3", Some(3))), Some(Enum("ENUM4", Some(4))), Some(Enum("ENUM5", Some(5))))), XMLArray("Array1", Some("Char"), 8, None)))
  //val messageTwo = Message("Message2", "ID_MESSAGE2", List(Basic("Signed32", Some(32))))

  "Workflow" should "be able to parse a XML file" in {
    workflow.parseXML("OneMessage.xml") shouldBe List(messageOne)
  }


}
