package com.gutiory.bindeserializer

import com.gutiory.bindeserializer.models._
import com.gutiory.bindeserializer.utils.TestUtils
import org.scalatest._

class MessagesSpec extends FlatSpec with Matchers with TestUtils {


  "Messages" should "be able to parse the list of messages" in {

    val xml =
      """<dataTypes>
        | <basicDataRepresentations>
        |   <basicData name="Unsigned8" size="8" />
        |   <basicData name="Unsigned16" size="16" />
        |   <basicData name="Unsigned32" size="32" />
        |   <basicData name="Signed8" size="8" />
        |   <basicData name="Signed16" size="16" />
        |   <basicData name="Signed32" size="32" />
        |   <basicData name="Float32" size="32" />
        |   <basicData name="Float64" size="64" />
        | </basicDataRepresentations>
        | <simpleDataTypes>
        |   <simpleData name="Char" representation="Signed8" />
        |   <simpleData name="Int" representation="Signed32" />
        |   <simpleData name="Unsigned_int" representation="Unsigned32" />
        |   <simpleData name="Unsigned_short" representation="Unsigned16" />
        |   <simpleData name="Unsigned_char" representation="Unsigned8" />
        |   <simpleData name="Bool" representation="Unsigned8" />
        |   <simpleData name="Enum" representation="Signed32" />
        |   <simpleData name="Float" representation="Float32" />
        |   <simpleData name="Double" representation="Float64" />
        | </simpleDataTypes>
        | <enumeratedDataTypes>
        |   <enumeratedData name="EnumName1" representation="Enum" >
        |     <enumerator name="ENUM0"	value="0" />
        |     <enumerator name="ENUM1"	value="1" />
        |     <enumerator name="ENUM2"	value="2" />
        |     <enumerator name="ENUM3"	value="3" />
        |     <enumerator name="ENUM4"	value="4" />
        |     <enumerator name="ENUM5"	value="5" />
        |   </enumeratedData>
        | </enumeratedDataTypes>
        | <arrayDataTypes>
        |   <arrayData name="Array1" dataType="Char" cardinality="8" />
        | </arrayDataTypes>
        | <structDataTypes>
        |   <structData name="StructData1" >
        |     <structField name="simpleField1"	dataType="Int" />
        |   </structData>
        | </structDataTypes>
        | <messageDataTypes>
        |   <messageData name="Message1" id="ID_MESSAGE1" >
        |     <field name="field1"	dataType="Int"	word=""	numBits=""	sbp=""/>
        |     <field name="enumField1"	dataType="EnumName1"	word=""	numBits=""	sbp="" />
        |     <field name="struct1"	dataType="StructData1"	word=""	numBits=""	sbp="" />
        |     <field name="array1"	dataType="Array1"	word=""	numBits=""	sbp="" />
        |   </messageData>
        | </messageDataTypes>
        |</dataTypes>""".stripMargin

    val expected = List(msg1)

    msgInterpreter(xml).parse.unsafeRunSync() shouldBe expected
  }

  "Messages" should "fail when a representation in missing" in {

    val xml =
      """<dataTypes>
        | <basicDataRepresentations>
        |   <basicData name="Unsigned8" size="8" />
        | </basicDataRepresentations>
        | <simpleDataTypes>
        |   <simpleData name="Char" representation="WrongRepresentation" />
        | </simpleDataTypes>
        | <arrayDataTypes>
        |   <arrayData name="Array1" dataType="Char" cardinality="8" />
        | </arrayDataTypes>
        | <messageDataTypes>
        |   <messageData name="Message1" id="ID_MESSAGE1" >
        |     <field name="array1"	dataType="Array1"	word=""	numBits=""	sbp="" />
        |   </messageData>
        | </messageDataTypes>
        |</dataTypes>""".stripMargin

    val expected = Left(RepresentationNotFoundError("WrongRepresentation"))


    msgInterpreter(xml).parse.attempt.unsafeRunSync() shouldBe expected
  }

  "Messages" should "fail when a datatype in missing" in {

    val xml =
      """<dataTypes>
        | <basicDataRepresentations>
        |   <basicData name="Unsigned8" size="8" />
        | </basicDataRepresentations>
        | <simpleDataTypes>
        |   <simpleData name="Char" representation="Unsigned8" />
        | </simpleDataTypes>
        | <arrayDataTypes>
        |   <arrayData name="Array1" dataType="InvalidType" cardinality="8" />
        | </arrayDataTypes>
        | <messageDataTypes>
        |   <messageData name="Message1" id="ID_MESSAGE1" >
        |     <field name="array1"	dataType="Array1"	word=""	numBits=""	sbp="" />
        |   </messageData>
        | </messageDataTypes>
        |</dataTypes>""".stripMargin

    val expected = Left(DataTypeNotFoundError("InvalidType"))


    msgInterpreter(xml).parse.attempt.unsafeRunSync() shouldBe expected
  }

  "Messages" should "fail when a representation name is not in the list of valid representation names" in {

    val xml =
      """<dataTypes>
        | <basicDataRepresentations>
        |   <basicData name="BasicRepresentation" size="8" />
        | </basicDataRepresentations>
        | <simpleDataTypes>
        |   <simpleData name="Char" representation="BasicRepresentation" />
        | </simpleDataTypes>
        | <arrayDataTypes>
        |   <arrayData name="Array1" dataType="Char" cardinality="8" />
        | </arrayDataTypes>
        | <messageDataTypes>
        |   <messageData name="Message1" id="ID_MESSAGE1" >
        |     <field name="array1"	dataType="Array1"	word=""	numBits=""	sbp="" />
        |   </messageData>
        | </messageDataTypes>
        |</dataTypes>""".stripMargin

    val expected = Left(InvalidRepresentationNameError("BasicRepresentation"))


    msgInterpreter(xml).parse.attempt.unsafeRunSync() shouldBe expected
  }

  "Messages" should "fail when a simple data name is not in the list of valid simple data names" in {

    val xml =
      """<dataTypes>
        | <basicDataRepresentations>
        |   <basicData name="Unsigned8" size="8" />
        | </basicDataRepresentations>
        | <simpleDataTypes>
        |   <simpleData name="InvalidSimpleName" representation="Unsigned8" />
        | </simpleDataTypes>
        | <arrayDataTypes>
        |   <arrayData name="Array1" dataType="Char" cardinality="8" />
        | </arrayDataTypes>
        | <messageDataTypes>
        |   <messageData name="Message1" id="ID_MESSAGE1" >
        |     <field name="array1"	dataType="Array1"	word=""	numBits=""	sbp="" />
        |   </messageData>
        | </messageDataTypes>
        |</dataTypes>""".stripMargin

    val expected = Left(InvalidSimpleNameError("InvalidSimpleName"))


    msgInterpreter(xml).parse.attempt.unsafeRunSync() shouldBe expected
  }

}
