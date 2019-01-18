package com.gutiory.bindeserializer

import com.gutiory.bindeserializer.utils.TestUtils
import org.scalatest._

class WritersSpec extends FlatSpec with Matchers with TestUtils {


  "WriterCSV" should "be able to deserialize a message" in {

    val expected = "Message(Message1,ID_MESSAGE1,List(MessageField(field1,SimpleDT(Int,Representation(Signed32,32))), MessageField(enumField1,EnumeratorDT(EnumName1,List(EnumValue(ENUM0,0), EnumValue(ENUM1,1), EnumValue(ENUM2,2), EnumValue(ENUM3,3), EnumValue(ENUM4,4), EnumValue(ENUM5,5)))), MessageField(struct1,StructDT(StructData1,List(StructField(simpleField1,SimpleDT(Int,Representation(Signed32,32)))))), MessageField(array1,ArrayDT(Array1,SimpleDT(Char,Representation(Signed8,8)),8)))): size 320"

    writerCSVInterpreter.deserialize(msg1).unsafeRunSync() shouldBe expected
  }



}
