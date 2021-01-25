package com.gutiory.bindeserializer

import com.gutiory.bindeserializer.models.DT
import com.gutiory.bindeserializer.utils.TestUtils
import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop._
import org.scalatest.prop.Checkers
import org.scalatest._


class WritersSpec extends FlatSpec with Matchers with TestUtils with Checkers {

  "WriterCSV" should "be able to deserialize all representations" in {
    simpleList.foreach(repr => {
      println(repr)
      val simpleMsg = genSimpleMessage(repr._1, repr._2).sample.get
      println("repr => " + simpleMsg)
      val simpleMsgData = simpleMsg.fields.map(mf => generateDTBytes(mf.dataType, "")).unzip
      val expected = simpleMsgData._2.foldRight("")(_ ++ _)
      assert(writerCSVInterpreter.deserialize(simpleMsg, simpleMsgData._1.foldRight(Array[Byte]())(_ ++ _)).unsafeRunSync() == expected,
        repr)
    })
  }

  "WriterCSV" should "be able to deserialize all simples messages" in {
    simpleGenList.foreach(simpleGenDT => {
      val simpleMsg = genSimpleMessage(simpleGenDT).sample.get
      println("simpleMsg => " + simpleMsg)
      val simpleMsgData = simpleMsg.fields.map(mf => generateDTBytes(mf.dataType, "")).unzip
      val expected = simpleMsgData._2.foldRight("")(_ ++ _)

      assert(writerCSVInterpreter.deserialize(simpleMsg, simpleMsgData._1.foldRight(Array[Byte]())(_ ++ _)).unsafeRunSync() == expected,
        simpleMsg)
      })
    }

    "WriterCSV" should "be able to deserialize a message" in {

    //val expected = "Message(Message1,ID_MESSAGE1,List(MessageField(field1,SimpleDT(Int,Representation(Signed32,32))), MessageField(enumField1,EnumeratorDT(EnumName1,List(EnumValue(ENUM0,0), EnumValue(ENUM1,1), EnumValue(ENUM2,2), EnumValue(ENUM3,3), EnumValue(ENUM4,4), EnumValue(ENUM5,5)))), MessageField(struct1,StructDT(StructData1,List(StructField(simpleField1,SimpleDT(Int,Representation(Signed32,32)))))), MessageField(array1,ArrayDT(Array1,SimpleDT(Char,Representation(Signed8,8)),8)))): size 320"
    println(genMessage.sample)

    val msg2 = genMessage.sample.get
    val msg2Data = msg2.fields.map(mf => generateDTBytes(mf.dataType, "")).unzip
    //println(msg2ByteArray)
    val expected = msg2Data._2.foldRight("")(_ ++ _)
    println(expected)
    println(msg2.size)
    println(msg2Data._1.foldRight(Array[Byte]())(_ ++ _).length)
    writerCSVInterpreter.deserialize(msg2, msg2Data._1.foldRight(Array[Byte]())(_ ++ _)).unsafeRunSync() shouldBe expected

    //println(dataToByteArray(3,4))
  }



}
