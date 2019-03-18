package com.gutiory.bindeserializer.utils

import cats.MonadError
import cats.effect.IO
import com.gutiory.bindeserializer.algebras.{Messages, Nodes}
import com.gutiory.bindeserializer.interpreters.{MessagesInterpreter, NodesInterpreter, WritersCSVInterpreter}
import com.gutiory.bindeserializer.models._
import org.scalacheck.{Arbitrary, Gen}
import Arbitrary.arbitrary

trait TestUtils {

  type TestTargetMonad[A] = IO[A]
  type TestMonadError[F[_]] = MonadError[TestTargetMonad, Throwable]

  val representationList = List("Unsigned8", "Unsigned16", "Unsigned32", "Signed8", "Signed16",
    "Signed32", "Float32", "Float64")
  val simpleList = List(("Unsigned_char","Unsigned8"), ("Unsigned_short","Unsigned16"), ("Unsigned_int", "Unsigned32"),
  ("Char", "Signed8"), ("Bool","Unsigned8"), ("Int","Signed32"), ("Float","Float32"), ("Double","Float64"), ("Enum", "Signed32"))

  def getRepresentation(name: String) : Option[Representation] = name match {
      case "Unsigned8" => Option(Representation(name, 8))
      case "Unsigned16" => Option(Representation(name, 16))
      case "Unsigned32" => Option(Representation(name, 32))
      case "Signed8" => Option(Representation(name, 8))
      case "Signed16" => Option(Representation(name, 16))
      case "Signed32" => Option(Representation(name, 32))
      case "Float32" => Option(Representation(name, 32))
      case "Float64" => Option(Representation(name, 64))
      case _ => None
    }

  def genChar: Char = Gen.alphaChar.sample.getOrElse('c')
  def genInt: Int = arbitrary[Int].sample.getOrElse(2)
  def genShort: Short = arbitrary[Short].sample.getOrElse(7.toShort)
  def genUnsignedShort: Short = arbitrary[Short].filter(_ >= 0).sample.getOrElse(7.toShort)
  def genUnsignedInt: Int = arbitrary[Int].filter(_ >= 0).sample.getOrElse(5)
  def getFloat: Float = arbitrary[Float].sample.getOrElse(3.5F)
  def getDouble: Double = arbitrary[Double].sample.getOrElse(0.34)
  def genGenericName: Gen[String] = Gen.listOfN(5, Gen.alphaChar).map(_.mkString)
  def genOneDT: Gen[DT] = Gen.oneOf(genSimpleDT, genEnumDT, genStructDT)//, genArrayDT)
  def genSimpleDt(simpleName: String, representationName: String) : SimpleDT = SimpleDT(simpleName, getRepresentation(representationName).get)
  def genSimpleMessageField(simpleDTGen: Gen[DT]) = MessageField(genGenericName.sample.get, simpleDTGen.sample.get)
  val genEnumValue: Gen[EnumValue] = for {
    name <- genGenericName
    value <- Gen.oneOf(0 to 20)
  } yield EnumValue(name, value)

  val genSimpleDT: Gen[SimpleDT] = for {
    (name,reprName) <- Gen.oneOf(simpleList)
    representation = getRepresentation(reprName).get
  } yield SimpleDT(name, representation)

  val genEnumDT: Gen[EnumeratorDT] = for {
    name <- genGenericName
    values = enumeratorList
  } yield EnumeratorDT(name, values)

  val genStructField: Gen[StructField] = for   {
    name <- genGenericName
    dataType:DT <- genOneDT
  } yield StructField(name, dataType)

  val genStructDT: Gen[StructDT] = for {
    name <- genGenericName
    fields = structFieldList
  } yield StructDT(name, fields)

  val genArrayDT: Gen[ArrayDT] = for {
    name <- genGenericName
    dataType <- genOneDT
    cardinality <- Gen.oneOf(1 to 10)
  } yield ArrayDT(name, dataType, cardinality)

  val genMessageField: Gen[MessageField] = for {
    name <- genGenericName
    dataType <- genOneDT
  } yield MessageField(name, dataType)

  val genMessage: Gen[Message] = for {
    name <- genGenericName
    id <- genGenericName
    fields = messageFieldList
  } yield Message(name, id, fields)

  def genSimpleMessage(simpleGen: Gen[DT]): Gen[Message] = for {
    name <- genGenericName
    id <- genGenericName
    fields = List(genSimpleMessageField(simpleGen))
  } yield Message(name, id, fields)

  def genSimpleMessage(simpleName:String, representation: String): Gen[Message] = for {
    name <- genGenericName
    id <- genGenericName
    fields = List(genSimpleMessageField(genSimpleDt(simpleName, representation)))
  } yield Message(name, id, fields)


  //val representationList = Gen.listOfN(5,genRepresentation).sample.get
  lazy val enumeratorList: List[EnumValue] = Gen.listOfN(Gen.oneOf(1 to 10).sample.get, genEnumValue).sample.get
  lazy val structFieldList: List[StructField] = Gen.listOfN(Gen.oneOf(1 to 10).sample.get, genStructField).sample.get
  lazy val messageFieldList: List[MessageField] = Gen.listOfN(Gen.oneOf(1 to 10).sample.get, genMessageField).sample.get
  def oneFieldMessage: MessageField = genMessageField.sample.get
  lazy val simpleGenList: List[Gen[DT]] = List(genSimpleDT, genEnumDT, genStructDT, genArrayDT)

  /*
  val arbitraryFieldDT:Arbitrary[XMLField] = Arbitrary {
    val fields: Gen[XMLField] = Gen.oneOf(
      List(SimpleDT, EnumeratorDT, StructDT, ArrayDT).map()
    )
  }
  */

  def msgInterpreter(xml: String): Messages[TestTargetMonad] = {
    implicit def nodes: Nodes[TestTargetMonad] = new NodesInterpreter[TestTargetMonad](Left(xml))

    new MessagesInterpreter[TestTargetMonad]
  }

  val writerCSVInterpreter: WritersCSVInterpreter[TestTargetMonad] = new WritersCSVInterpreter[TestTargetMonad]

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

  //val msg1ByteArray: Array[Byte] = msg1.fields.flatMap(mf => generateDTBytes(mf.dataType)).toArray


  def dataToByteArray(value: AnyVal, size: Int): Array[Byte] = {
    val bb = java.nio.ByteBuffer.allocate(size)
    value match {
      case c: Char => bb.putChar(c).array()
      case s: Short => bb.putShort(s).array()
      case i: Int => bb.putInt(i).array()
      case f: Float => bb.putFloat(f).array()
      case d: Double => bb.putDouble(d).array()
      case b: Byte => bb.put(b).array()
      case _ => bb.array()
    }
  }

  def generateDTBytes(fieldDT: DT, acc: String): (Array[Byte], String) = {
    //println("generateDTBytes => " + fieldDT.name)
    fieldDT match {
      case s: SimpleDT => s.name match {
        case "Char" =>
          val newChar = genChar
          (dataToByteArray(newChar.toByte, 1), acc ++ newChar.toString ++ ";")
        case "Int" =>
          val newInt = genInt
          (dataToByteArray(newInt, 4), acc ++ newInt.toString  ++ ";")
        case "Unsigned_int" =>
          val newUnsignedInt = genUnsignedInt
          (dataToByteArray(newUnsignedInt, 4), acc ++ newUnsignedInt.toString ++ ";")
        case "Unsigned_short" =>
          val newUnsignedShort = genUnsignedShort
          (dataToByteArray(newUnsignedShort, 2), acc ++ newUnsignedShort.toString ++ ";")
        case "Unsigned_char" => (dataToByteArray('s'.toByte, 1), acc ++ 's'.toString ++ ";")
        case "Bool" => (dataToByteArray(1.toByte, 1), acc ++ 1.toString ++ ";")
        case "Enum" => (dataToByteArray(4, 4), acc ++ 4.toString ++ ";")
        case "Float" => (dataToByteArray(3.45F, 4), acc ++ 3.45F.toString ++ ";")
        case "Double" => (dataToByteArray(56.89, 8), acc ++ 56.89.toString ++ ";")
      }
      case e: EnumeratorDT => (dataToByteArray(2, 4), acc ++ 2.toString ++ ";")
      case s: StructDT => generateStructDTBytes(s, acc)
      case a: ArrayDT => generateArrayDTBytes(a.dataType, a.cardinality, Array[Byte](), acc)
    }
  }
  def generateArrayDTBytes(arrayDT: DT, cardinality: Int, acumBytes: Array[Byte], acumStr:String) : (Array[Byte], String) = {
    val generatedValues = generateDTBytes(arrayDT, acumStr)
    cardinality match {
      case 1 => (acumBytes ++ generatedValues._1, generatedValues._2)
      case _ => generateArrayDTBytes(arrayDT, cardinality - 1, acumBytes ++ generatedValues._1, generatedValues._2)
    }
  }

  def generateStructDTBytes(fieldStruct: StructDT, acum:String): (Array[Byte], String) = {
    val resPair = fieldStruct.fields.map{
      fs => generateDTBytes(fs.dataType, acum)
    }.unzip
    val resBytes = resPair._1.foldRight(Array[Byte]())(_ ++ _)
    val resString = resPair._2.foldRight("")(_ ++ _)
    (resBytes, resString)
  }
}