package com.gutiory.bindeserializer.models

import scodec.Codec
import scodec.codecs._

sealed trait XMLField {
  val name: String
  def size: Int
  def codec[_]: Codec[_]
}

trait DT extends XMLField

object AllowedValues {
  val representationNameList = List("Unsigned8", "Unsigned16", "Unsigned32", "Signed8", "Signed16",
    "Signed32", "Float32", "Float64")

  val simpleNameList = List("Unsigned_char", "Unsigned_short", "Unsigned_int", "Char",
    "Bool", "Int", "Float", "Double","Enum")
}

case class SimpleDT(name:String, representation: Representation) extends DT {
  override def size: Int = representation.size

  override def codec[_]: Codec[_] = representation.codec
}

case class EnumeratorDT(name: String, values: List[EnumValue]) extends DT {
  override def size: Int = 32

  override def codec[_]:Codec[_] = values.head.codec
}

case class StructDT(name: String, fields: List[StructField]) extends DT {
  override def size: Int = fields.map(_.size).sum

  override def codec[_]: Codec[_] = fields.map(_.codec).reduceLeft(_ ~ _)
}

case class ArrayDT(name: String, dataType: DT, cardinality: Int) extends DT {
  override def size: Int = cardinality * dataType.size

  override def codec[_]: Codec[_] = dataType.codec//List.fill(cardinality)(dataType.codec).fold(Codec.deriveHNil)(_::_)
}

case class Representation(name: String, s: Int) extends XMLField {
  override def size: Int = s

  override def codec[_]: Codec[_] = name match {
    case "Unsigned8" => uint8
    case "Unsigned16" => uint16
    case "Unsigned32" => uint32
    case "Signed8" => int8
    case "Signed16" => int16
    case "Signed32" => int32
    case "Float32" => float
    case "Float64" => double
  }
}

case class EnumValue(name: String, value: Int) extends XMLField {
  override def size: Int = 32

  override def codec[_]: Codec[_] = int32
}

case class StructField(name: String, dataType: DT) extends XMLField {
  override def size: Int = dataType.size

  override def codec[_]: Codec[_] = dataType.codec
}

case class MessageField(name: String, dataType: DT) extends XMLField {
  override def size: Int = dataType.size

  override def codec[_]: Codec[_] = dataType.codec
}

case class Message(name: String, id: String, fields: List[MessageField]) extends XMLField {
  override def size: Int = fields.map(_.size).sum

  override def codec[_]: Codec[_] = fields.map(_.codec).fold(Codec.deriveHNil)( (x,y) => x ~ y)
}

sealed trait BinDeserializerError extends Throwable{
  val error: String
}

case class KeyNotFoundError(key: String) extends BinDeserializerError {
  override val error: String = s"Key $key not found"
}

case class InvalidRepresentationNameError(name: String) extends BinDeserializerError {
  override val error: String = s"Representation $name is not valid"
}

case class InvalidSimpleNameError(name: String) extends BinDeserializerError {
  override val error: String = s"Simple $name is not valid"
}

case class RepresentationNotFoundError(representation: String) extends BinDeserializerError {
  override val error: String = s"Representation $representation not found"
}

case class DataTypeNotFoundError(dataType: String) extends BinDeserializerError {
  override val error: String = s"DataType $dataType not found"
}

case object UnknownNodeError extends BinDeserializerError {
  override val error: String = "Unknown node"
}

case class DecodeError(field: DT, cause: String) extends BinDeserializerError {
  override val error: String = s"$field.name decode failed. $cause."
}

case class XMLFile(file: String)

