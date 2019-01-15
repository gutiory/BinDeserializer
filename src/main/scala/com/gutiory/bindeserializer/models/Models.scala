package com.gutiory.bindeserializer.models

sealed trait XMLField {
  val name: String
  def beauty(depth: Int = 0): String = this.toString
  def size(acc: Int = 0): Int = 0
}

trait DT
case class SimpleDT(name:String, representation: Representation) extends XMLField with DT
case class EnumeratorDT(name: String, values: List[EnumValue]) extends XMLField with DT
case class StructDT(name: String, fields: List[StructField]) extends XMLField with DT
case class ArrayDT(name: String, dataType: DT, cardinality: Int) extends XMLField with DT

case class Representation(name: String, size: Int) extends XMLField
case class EnumValue(name: String, value: Int) extends XMLField
case class StructField(name: String, dataType: DT) extends XMLField
case class MessageField(name: String, dataType: DT) extends XMLField
case class Message(name: String, id: String, fields: List[MessageField]) extends XMLField

sealed trait BinDeserializerError extends Throwable{
  val error: String
}

case class KeyNotFoundError(key: String) extends BinDeserializerError {
  override val error: String = s"Key $key not found"
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

