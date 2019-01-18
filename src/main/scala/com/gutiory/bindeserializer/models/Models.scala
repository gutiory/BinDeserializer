package com.gutiory.bindeserializer.models

sealed trait XMLField {
  val name: String
  def size: Int
}

trait DT extends XMLField

case class SimpleDT(name:String, representation: Representation) extends DT {
  override def size: Int = representation.size
}

case class EnumeratorDT(name: String, values: List[EnumValue]) extends DT {
  override def size: Int = values.map(_.size).sum
}

case class StructDT(name: String, fields: List[StructField]) extends DT {
  override def size: Int = fields.map(_.size).sum
}

case class ArrayDT(name: String, dataType: DT, cardinality: Int) extends DT {
  override def size: Int = cardinality * dataType.size
}

case class Representation(name: String, s: Int) extends XMLField {
  override def size: Int = s
}

case class EnumValue(name: String, value: Int) extends XMLField {
  override def size: Int = 32
}

case class StructField(name: String, dataType: DT) extends XMLField {
  override def size: Int = dataType.size
}

case class MessageField(name: String, dataType: DT) extends XMLField {
  override def size: Int = dataType.size
}

case class Message(name: String, id: String, fields: List[MessageField]) extends XMLField {
  override def size: Int = fields.map(_.size).sum
}

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

