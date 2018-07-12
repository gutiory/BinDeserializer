package com.gutiory.bindeserializer.models

abstract class Field(val name: String, val dataType: Option[String] = None, val word: Option[Int] = None, val numBits: Option[Int] = None)

case class Basic(override val name: String, size: Option[Int]) extends Field(name, numBits = size)

case class Simple(override val name:String, representation: Basic) extends Field(name, numBits = representation.size)

case class Struct(override val name: String, fields: List[Option[Field]]) extends Field(name, None)

case class StructField(override val name: String, override val dataType: Option[String]) extends Field(name, dataType)

case class Enum(override val name: String, value: Option[Int]) extends Field(name)

case class EnumList(override val name: String, enums: List[Option[Field]]) extends Field(name)

case class Array(override val name: String, override val dataType: Option[String], cardinality: Int, countName: Option[String]) extends Field(name, dataType)

case class Message(name: String, id: String, fields: List[Field])
