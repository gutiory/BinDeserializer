package com.gutiory.bindeserializer.models

sealed trait XMLField{val name: String}

case class Basic(name: String, numBits: Option[Int]) extends XMLField

case class Simple(name:String, representation: Option[String]) extends XMLField

case class Struct(name: String, fields: List[Option[StructField]]) extends XMLField

case class StructField(name: String, dataType: Option[String]) extends XMLField

case class Enum(name: String, value: Option[Int]) extends XMLField

case class EnumList(name: String,representation: Option[String], enums: List[Option[XMLField]]) extends XMLField

case class XMLArray(name: String, dataType: Option[String], cardinality: Int, countName: Option[String]) extends XMLField

case class MessageField(name: String, dataType: Option[String]) extends XMLField

case class Message(name: String, id: String, fields: List[XMLField])
