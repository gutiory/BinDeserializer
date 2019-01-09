package com.gutiory.bindeserializer

import scala.xml.XML
import implicits.runtime._
import cats.effect.IO

object BinDeserializer extends App {
  workflow[IO].parseXML("OneMessage.xml")
}

