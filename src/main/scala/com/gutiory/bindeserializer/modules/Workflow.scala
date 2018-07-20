package com.gutiory.bindeserializer.modules

import com.gutiory.bindeserializer.algebras.Messages

trait Workflow[+F[_]] {

  def parseXML(xmlFile: String)

  def printStr(str: String)
}

object Workflow{
  def impl[F[_]](messages: Messages[F]) = new Workflow[F] {
    override def parseXML(xmlFile: String): Unit = messages.parseXMLFile(xmlFile)

    override def printStr(str: String): Unit = println(str)
  }
}
