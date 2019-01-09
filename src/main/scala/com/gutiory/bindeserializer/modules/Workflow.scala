package com.gutiory.bindeserializer.modules

import com.gutiory.bindeserializer.algebras.Messages
import com.gutiory.bindeserializer.models.Message

trait Workflow[F[_]] {

  def parseXML(xmlFile: String): F[List[Message]]

  def printStr(str: String)
}

object Workflow{
  def impl[F[_]](messages: Messages[F]): Workflow[F] = new Workflow[F] {
    override def parseXML(xmlFile: String): F[List[Message]] = {
      messages.parseXMLFile(xmlFile)
    }

    override def printStr(str: String): Unit = println(str)
  }
}
