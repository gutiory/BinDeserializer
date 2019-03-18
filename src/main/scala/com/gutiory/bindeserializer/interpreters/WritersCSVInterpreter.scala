package com.gutiory.bindeserializer.interpreters

import cats.Applicative
import cats.implicits._
import com.gutiory.bindeserializer.algebras.Writers
import com.gutiory.bindeserializer.implicits.runtime.DeserializerMonadError
import com.gutiory.bindeserializer.models._
import scodec.Attempt.{Failure, Successful}
import scodec.{Attempt, DecodeResult}
import scodec.bits.{BitVector, ByteVector}

class WritersCSVInterpreter[F[_]: Applicative](implicit F: DeserializerMonadError[F]) extends Writers[F] {

  override def deserialize(msg: Message, bytes: Array[Byte]) : F[String] = {
    def deserializeField(fields: List[MessageField], output: String, current: Int) : String = fields match {
      case x::xs => println(current + " ===> " + (x.size + 1)/4)
        deserializeField(xs,
        (output + decodeField(x.dataType, ByteVector(bytes.slice(current, (x.size + 1)/4)).bits)) + ";", current + (x.size+1)/4)
      case Nil => output
    }
    def decodeField(fieldDT: DT, bits: BitVector) : String = fieldDT.codec.decode(bits) match {
      case Successful(value) => fieldDT match {
        case simple: SimpleDT =>
          if (simple.name == "Unsigned_char" || simple.name == "Char") {
          value.value match {
            case i : Int => i.toChar.toString
            case _ => value.value.toString
           }
          }
          else value.value.toString
        case _ => value.value.toString
      }
      case Failure(cause) => cause.message//F.raiseError(DecodeError(fieldDT, cause.message))
    }
    deserializeField(msg.fields, "", 0).pure[F]
  }

}

