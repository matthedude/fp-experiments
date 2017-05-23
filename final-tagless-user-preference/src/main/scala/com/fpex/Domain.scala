package com.fpex

import io.circe.Decoder
import io.circe.generic.AutoDerivation
import shapeless._
import shapeless.tag.@@

case class Feature(name: FeatureName, weight: Weight)

case class UserProfile(id: UserId, features: List[Feature])

case class ProgrammeData(programmeId: ProgrammeId, features: List[Feature])

case class AppError(message: String) extends Exception(message)

case class ProgrammeScore(programmeId: ProgrammeId, score: Score)

trait Decoders extends AutoDerivation {

  implicit def stringTagDecoder[T]: Decoder[String @@ T] =
    Decoder[String].map(tag[T][String](_))

  implicit def floatTagDecoder[T]: Decoder[Float @@ T] =
    Decoder[Float].map(tag[T][Float](_))

  /*
  why can't I get this to work??:
  implicit def tagDecoder[T, U](implicit taggedDecoder: Decoder[U]): Decoder[U @@ T] =
    taggedDecoder.map(tag[T][U](_))
   */

}
