package com.fpex.preference

import cats.data.NonEmptyList
import eu.timepit.refined.types.string.NonEmptyString
import io.circe._
import io.circe.generic.AutoDerivation
import io.circe.refined._
import shapeless._
import shapeless.tag.@@

final case class Feature(name: FeatureName, weight: Weight)

final case class UserProfile(id: UserId, features: NonEmptyList[Feature])

final case class ProgrammeData(programmeId: ProgrammeId, features: NonEmptyList[Feature])

final case class ProgrammeScore(programmeId: ProgrammeId, score: Score)

trait Decoders extends AutoDerivation {

  val nonEmptyStringDecoder: Decoder[NonEmptyString] = refinedDecoder
  val zeroToOneDecoder: Decoder[ZeroToOneDouble]     = refinedDecoder
  val positiveDoubleDecoder: Decoder[PositiveDouble] = refinedDecoder

  implicit def nonEmptyStringTagDecoder[T]: Decoder[NonEmptyString @@ T] =
    nonEmptyStringDecoder.map(tag[T][NonEmptyString](_))

  implicit val scoreDecoder: Decoder[ZeroToOneDouble @@ WeightTag] =
    zeroToOneDecoder.map(tag[WeightTag][ZeroToOneDouble](_))

  implicit val weightDecoder: Decoder[PositiveDouble @@ ScoreTag] =
    positiveDoubleDecoder.map(tag[ScoreTag][PositiveDouble](_))
}
