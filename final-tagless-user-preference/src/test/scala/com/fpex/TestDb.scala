package com.fpex

import cats.implicits._
import io.circe._
import io.circe.syntax._
import io.circe.refined._
import UserPreference._
import eu.timepit.refined.types.string.NonEmptyString
import shapeless.tag.@@
import TestDb._

case class TestDb(upDb: Map[UserId, UserProfile], pdDb: Map[ProgrammeId, ProgrammeData])
    extends DatabaseAlg[EitherExec] {

  val nonEmptyStringEncoder: Encoder[NonEmptyString] = refinedEncoder
  val zeroToOneEncoder: Encoder[ZeroToOneDouble]     = refinedEncoder
  val positiveDoubleEncoder: Encoder[PositiveDouble] = refinedEncoder

  implicit def nonEmptyStringTagEncoder[T]: Encoder[NonEmptyString @@ T] =
    nonEmptyStringEncoder.narrow

  implicit def weightEncoder: Encoder[ZeroToOneDouble @@ WeightTag] =
    zeroToOneEncoder.narrow

  implicit def scoreEncoder: Encoder[PositiveDouble @@ ScoreTag] =
    positiveDoubleEncoder.narrow

  override def getUserProfile(id: UserId) =
    upDb.get(id).map(_.asJson.noSpaces).asRight[Error]

  override def getProgrammeData(id: ProgrammeId) =
    pdDb.get(id).map(_.asJson.noSpaces).asRight[Error]
}

object TestDb {
  type EitherExec[A] = Either[Error, A]

  def apply(userProfile: UserProfile, pdDb: List[ProgrammeData]): TestDb =
    apply(Map(userProfile.id -> userProfile), pdDb.map(p => (p.programmeId, p)).toMap)
}
