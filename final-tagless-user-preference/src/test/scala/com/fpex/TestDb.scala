package com.fpex

import cats._
import cats.implicits._
import io.circe._
import io.circe.syntax._
import shapeless.tag.@@

import UserPreference._

case class TestDb(upDb: Map[UserId, UserProfile],
                  pdDb: Map[ProgrammeId, ProgrammeData])
    extends DatabaseAlg[Id] {

  implicit def stringTagEncoder[T]: Encoder[String @@ T] =
    Encoder[String].narrow

  implicit def floatTagEncoder[T]: Encoder[Float @@ T] =
    Encoder[Float].narrow

  override def getUserProfile(id: UserId) =
    upDb.get(id).map(_.asJson.noSpaces).asRight[AppError]

  override def getProgrammeData(id: ProgrammeId) =
    pdDb.get(id).map(_.asJson.noSpaces).asRight[AppError]
}

object TestDb {
  def apply(userProfile: UserProfile, pdDb: List[ProgrammeData]): TestDb =
    apply(Map(userProfile.id -> userProfile),
          pdDb.map(p => (p.programmeId, p)).toMap)
}
