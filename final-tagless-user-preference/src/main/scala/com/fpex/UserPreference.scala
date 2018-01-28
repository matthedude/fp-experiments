package com.fpex

import cats._
import cats.data.OptionT
import cats.implicits._
import io.circe._
import io.circe.parser._
import shapeless.tag

object UserPreference {

  trait RepositoryAlg[F[_]] {
    def userProfile(userId: UserId): F[Option[UserProfile]]
    def programmeData(programmeId: ProgrammeId): F[Option[ProgrammeData]]
  }

  trait DatabaseAlg[F[_]] {
    def getUserProfile(userId: UserId): F[Option[String]]
    def getProgrammeData(programmeId: ProgrammeId): F[Option[String]]
  }

  class RepositoryWithDatabaseInterpreter[F[_] : Monad](db: DatabaseAlg[F])(implicit M: MonadError[F, Error])
      extends RepositoryAlg[F] {

    override def userProfile(userId: UserId) =
      decodeFromDb[UserProfile, UserId](userId)(db.getUserProfile _)

    override def programmeData(programmeId: ProgrammeId) =
      decodeFromDb[ProgrammeData, ProgrammeId](programmeId)(db.getProgrammeData _)

    private def decodeFromDb[T, I](id: I)(fetchData: I => F[Option[String]])(
        implicit decoder: Decoder[T]): F[Option[T]] =
      M.rethrow(fetchData(id).map(_.map(decode[T]).sequence))
  }

  def sortProgrammes[F[_] : Monad](userId: UserId, programmesToSort: List[ProgrammeId])(
      implicit repo: RepositoryAlg[F]): F[List[ProgrammeId]] = {

    def sort(userProfile: UserProfile, programmeData: List[ProgrammeData]): List[ProgrammeId] =
      programmeData.map { pd =>
        val scoreProduct =
          for {
            pdFeature          <- pd.features
            userProfileFeature <- userProfile.features
            if pdFeature.name == userProfileFeature.name
          } yield (pdFeature.weight * userProfileFeature.weight)

        ProgrammeScore(pd.programmeId, tag[ScoreTag][Float](scoreProduct.sum))
      }.sortBy(-_.score).map(_.programmeId)

    val upF    = repo.userProfile(userId)
    val pdF    = programmesToSort.traverse(repo.programmeData)
    val knownF = pdF.map(_.filter(_.isDefined).sequence)
    val unknownF = pdF.map { pdL =>
      val (_, unknown) = pdL.zip(programmesToSort).filter(_._1.isEmpty).unzip
      Option(unknown)
    }

    for {
      up      <- OptionT(upF)
      pdL     <- OptionT(knownF)
      unknown <- OptionT(unknownF)
    } yield (sort(up, pdL) ::: unknown)
  }.value.map(_.getOrElse(programmesToSort))

}
