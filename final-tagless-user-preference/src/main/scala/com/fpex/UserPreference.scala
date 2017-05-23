package com.fpex

import cats._
import cats.data._
import cats.implicits._
import io.circe._
import io.circe.parser._
import shapeless.tag

object UserPreference {

  trait RepositoryAlg[F[_]] {
    def userProfile(userId: UserId): F[Either[AppError, Option[UserProfile]]]
    def programmeData(programmeId: ProgrammeId): F[Either[AppError, Option[ProgrammeData]]]
  }

  trait DatabaseAlg[F[_]] {
    def getUserProfile(userId: UserId): F[Either[AppError, Option[String]]]
    def getProgrammeData(programmeId: ProgrammeId): F[Either[AppError, Option[String]]]
  }

  class RepositoryWithDatabaseInterpreter[F[_] : Monad](db: DatabaseAlg[F]) extends RepositoryAlg[F] {

    override def userProfile(userId: UserId) =
      decodeFromDb[UserProfile, UserId](userId)(db.getUserProfile _)

    override def programmeData(programmeId: ProgrammeId) =
      decodeFromDb[ProgrammeData, ProgrammeId](programmeId)(db.getProgrammeData _)

    private def decodeFromDb[T, I](id: I)(fetchData: I => F[Either[AppError, Option[String]]])(
        implicit decoder: Decoder[T]): F[Either[AppError, Option[T]]] =
      fetchData(id).map(
        _.flatMap(
          _.map(decode[T]).sequence.leftMap(e => AppError(e.getMessage))
        )
      )
  }

  def sortProgrammes[F[_] : Monad](userId: UserId, programmesToSort: List[ProgrammeId])(
      implicit repo: RepositoryAlg[F]): F[Either[AppError, List[ProgrammeId]]] = {

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

    val upF = repo.userProfile(userId)
    val pdF = programmesToSort.traverse(repo.programmeData).map(_.sequence)

    for {
      upO <- EitherT(upF)
      pdL <- EitherT(pdF)
      (_, unknown) = pdL.zip(programmesToSort).filter(_._1.isEmpty).unzip
      pd           = pdL.flatten
      sorted <- upO
        .map(sort(_, pd) ::: unknown)
        .getOrElse(programmesToSort)
        .asRight[AppError]
        .toEitherT[F]
    } yield sorted
  }.value

}
