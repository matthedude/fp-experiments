package com.fpex.preference

import com.fpex.preference.UserPreference.RepositoryWithDatabaseInterpreter
import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck.ScalacheckShapeless._
import shapeless.tag
import shapeless.tag.@@
import eu.timepit.refined._
import eu.timepit.refined.api.RefType
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.types.string._
import eu.timepit.refined.scalacheck._
import eu.timepit.refined.scalacheck.string._
import eu.timepit.refined.scalacheck.numeric._
import cats.instances.either._
import cats.data.NonEmptyList
import TestDatabaseAlg._

class UserPreferenceProps extends Properties("UserPreference") with Generators {

  //without this UserProfile at :30 can't be found :-/
  implicit val arbProgrammeDataList: Arbitrary[List[ProgrammeData]] = Arbitrary(
    Gen.nonEmptyListOf(implicitly[Arbitrary[ProgrammeData]].arbitrary))

  property("returns the programmes in the same order if all the programmes are unknown") = forAll {
    (programmesToSort: List[ProgrammeId], userProfile: UserProfile) =>
      implicit val userRepository =
        new RepositoryWithDatabaseInterpreter(TestDatabaseAlg(userProfile, List.empty))

      UserPreference.sortProgrammes[EitherExec](userProfile.id, programmesToSort) == Right(programmesToSort)
  }

  property("returns the programmes in the same order if the user is unknown") = forAll {
    (programmes: NonEmptyList[ProgrammeData], userId: UserId) =>
      implicit val userRepository =
        new RepositoryWithDatabaseInterpreter(
          TestDatabaseAlg(Map.empty[UserId, UserProfile], programmes.toList.map(p => p.programmeId -> p).toMap))

      val programmesToSort = programmes.map(_.programmeId).toList

      UserPreference.sortProgrammes[EitherExec](userId, programmesToSort) == Right(programmesToSort)
  }

  property("sorting the programmes is idempotent") = forAll { (programmes: NonEmptyList[ProgrammeData]) =>
    val userProfile = UserProfile(tag[UserIdTag][NonEmptyString](refineMV("userId")),
                                  NonEmptyList.fromListUnsafe(programmes.toList.take(5).flatMap(_.features.toList)))
    implicit val userRepository =
      new RepositoryWithDatabaseInterpreter(TestDatabaseAlg(userProfile, programmes.toList))

    val programmesToSort = programmes.map(_.programmeId)
    val sortedProgrammes =
      UserPreference.sortProgrammes[EitherExec](userProfile.id, programmesToSort.toList).getOrElse(List.empty)

    UserPreference.sortProgrammes[EitherExec](userProfile.id, sortedProgrammes) == Right(sortedProgrammes)
  }

  property("programmes with no features in common with the user profile are always at the bottom of the list") =
    forAll { (programmes: NonEmptyList[ProgrammeData]) =>
      val userProfile = UserProfile(tag[UserIdTag][NonEmptyString](refineMV("userId")),
                                    NonEmptyList.fromListUnsafe(programmes.toList.take(5).flatMap(_.features.toList)))
      implicit val userRepository =
        new RepositoryWithDatabaseInterpreter(TestDatabaseAlg(userProfile, programmes.toList))

      val programmesToSort = programmes.map(_.programmeId).toList
      val sortedProgrammes =
        UserPreference.sortProgrammes[EitherExec](userProfile.id, programmesToSort).getOrElse(List.empty)

      val featuresInCommon =
        (s: Set[FeatureName]) => !s.intersect(userProfile.features.map(_.name).toList.toSet).isEmpty

      val programmaData       = programmes.toList.map(p => (p.programmeId -> p)).toMap
      val sortedProgrammeData = sortedProgrammes.map(programmaData)

      val bottomOfList = sortedProgrammeData
        .map(
          _.features
            .map(_.name)
            .toList
            .toSet)
        .dropWhile(featuresInCommon(_))

      !featuresInCommon(bottomOfList.toSet.flatten)
    }
}

trait Generators {
  implicit def nonEmptyStringArbitrary[F[_, _]](
      implicit rt: RefType[F],
  ): Arbitrary[F[String, NonEmpty]] =
    arbitraryRefType(Gen.nonEmptyListOf[Char](arbChar.arbitrary).map(_.mkString))

  /* Didn't compile :(
  implicit def arbTag[A, B, T, R[_, _]](implicit arb: Arbitrary[R[A, B]]): Arbitrary[R[A, B] @@ T] =
    arbitraryRefType(arb.arbitrary)
   */

  implicit def arbNoneEmptyStringTag[T](implicit arb: Arbitrary[NonEmptyString]): Arbitrary[NonEmptyString @@ T] =
    arbitraryRefType(arb.arbitrary)

  implicit def arbZeroToOneDoubleTag[T](implicit arb: Arbitrary[ZeroToOneDouble]): Arbitrary[ZeroToOneDouble @@ T] =
    arbitraryRefType(arb.arbitrary)
}
