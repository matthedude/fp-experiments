package com.fpex

import com.fpex.UserPreference.RepositoryWithDatabaseInterpreter
import org.scalacheck._
import org.scalacheck.Prop._
import shapeless.tag
import shapeless.tag.@@

class UserPreferenceSpec extends Properties("UserPreference") with Generators {

  property("returns the programmes in the same order if all the programmes are unknown") = forAll {
    (programmesToSort: List[ProgrammeId], userProfile: UserProfile) =>
      implicit val userRepository =
        new RepositoryWithDatabaseInterpreter(TestDb(userProfile, List.empty))

      UserPreference.sortProgrammes(userProfile.id, programmesToSort) == Right(programmesToSort)
  }

  property("returns the programmes in the same order if the user is unknown") = forAll {
    (programmes: List[ProgrammeData], userId: UserId) =>
      implicit val userRepository =
        new RepositoryWithDatabaseInterpreter(
          TestDb(Map.empty[UserId, UserProfile], programmes.map(p => p.programmeId -> p).toMap))

      val programmesToSort = programmes.map(_.programmeId)

      UserPreference.sortProgrammes(userId, programmesToSort) == Right(programmesToSort)
  }

  property("sorting the programmes is idempotent") = forAll { (programmes: List[ProgrammeData]) =>
    val userProfile = UserProfile(tag[UserIdTag][String]("userId"), programmes.take(5).flatMap(_.features))
    implicit val userRepository =
      new RepositoryWithDatabaseInterpreter(TestDb(userProfile, programmes))

    val programmesToSort = programmes.map(_.programmeId)
    val sortedProgrammes       = UserPreference.sortProgrammes(userProfile.id, programmesToSort).getOrElse(List.empty)

    UserPreference.sortProgrammes(userProfile.id, sortedProgrammes) == Right(sortedProgrammes)
  }

  property("programmes with no features in common with the user profile are always at the bottom of the list") =
    forAll { (programmes: List[ProgrammeData]) =>
      def allBelowNoFeaturesInCommon(userProfile: UserProfile,
                                     left: List[ProgrammeId],
                                     prevInCommon: Boolean = true): Boolean =
        left match {
          case Nil => true
          case programmeId :: rest =>
            val programme = programmes.find(_.programmeId == programmeId).get //test data always there
            val notInCommon =
              programme.features.map(_.name).toSet.intersect(userProfile.features.map(_.name).toSet).isEmpty
            val allBelowNotInCommon = if (prevInCommon) true else notInCommon

            allBelowNotInCommon && allBelowNoFeaturesInCommon(userProfile, rest, !notInCommon)
        }

      val userProfile = UserProfile(tag[UserIdTag][String]("userId"), programmes.take(5).flatMap(_.features))
      implicit val userRepository =
        new RepositoryWithDatabaseInterpreter(TestDb(userProfile, programmes))

      val programmesToSort = programmes.map(_.programmeId)
      val sortedProgrammes = UserPreference.sortProgrammes(userProfile.id, programmesToSort).getOrElse(List.empty)

      allBelowNoFeaturesInCommon(userProfile, sortedProgrammes)
    }
}

trait Generators {
  def genNonEmptyStringTag[T] = Gen.alphaStr.suchThat(!_.isEmpty).map(tag[T][String](_))
  def genWeight               = Gen.choose(0.01f, 1f).map(tag[WeightTag][Float](_))

  implicit def arbTag[T]: Arbitrary[String @@ T] = Arbitrary(genNonEmptyStringTag[T])

  val genFeature: Gen[Feature] =
    for {
      fn     <- genNonEmptyStringTag[FeatureNameTag]
      weight <- genWeight
    } yield Feature(fn, weight)

  val genFeatures: Gen[List[Feature]] = Gen.listOfN(5, genFeature)

  implicit val arbUserProfile: Arbitrary[UserProfile] = Arbitrary {
    for {
      userId   <- genNonEmptyStringTag[UserIdTag]
      features <- genFeatures
    } yield UserProfile(userId, features)
  }

  implicit val arbProgrammeDataList: Arbitrary[List[ProgrammeData]] = Arbitrary {
    Gen.nonEmptyListOf {
      for {
        programmeId <- genNonEmptyStringTag[ProgrammeIdTag]
        features    <- genFeatures
      } yield ProgrammeData(programmeId, features)
    }.map(pds => pds.map(pd => (pd.programmeId -> pd)).toMap.values.toList)
  }
}
