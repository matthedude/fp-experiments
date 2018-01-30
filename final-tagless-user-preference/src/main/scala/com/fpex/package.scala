package com

import shapeless.tag.@@
import eu.timepit.refined.api.Refined
import eu.timepit.refined.boolean._
import eu.timepit.refined.types.string._
import eu.timepit.refined._
import eu.timepit.refined.numeric._

package object fpex extends Decoders {

  type ZeroToOneDouble   = Refined[Double, Greater[W.`0.0`.T] And Not[Greater[W.`1.0`.T]]]
  type PositiveDouble    = Double Refined Positive
  type NonNegativeDouble = Double Refined NonNegative

  trait UserIdTag
  trait ProgrammeIdTag
  trait FeatureNameTag
  trait WeightTag
  trait ScoreTag

  type UserId      = NonEmptyString @@ UserIdTag
  type ProgrammeId = NonEmptyString @@ ProgrammeIdTag
  type FeatureName = NonEmptyString @@ FeatureNameTag
  type Weight      = ZeroToOneDouble @@ WeightTag
  type Score       = NonNegativeDouble @@ ScoreTag
}
