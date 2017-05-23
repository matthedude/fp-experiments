package com

import shapeless.tag.@@

package object fpex extends Decoders {

  trait UserIdTag
  trait ProgrammeIdTag
  trait FeatureNameTag
  trait WeightTag
  trait ScoreTag

  type UserId = String @@ UserIdTag
  type ProgrammeId = String @@ ProgrammeIdTag
  type FeatureName = String @@ FeatureNameTag
  type Weight = Float @@ WeightTag
  type Score = Float @@ ScoreTag
}
