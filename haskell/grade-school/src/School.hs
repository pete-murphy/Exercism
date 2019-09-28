module School
  ( School
  , add
  , empty
  , grade
  , sorted
  ) where

import           Data.List  (sort)
import           Data.Map   (Map)
import qualified Data.Map   as M
import           Data.Maybe (fromMaybe)

type School = Map GradeNum [Student]

type GradeNum = Int

type Student = String

add :: GradeNum -> Student -> School -> School
add gradeNum student = fmap sort <$> M.insertWith (++) gradeNum [student]

empty :: School
empty = mempty

grade :: GradeNum -> School -> [Student]
grade = fmap (fromMaybe []) <$> M.lookup

sorted :: School -> [(GradeNum, [Student])]
sorted = sort <$> M.toList
