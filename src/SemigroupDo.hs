module SemigroupDo where

import Relude

(>>) :: Semigroup a => a -> a -> a
(>>) = (<>)
