{-#  LANGUAGE GADTs  #-}
{-#  LANGUAGE KindSignatures  #-}
{-#  LANGUAGE TypeFamilies  #-}
{-#  LANGUAGE TypeOperators  #-}
{-#  LANGUAGE MultiParamTypeClasses  #-}
{-#  LANGUAGE FunctionalDependencies  #-}
{-#  LANGUAGE FlexibleInstances  #-}
{-#  LANGUAGE RankNTypes  #-}
{-#  LANGUAGE ScopedTypeVariables  #-}
{-#  LANGUAGE OverlappingInstances  #-} --  Only for the Show
{-#  LANGUAGE NoMonomorphismRestriction  #-} --  Only for the Show
{-#  LANGUAGE TemplateHaskell  #-} --  Only for the Show

module CommandDiff where
import Data.Generic.Diff
import Commands
import CommandDiffTH
import Types

$(make_family_gadt ''Output)

