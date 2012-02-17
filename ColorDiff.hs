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

module ColorDiff where
    
import System.Console.Terminfo.PrettyPrint
import Data.Generic.Diff
import System.Console.Terminfo.Color
import Text.PrettyPrint.Free
import System.Console.Terminfo.Base


ppr_edits :: EditScriptL f txs tys -> TermDoc
ppr_edits (Cpy c d)   = (text $ string c) <+> ppr_edits d
ppr_edits (CpyTree d) = (text " ... ") <+> ppr_edits d
ppr_edits (Del c d)   = (with (Foreground Red)   $ text $ ("- " ++ string c)) <+> ppr_edits d
ppr_edits (Ins c d)   = (with (Foreground Green) $ text $ ("+ " ++ string c)) <+> ppr_edits d
ppr_edits (End)       = line

display_better d = do
    t <- setupTermFromEnv
    displayDoc'' t 1.0 60 d