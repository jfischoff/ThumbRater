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

module DiffTest where
import System.Console.Terminfo.PrettyPrint
import Data.Generic.Diff
import System.Console.Terminfo.Color
import Text.PrettyPrint.Free
import System.Console.Terminfo.Base


data Expr = Add Expr Expr
          | Primitive Int
          
data ExprFamily :: * -> * -> * where
    Add'           ::        ExprFamily Expr (Cons Expr (Cons Expr Nil))      
    Primitive'     ::        ExprFamily Expr (Cons Int Nil)
    Int'           :: Int -> ExprFamily Int Nil
    
instance Family ExprFamily where
    decEq Add'    Add'                       = Just (Refl, Refl)
    decEq Primitive' Primitive'              = Just (Refl, Refl)
    decEq (Int' x) (Int' y)     | x == y     = Just (Refl, Refl)
                                | otherwise  = Nothing
    decEq _ _                                = Nothing
    
    fields Add' (Add e e') = Just (CCons e (CCons e' CNil))
    fields Primitive' (Primitive i) = Just (CCons i CNil)
    fields (Int' _) _ = Just CNil
    fields _ _ = Nothing
    
    apply Add' (CCons e (CCons e' CNil)) = Add e e'
    apply Primitive' (CCons i CNil) = Primitive i
    apply (Int' i) CNil = i
    
    string Add'       = "Add"
    string Primitive' = "Primitive"
    string (Int' i)   = show i
    
instance Type ExprFamily Expr where
    constructors = [Concr Primitive', Concr Add']
    
instance Type ExprFamily Int where
    constructors = [Abstr Int']
    
    
    
test_0 = Add (Primitive 1) $ Add (Primitive 1) $ Add (Primitive 1) $ Add (Primitive 1) $ Add (Primitive 1) (Primitive 2)
test_1 = Add (Primitive 1) $ Add (Primitive 1) $ Add (Primitive 2) $ Add (Primitive 1) $ Add (Primitive 1) (Primitive 2)

diff_0 :: EditScript ExprFamily Expr Expr
diff_0 = diff test_0 test_1

compress_0 :: EditScriptL ExprFamily (Cons Expr Nil) (Cons Expr Nil)
compress_0 = compress diff_0


{-
    I want a function that shows an edit list of a given type

-}

ppr_edits :: EditScriptL f txs tys -> TermDoc
ppr_edits (Cpy c d)   = (text $ string c) <+> ppr_edits d
ppr_edits (CpyTree d) = (text " ... ") <+> ppr_edits d
ppr_edits (Del c d)   = (with (Foreground Red)   $ text $ ("- " ++ string c)) <+> ppr_edits d
ppr_edits (Ins c d)   = (with (Foreground Green) $ text $ ("+ " ++ string c)) <+> ppr_edits d
ppr_edits (End)       = line


    
display_better d = do
    t <- setupTermFromEnv
    displayDoc'' t 1.0 60 d
    
    
------------------------------------------------------------------------------------------------------------------------------

{-
data RoseTree = Node Int [RoseTree]
              | Leaf Int
    
    
data RoseTreeFamily :: * -> * -> * where
  Node'          ::        RoseTreeFamily RoseTree (Cons Int (Cons [RoseTree] Nil))      
  Leaf'          ::        RoseTreeFamily RoseTree (Cons Int Nil)
  RList          :: 
  RInt'          :: Int -> RoseTreeFamily Int Nil  
  
  
  
instance Family RoseTreeFamily where
  decEq Node'    Node'           = Just (Refl, Refl)
  decEq Leaf' Leaf'              = Just (Refl, Refl)
  decEq (RInt' x) (RInt' y)   | x == y     = Just (Refl, Refl)
                              | otherwise  = Nothing
  decEq _ _                                = Nothing

  fields Node' (Node e e') = Just (CCons (e) (CCons e' CNil))
  fields Leaf' (Leaf i) = Just (CCons i CNil)
  fields (RInt' _) _ = Just CNil
  fields _ _ = Nothing

  apply Node' (CCons e (CCons e' CNil)) = Node e e'
  apply Leaf' (CCons i CNil) = Leaf i
  apply (RInt' i) CNil = i

  string Node'       = "Add"
  string Leaf' = "Primitive"
  string (RInt' i)   = show i

instance Type RoseTreeFamily RoseTree where
  constructors = [Concr Node', Concr Leaf']

instance Type RoseTreeFamily [RoseTree] where
    constructors = [Concr RInt']

instance Type RoseTreeFamily Int where
  constructors = [Abstr RInt']

-}
  
 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
    
    