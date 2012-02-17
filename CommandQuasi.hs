{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module CommandQuasi where
import Parser
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Text.Parsec


parseExpr p (file, line, col) s =
    case runParser p () "" s of
      Left err  -> fail $ show err
      Right e   -> return e
 
cmd  :: QuasiQuoter
cmd  =  QuasiQuoter quoteStateExp undefined undefined undefined

quoteStateExp :: String -> TH.ExpQ
quoteStateExp s =  do  loc <- TH.location
                       let pos =  (TH.loc_filename loc,
                                     fst (TH.loc_start loc),
                                     snd (TH.loc_start loc))
                       state <- parseExpr parse_command pos s
                       dataToExpQ (const Nothing) state
                       
cmds  :: QuasiQuoter
cmds  =  QuasiQuoter quoteStateExps undefined undefined undefined

quoteStateExps :: String -> TH.ExpQ
quoteStateExps s =  do  loc <- TH.location
                        let pos =  (TH.loc_filename loc,
                                        fst (TH.loc_start loc),
                                        snd (TH.loc_start loc))
                        state <- parseExpr parse_commands pos s
                        dataToExpQ (const Nothing) state