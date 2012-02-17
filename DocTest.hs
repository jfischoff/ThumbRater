module DocTest where
    
import Text.PrettyPrint.Free

doc = result where
    result = text "hey" </> (indent 2 $ text "indent 2") </> (indent 1 $ text "indent 1")

main = putStr (displayS (renderCompact doc) "")