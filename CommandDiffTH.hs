module CommandDiffTH where
import Language.Haskell.TH
import Control.Applicative
import Data.List
import Control.Monad

reify_data_dec name = do
    (TyConI dec) <- reify name
    return dec
    
make_family_gadt :: Name -> Q [Dec]
make_family_gadt name = do
    all_constructor <- recursively_get_types name
    let gadt_constr     = concatMap convert_to_gadt_constrs all_constructor
        gadt_name       = mkName $ (show name) ++ "Fam"
    (:[]) <$> mk_gadt gadt_name gadt_constr
    
collect_constr :: Dec -> Q [(Name, [Con])]
collect_constr (DataD _ name _ cons _) = do
    children <- concat <$> mapM collect_constr_decs cons 
    return ((name, cons):children)
    
collect_constr_decs :: Con -> Q [(Name, [Con])]
collect_constr_decs (NormalC _ stys) = concat <$> (mapM look_con_args $ map (\(_, typ) -> typ) stys)
collect_constr_decs (RecC    _ vtys) = concat <$> (mapM look_con_args $ map (\(_, _, typ) -> typ) vtys)

look_con_args :: Type -> Q [(Name, [Con])]
look_con_args (ConT name) = recursively_get_types name
    
recursively_get_types :: Name -> Q [(Name, [Con])]
recursively_get_types name | is_primitive name = return []
recursively_get_types name | otherwise = do
    dec <- reify_data_dec name
    collect_constr dec

is_primitive name = any (\x -> isInfixOf x (show name)) ["Int", "Char"]

convert_to_gadt_constrs (name, cons) = map (mk_gadt_con name) cons

mk_gadt_con name (NormalC c_name stys) = mk_gadt_con' name $ map (\(_, typ) -> typ) stys 
mk_gadt_con name (RecC    c_name vtys) = mk_gadt_con' name $ map (\(_, _, typ) -> typ) vtys 

mk_gadt_con' name c_name tys = ForallC [] [EqualP (VarT $ mkName "a") $ ConT name, 
    EqualP (VarT $ mkName "b") $ foldl' AppT  tys
    (NormalC (mkName ((show name) ++ "'")) [])
    
mk_gadt_constr_context = 

mk_gadt             = undefined

