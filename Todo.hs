{-

I need something that is like my type but shows the differences

it applies a edit list but shows the diffs

so if I had the type

I think that I am making something called 

diff show type class

I think I should start by writing compress

-}

data P = P

data TaggedADT a = Sum a [TaggedADT a]
                 | Product a [TaggedADT a]
                 | Primitive a P 

data Change = Del 
            | Ins
            | Cpy
            
class DiffType a where
    


{-

I need to go back to the drawing board here
What I want to do is replace subsitute the tyvars in the typesym

so what is my algo

if it is a typesym 
then 

AppT (ConT blac) (ConT do)

This means that black is defined as
AppT (ConT clack) (VarT a)

So the first step is to replace ConT with is type definition

AppT (AppT (ConT clack) (VarT a)) (ConT do)

then convert to lambdas

App (Lam a (App (Var clack) (Var a))) Var do

Once that is complete when need to reduce

the fundemental

(\x -> black x) do

nf

black do


-}
        











