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





-}