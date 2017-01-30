data Exp = Constant Int 
         | Variable String
         | Minus Exp Exp 
         | Greater Exp Exp 
         | Times Exp Exp 
         deriving Show

data Com = Assign String Exp 
         | Seq Com Com 
         | Cond Exp Com Com 
         | While Exp Com 
         | Declare String Exp Com 
         | Print Exp 
         deriving Show


type Name = String
type Location = Int
type Value = Int 
type Index = [String]
type Stack = [Value]

position :: Name -> Index -> Location         
position name index = pos 1 index 
   where pos n (h:t) = if name == h
                       then n 
                       else pos (n+1) t 

fetch :: Location -> Stack -> Value
fetch n (h:t) = if n == 1 then h else fetch (n-1) t  
