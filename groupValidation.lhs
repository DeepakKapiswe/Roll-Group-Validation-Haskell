> import Data.Char
> import Data.List
> import qualified Data.Map as M
> import qualified Data.Text as T

> data Group = Group {applier::Int,memberList::[Int]} deriving (Show,Eq)

> makeGroup::[Int]->Group
> makeGroup (x:xs) = Group x xs 

> makeGroupList::String->M.Map Int [Int]
> makeGroupList = M.fromAscList .fmap (\x->(applier x,memberList x)).sortOn applier.(makeGroup.makeRollList <$>).breakStr

> breakStr::String->[T.Text]
> breakStr = filter (not.T.null).T.split (flip elem delems).T.pack

> delems = "#"
> u = undefined 

> makeRollList::T.Text->[Int]
> makeRollList =fmap (read.T.unpack).nub.filter (\x->T.any isDigit x && T.length x==rollLength).T.groupBy (\x y -> isDigit x && isDigit y)

> rollLength=5

> input = do 
>  s <- readFile "groups01-1.txt"
>  return s
