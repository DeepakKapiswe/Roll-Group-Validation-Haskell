> module ValidateGroups where
> import Data.Char
> import Data.List
> import Data.Monoid
> import qualified Data.Map as M
> import qualified Data.Text as T

> data Group = Group {applier::Int,memberList::[Int]} deriving (Eq,Ord)
> instance Show Group where
>  show= show.sort.groupMembers
>
> type RollNum = Int

> makeGroup::[RollNum]-> Group
> makeGroup (x:xs) = Group x xs

> makeGroupList::String->[Group]
> makeGroupList = sortOn applier.(makeGroup.makeRollList <$>).breakStr

> breakStr::String->[T.Text]
> breakStr = filter (not.T.null).T.split (flip elem delems).T.pack

> delems = "#"

> makeRollList::T.Text->[RollNum]
> makeRollList = fmap (read.T.unpack).nub.filter (\x->T.any isDigit x && T.length x==rollLength).T.groupBy (\x y -> isDigit x && isDigit y)
>  where rollLength=5

> sepBySize::[Group]->[[Group]]
> sepBySize x = _sepBySize x [] [] [] []

> _sepBySize::[Group]->[Group]->[Group]->[Group]->[Group]->[[Group]]
> _sepBySize [] s3 s2 s1 invalids =nub<$> [s3,s2,s1,invalids]
> _sepBySize (x:xs) s3 s2 s1 invalids |y x>2  = _sepBySize xs s3 s2 s1 (invalids <> [x])
>                                     |y x==0 = _sepBySize xs s3 s2 (s1 <> [x]) invalids
>                                     |y x==1 = _sepBySize xs s3 (s2 <> [x]) s1 invalids
>                                     |y x==2 = _sepBySize xs (s3 <> [x]) s2 s1 invalids
>  where
>   y = length.memberList

> -- | Gives the members of a group
> groupMembers::Group->[RollNum]
> groupMembers (Group x xs) = x:xs

> -- | checks whether both of the group are Isomorphic i.e all members are same
> isIsomorphic::Group->Group->Bool
> isIsomorphic x y = let [x',y'] = (sort.groupMembers)<$>[x,y]
>                    in x'==y'
> -- | validate a particular group by size whether all group members agree or not
> --   with the help of the hypothesis that if we have a group of n members then we
> --   must have atleast n isomorphic groups present with different applier
> validateAllMembersAgree::Int->[Group]->[Group]
> validateAllMembersAgree size xs = [x|x<-xs,(length.filter (isIsomorphic x)$xs)==size]


> separateNotAgreeingGroups::Int->[Group]->([Group],[Group])
> separateNotAgreeingGroups size groupList = let valids = validateAllMembersAgree size groupList
>                                                invalids = groupList \\ valids
>                                            in  (valids,invalids)

> -- | generates the zip of occurences of a particular member 
> generateCountList::[Group]->[(RollNum,Int)]
> generateCountList = M.toAscList.M.fromListWith (+).((\x->(x,1))<$>).mconcat.(groupMembers <$>)


> -- | gives list of other groups which belong to the family of 
> --   the given group means those groups which contains atleast
> --   one same member
> getGroupFamily::Group->[Group]->[Group]
> getGroupFamily g = delete g.filter (isIntersectingGroup g)

> isIntersectingGroup::Group->Group->Bool
> isIntersectingGroup g1 g2 = intersect (groupMembers g1) (groupMembers g2) /= []

> sepConflictingGroups::[Group]->([Group],[Group])
> sepConflictingGroups xs =let conflictingGroups =[y|y<-xs,getGroupFamily y xs /=[]]
>                              nonConflictingGroups =[y|y<-xs,getGroupFamily y xs ==[]]
>                          in (nonConflictingGroups,conflictingGroups)

> -- | calculates the loss quantised in terms of rollno we are lossing if we select a particular
> --   group from a list of groups
> calculateLoss::[Group]->Group->Int
> calculateLoss groupList g = let countList = generateCountList groupList
>                                 membersInDangerCountList = filter (\(r,c)->elem r membersInDanger) countList
>                                 membersInDanger = mconcat $groupMembers <$> getGroupFamily g groupList
>                             in length.filter (\(r,c)->c<1).M.toList$foldr (\(a,b)->M.insertWith (\x y->y-x) a b) (M.fromList membersInDangerCountList)$ (\x->(x,1))<$>  membersInDanger
>

> -- | The core function to resolve conflicts when a rollno appears in different groups
> --   it first finds the family of groups in which the given rollno is a member then calculates
> --   or compares the associated loss with the choices if that is going to be selected
> --   if any of the loss is 0 then that particular choice would get selected else the choice with
> --   minimum loss would be selected and also if multiple choices have same loss differnt from 0 
> --   then we can't solve the conflict arbitrarily we would say it inconsistent
> conFlictResolution::[Group]->[Group]->[([Group],String)]->([Group],[([Group],String)])
> conFlictResolution [] valids conflicts = (valids,conflicts)
> conFlictResolution (e:es) valids conflicts = case getGroupFamily e (e:es) of
>  [] -> conFlictResolution es (e:valids) conflicts
>  gFamily -> case isConsistent (e:gFamily) (e:es) of
>              Nothing -> conFlictResolution (es \\ gFamily) valids (((e:gFamily),"Can't Resolve The Conflict Between Groups due to multiple presence of the same member"):conflicts )
>              Just (selected,toBeDiscarded) -> conFlictResolution ((e:es) \\ (selected:toBeDiscarded)) (selected:valids) ((toBeDiscarded,"Conflicting Groups Discarded in favour of " <> show selected):conflicts)


> -- | Checks whether the group family have consistent solution or not as described above
> isConsistent::[Group]->[Group]->Maybe (Group,[Group])
> isConsistent gFamily gList = case (sortOn fst$(\x->(calculateLoss gList x,x))<$> gFamily) of
>  (0,choice1):xs -> Just (choice1,filter (isIntersectingGroup choice1).delete choice1 $gList)
>  (x,sol1):(y,sol2):xs |x==y -> Nothing
>  (x,choice1):xs -> Just (choice1,filter (isIntersectingGroup choice1).delete choice1 $gList)
>

> -- | validateGroups applies whole logic to the input String
> validateGroups::String-> IO String
> validateGroups inputStr= do
>  let groupList = makeGroupList inputStr
>      [g3,g2,g1,invalidSizedGroups] = sepBySize groupList
>      (agreeingGrps,notAgreeingGroups) = (\(x,y)->(nubBy isIsomorphic.mconcat $x,mconcat y)).unzip$zipWith separateNotAgreeingGroups [3,2] [g3,g2]
>      (purelyValids,conflictingGroups) = sepConflictingGroups (g1++agreeingGrps)
>      (resolved,conflicts) = conFlictResolution conflictingGroups [] []
>      validResult x  |x==[] =[]
>                     |otherwise = ("valid Groups ::\n" <>).unlines.(show <$>).sort $ x
>      invalidResult x| x==[]=[]
>                     |otherwise =("\nInvalid Sized Groups ::\n" <>). unlines$ show <$> x
>      notAgreeingResult x |x==[]=[]
>                          |otherwise =("\nInvalidated due to not agreement of all Group Members ::\n" <>). unlines$show <$> x
>      conflictResults x | x==[]=[]
>                        | otherwise =unlines.(["Invalid Conflicting Groups ::"]<>)$ (\(gps,msg)-> unlines $msg:(show<$>gps))<$>x
>  return.unlines.filter (/="") $ [validResult$purelyValids ++ resolved,invalidResult invalidSizedGroups,notAgreeingResult notAgreeingGroups,conflictResults conflicts]
