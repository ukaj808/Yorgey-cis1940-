module Party where

import Data.List (sort)
import qualified Data.Tree as T
import Data.Tuple (swap)
import Employee
  ( Employee(Emp)
  , GuestList(GL)
  , empFun
  , empName
  , testCompany
  , testCompany2
  , testCompany3
  )
import System.IO (IO)
import Text.XHtml (input, treeColors)

instance Semigroup GuestList where
  (<>) (GL emps1 fun1) (GL emps2 fun2) = GL (emps1 ++ emps2) (fun1 + fun2)

instance Monoid GuestList where
  mempty = GL [] 0

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL emps fun) = GL (emp : emps) (empFun emp + fun)

glConsBoss :: Employee -> GuestList -> GuestList
glConsBoss boss (GL emps fun) = GL (boss : emps) (empFun boss)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ fun1) gl2@(GL _ fun2) =
  if fun1 > fun2
    then gl1
    else gl2

combineGls :: GuestList -> GuestList -> GuestList
combineGls (GL emps fun) (GL emps' fun') = GL (emps ++ emps') (fun + fun')

-- 1. takes a tree as input
-- 2. folds the subforest of the given tree
-- 3. the function we fold with recurses into each tree in the subforest
--
-- 1. the return type
-- 2. How to combine results
-- 3. the starting value of the accumulator
treeFold' :: (T.Tree a -> b) -> (b -> b -> b) -> b -> T.Tree a -> b
treeFold' f g e tr = foldr z e (T.subForest tr)
  where
    z x = g (f x)

treeSize :: T.Tree a -> Int
treeSize tr = foldr (\x y -> treeSize x + y) 1 (T.subForest tr)

treeSize' :: T.Tree a -> Int
treeSize' = treeFold' treeSize' (+) 1

treeSum :: T.Tree Int -> Int
treeSum tr = foldr (\x y -> treeSum x + y) (T.rootLabel tr) (T.subForest tr)

treeSum' :: T.Tree Int -> Int
treeSum' tr = treeFold' treeSum' (+) (T.rootLabel tr) tr

treeDepth :: T.Tree a -> Int
treeDepth tr = foldr (\x y -> max (treeDepth x + 1) y) 1 (T.subForest tr)

treeDepth' :: T.Tree a -> Int
treeDepth' = treeFold' (\x -> treeDepth' x + 1) max 1

flatten :: T.Tree a -> [a]
flatten tr = foldr (\x y -> y ++ flatten x) [T.rootLabel tr] (T.subForest tr)

flatten' :: T.Tree a -> [a]
flatten' tr = treeFold' flatten' (++) [T.rootLabel tr] tr

-- my solution
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss [] = (glCons boss mempty, mempty)
nextLevel boss glts = swap ((mempty, glCons boss mempty) <> mconcat glts)

-- solution found online
nextLevel' :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel' emp gls = (glCons emp secondBest, firstBest)
  where
    firstBest = mconcat . map fst $ gls
    secondBest = mconcat . map snd $ gls

considerBoss :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
considerBoss e gls = (glCons e bestWith, bestWithout)
  where
    bestWith = mconcat (map snd gls)
    bestWithout = mconcat (map (uncurry moreFun) gls)

-- Yorgeys solution
nextLevel'' :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel'' = considerBoss

pickBetterGuestList :: (GuestList, GuestList) -> GuestList
pickBetterGuestList = uncurry moreFun

maxFunPair :: T.Tree Employee -> (GuestList, GuestList)
maxFunPair (T.Node emp []) = (glCons emp mempty, mempty)
maxFunPair (T.Node emp subtrees) = nextLevel emp (map maxFunPair subtrees)

-- https://github.com/sarabander/cis194/blob/master/08/Party.hs (solution for maxFun found here.)
maxFun :: T.Tree Employee -> GuestList
maxFun = pickBetterGuestList . maxFunPair

totalFunLn :: GuestList -> String
totalFunLn (GL _ fun) = "Total fun: " ++ show fun

employeeNameLn :: Employee -> String
employeeNameLn emp = empName emp

getEmployees :: GuestList -> [Employee]
getEmployees (GL emps fun) = emps

instance Ord Employee where
  compare x y = compare (empName x) (empName y)
  (<) x y = (<) (empName x) (empName y)
  (<=) x y = (<=) (empName x) (empName y)
  (>) x y = (>) (empName x) (empName y)
  (>=) x y = (>=) (empName x) (empName y)

sortByFirstName :: GuestList -> GuestList
sortByFirstName gl@(GL [] _) = gl
sortByFirstName (GL emps fun) = GL (sort emps) fun

getCompanyTree :: String -> T.Tree Employee
getCompanyTree str = read str

main :: IO ()
main =
  readFile "./company.txt" >>= (\txt -> return (getCompanyTree txt)) >>=
  (\tree -> return (maxFun tree)) >>=
  (\bestGuestList ->
     putStrLn (totalFunLn bestGuestList) >>
     mapM_ putStrLn (map empName (getEmployees (sortByFirstName bestGuestList))))
