module Days.Day7 where 

import Utils

import Data.List
import Data.List.Split

data INode   = File String Int | Dir Folder deriving (Eq, Show)
data Folder  = Folder {fname :: String, inodes :: [INode], size :: Int} deriving (Eq, Show)
data Context = Context {cname :: String, left :: [INode], right :: [INode]} deriving (Eq, Show)
data Zipper  = Zipper {cwd :: Folder, contexts :: [Context]} deriving (Eq, Show)

data Command = Cd String | Ls [String] deriving (Eq, Show)

isDir :: INode -> Bool
isDir (Dir _) = True 
isDir _       = False

moveUp :: Zipper -> Zipper 
moveUp Zipper{cwd=f, contexts=c:cs} = 
    let upperInodes = (left c) ++ [Dir f] ++ (right c)
    in Zipper{cwd=Folder{fname=cname c, inodes=upperInodes, size=0}, contexts=cs} 

moveAllUp :: Zipper -> Folder 
moveAllUp Zipper{cwd=f, contexts=[]} = f 
moveAllUp z = moveAllUp $ moveUp z

parseNode :: String -> INode
parseNode s =
    let [left, right] = words s 
    in if left == "dir"
         then Dir Folder{fname=right, inodes=[], size=0}
         else File right (read left)

parseCommand :: String -> Command 
parseCommand s | length (lines s) == 1 = Cd $ (words s)!!1
               | otherwise = Ls (tail $ lines s)

matchingFolder :: String -> INode -> Bool 
matchingFolder s (Dir Folder{fname=t}) | s == t = True 
matchingFolder _ _ = False

runStep :: Command -> Zipper -> Zipper 
runStep (Cd "..") z = moveUp z
runStep (Cd s) z@Zipper{cwd=f, contexts=cs} = 
    let (before, (Dir current):after) = break (matchingFolder s) $ inodes $ cwd z
    in  Zipper{cwd=current, contexts=(Context{cname=fname f, left=before, right=after}):cs}
runStep (Ls objects) z@Zipper{cwd=f} = z{cwd=f{inodes=map parseNode objects}}

initialState :: Zipper
initialState = Zipper{cwd=Folder{fname="/", inodes=[], size=0}, contexts=[]}

sizeFolder :: Folder -> Folder
sizeFolder f =
    let (subfolders, files)  = partition isDir $ inodes f 
        subSized             = map (\(Dir f') -> sizeFolder f') subfolders
        subSum               = sum $ map size subSized
        fileSum              = sum $ map (\(File _ i) -> i) $ files
        folderSum            = fileSum + subSum
    in f{size=folderSum, inodes=files ++ map Dir subSized}

sizeList :: Folder -> [Int]
sizeList f =
    let subfolders  = map (\(Dir f') -> f') $ filter isDir $ inodes f 
    in (size f):(concatMap sizeList subfolders)

run :: String -> (String, String)
run s = 
    let commands     = tail $ map parseCommand $ tail $ splitOn "$" s
        tree         = moveAllUp $ foldl (flip runStep) initialState commands
        sized        = sizeFolder tree
        allSizes     = sizeList sized
        toFree       = 30000000 - (70000000 - size sized)
        ans1         = sum $ filter (< 100000) allSizes
        ans2         = minimum $ filter (>= toFree) allSizes
    in both show (ans1, ans2)