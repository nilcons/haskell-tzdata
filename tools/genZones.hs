import Control.Applicative
import Control.Monad hiding (join)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Function (on)
import Data.List
import Data.List.Split (splitOn)
import Data.Maybe
import System.Directory
import System.FilePath.Find
import System.Environment

-- Suppress 'redundant import' warning:
import Prelude

-- The following list functions were copied from MissingH, written by John Goerzen.
-- The library is unmaintained, see https://github.com/haskell-hvr/missingh/issues/54
-- It is BSD-3-Clause licensed.
-- They were modified to use 'splitOn' from the 'split' package.

{- | Given a list and a replacement list, replaces each occurance of the search
list with the replacement list in the operation list.
Example:
>replace "," "." "127,0,0,1" -> "127.0.0.1"
-}
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new = join new . splitOn old

{- | Given a delimiter and a list of items (or strings), join the items
by using the delimiter.
Example:
> join "|" ["foo", "bar", "baz"] -> "foo|bar|baz"
-}
join :: [a] -> [[a]] -> [a]
join delim = concat . intersperse delim

data TZFile
  = Reg String FilePath
  | Link String String
  deriving (Eq, Show)

data TZDesc
  = RegD { _name :: String, _label :: String, _desc :: BL.ByteString }
  | LinkD { _name :: String, _target :: String }
  deriving (Eq,Show)

collect :: FilePath -> IO [TZFile]
collect dir0 = do
  dir <- (++ "/") <$> canonicalizePath dir0
  let
    relname = fromJust . stripPrefix dir
    add :: [TZFile] -> FileInfo -> [TZFile]
    add l = evalClause $ do
      ftype <- fileType
      fp <- filePath
      let name = relname fp
      case ftype of
        RegularFile -> return $ Reg name fp : l
        SymbolicLink -> do
          target <- canonicalPath
          return $ Link name (relname target) : l
        _ -> return l

  fold always add [] dir

toDesc :: TZFile -> IO TZDesc
toDesc (Link name target)
  = return $ LinkD name target
toDesc (Reg name file) = do
  desc <- BL.readFile file
  return $ RegD name (nameToLabel name) desc

nameToLabel :: String -> String
nameToLabel = replace "-" "_" . replace "+" "'" . replace "/" "__"

mkSomeTZLabelCases :: [String] -> String
mkSomeTZLabelCases = join "\n  " . map f
  where
    f label = label ++ " -> SomeTZLabel (Proxy :: Proxy '" ++ label ++ ")"

mkKnownTZLabelInstances :: [String] -> String
mkKnownTZLabelInstances = join "\n\n" . map f
  where
    f label = "instance KnownTZLabel '" ++ label ++ " where\n  tzLabelVal _ = " ++ label

mkTZDescriptions :: [TZDesc] -> String
mkTZDescriptions = join ",\n      " . map f
  where
    f (LinkD name target) = "l " ++ show name ++ " " ++ show target
    f (RegD name label desc) = "p " ++ show name ++ " " ++ label ++ " " ++ show (BL.unpack desc)

mkTZLabelConstructors :: [String] -> String
mkTZLabelConstructors = (++) "= " . join "\n  | "

genCode :: FilePath -> FilePath -> [TZDesc] -> IO ()
genCode templatePath outputPath zones = do
  template <- readFile templatePath
  let
    code = replace "TZ_CASES" (mkSomeTZLabelCases labels)
         $ replace "TZ_INSTANCES" (mkKnownTZLabelInstances labels)
         $ replace "TZ_DESCRIPTIONS" (mkTZDescriptions zones)
         $ replace "TZ_LABEL_DECL" (mkTZLabelConstructors labels) template
  writeFile outputPath code
  where
    labels = go zones
    go [] = []
    go (RegD _ label _ : zs) = label : go zs
    go (LinkD _ _ : zs) = go zs

sumSize :: [TZDesc] -> Int
sumSize = sum . map s
  where
    s (LinkD name target) = length name + length target
    s (RegD name label desc) = length name + length label + fromIntegral (BL.length desc)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [dir, template, output] -> do
      zones0 <- collect dir
      zones <- sortBy (compare `on` _name) <$> mapM toDesc zones0
      putStrLn $ "Approximate size of the data: " ++ show (sumSize zones)
      genCode template output zones
    _ -> putStrLn "usage: genZones <zoneinfo-dir> <template> <output>"
