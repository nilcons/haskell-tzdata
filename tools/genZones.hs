import Control.Applicative
import Control.Monad hiding (join)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Function (on)
import Data.List
import Data.Maybe
import Data.String.Utils (replace, join)
import System.Directory
import System.FilePath.Find
import System.Environment
import System.IO.Unsafe

data TZFile
  = Reg String FilePath
  | Link String String
  deriving (Eq, Show)

data TZDesc
  = RegD { _name :: String, _label :: String, _desc :: BL.ByteString }
  | LinkD { _name :: String, _target :: String }
  deriving (Eq,Show)

-- TODO(klao): remove when/if https://github.com/bos/filemanip/pull/4
-- is merged and released.
canonicalPath' :: FindClause FilePath
canonicalPath' = (unsafePerformIO . canonicalizePath) `liftM` filePath

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
          target <- canonicalPath'
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
nameToLabel = replace "-" "_" . replace "+" "" . replace "/" "__"

labelDecl :: [TZDesc] -> String
labelDecl zones = "= " ++ join "\n  | " (go zones)
  where
    go [] = []
    go (RegD _ label _ : zs) = label : go zs
    go (LinkD _ _ : zs) = go zs

descriptionList :: [TZDesc] -> String
descriptionList = join ",\n      " . map f
  where
    f (LinkD name target) = "l " ++ show name ++ " " ++ show target
    f (RegD name label desc) = "p " ++ show name ++ " " ++ label ++ " " ++ show (BL.unpack desc)

genCode :: FilePath -> FilePath -> [TZDesc] -> IO ()
genCode templatePath outputPath zones = do
  template <- readFile templatePath
  let
    code = replace "TZ_DESCRIPTIONS" (descriptionList zones)
           $ replace "TZ_LABEL_DECL" (labelDecl zones) template
  writeFile outputPath code

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
    _ -> putStrLn "usage: getZones <zoneinfo-dir> <template> <output>"
