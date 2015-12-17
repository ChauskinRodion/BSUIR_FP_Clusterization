{-# LANGUAGE ScopedTypeVariables #-}

import Arguments
import qualified FCM
import Options.Applicative

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as CSV
import qualified Data.Vector as V

import qualified Data.Matrix as M
import qualified Data.Either
import System.Random
import Data.Char


main :: IO ()
main = do
  args <- ioArgs
  startProcess args
  where opts = info (helper <*> arguments) (fullDesc)
        ioArgs = execParser(opts)

startProcess :: Arguments -> IO ()
startProcess args = do
  printDebugParams args
  objects <- parseObjects args
  std_gen <- getStdGen
  

  let result = FCM.run std_gen args objects
      resultMatrix = M.fromLists result
      file = out_file args    
  if file /= "STDOUT" then toFile file resultMatrix else toStdout resultMatrix



toStdout :: (Show a) => M.Matrix a -> IO ()
toStdout result = print(result)


toFile :: (Show a) => String -> M.Matrix a -> IO ()
toFile filename result = writeFile filename lines
                where lines = M.prettyMatrix result    

printDebugParams :: Arguments -> IO()
printDebugParams params = do
  print $ "in_file: "               ++ in_file params
  print $ "out_file: "              ++ out_file params
  print $ "clusters: "              ++ show(clusters params)
  print $ "precision: "             ++ show(precision params)
  print $ "csv_separator: "         ++ csv_separator params
  print $ "csv_skip_first_column: " ++ show(csv_skip_first_column params)
  print $ "csv_skip_last_column: "  ++ show(csv_skip_last_column params)
  print $ "csv_skip_first_row: "    ++ show(csv_skip_first_row params)


parseObjects :: Arguments -> IO [[Double]]
parseObjects params = do
  content <- BL.readFile(in_file params)
  case CSV.decodeWith csvDelimiter skip_head_row content :: Either String (V.Vector [String]) of
    Left err -> do
      print err
      return []
    Right csvMatrix -> do
      let parseString = (\x -> [read y :: Double | y <- cutColumns x skip_first skip_last])
          skip_first = csv_skip_first_column params
          skip_last = csv_skip_last_column params
      return $ Prelude.map parseString $ V.toList csvMatrix
  where
      csvDelimiter = CSV.DecodeOptions { CSV.decDelimiter = fromIntegral $ ord $ head $ csv_separator params }
      skip_head_row = if csv_skip_first_row params then CSV.HasHeader else CSV.NoHeader

cutColumns :: [String] -> Bool -> Bool -> [String]
cutColumns array first_col last_col
  | first_col = cutColumns (tail array) False last_col
  | last_col = init array
  | otherwise = array