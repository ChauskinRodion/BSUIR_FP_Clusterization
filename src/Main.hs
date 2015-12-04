module Main where
--import System.Environment

import Arguments
import Options.Applicative

main :: IO ()
main = do 
  let opts = info (helper <*> arguments) (fullDesc) 
  execParser opts >>= startProcess
    

startProcess :: Arguments -> IO ()
startProcess (Arguments in_file
                        out_file
                        clusters
                        precision
                        csv_separator
                        csv_skip_first_column
                        csv_skip_last_column
                        csv_skip_first_row
                        ) = do
  print $ "in_file: "               ++ in_file
  print $ "out_file: "              ++ out_file
  print $ "clusters: "              ++ show(clusters)
  print $ "precision: "             ++ show(precision)
  print $ "csv_separator: "         ++ csv_separator
  print $ "csv_skip_first_column: " ++ show(csv_skip_first_column)
  print $ "csv_skip_last_column: "  ++ show(csv_skip_last_column)
  print $ "csv_skip_first_row: "    ++ show(csv_skip_first_row)
  