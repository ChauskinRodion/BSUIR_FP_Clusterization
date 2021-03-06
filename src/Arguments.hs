module Arguments where

import Options.Applicative

data Arguments = Arguments
  { in_file :: String
  , out_file :: String
  , clusters :: Int
  , precision :: Double
  , random_matrix :: Bool
  , algorytm :: String
  , csv_separator :: String
  , csv_skip_first_column :: Bool
  , csv_skip_last_column :: Bool
  , csv_skip_first_row :: Bool}

arguments :: Parser Arguments
arguments = Arguments
    <$> strOption
         ( long "input"
        <> short 'i'
        <> metavar "FILE"
        <> help "Data FILE" )
    <*> strOption
         ( long "out"
        <> short 'o'
        <> metavar "OUT FILE"
        <> value "STDOUT"
        <> help "Write output to FILE" )         
    <*> option auto
         ( long "clusters"
        <> short 'c'
        <> metavar "CLUSTERS COUNT"
        <> value 3
        <> help "Count of clusters. Default 3" )
    <*> option auto
         ( long "precision"
        <> short 'p'
        <> metavar "PRECISION"
        <> value 0.001
        <> help "Precision of calculations. Default 0.001" )
    <*> switch
         ( long "random-matrix"
        <> help "Skip first column of csv file." )                  
    <*> strOption
         ( long "algorytm"
        <> short 'a'
        <> metavar "Euclidean(E) or Hamming(H). Default E"
        <> value "A"
        <> help "Write output to FILE" )         
    <*> strOption
         ( long "csv-separator"
        <> metavar "SEPARATOR"
        <> value ","
        <> help "Values separator for csv file. Default ',' " )
    <*> switch
         ( long "csv-ignore-first-column"
        <> help "Skip first column of csv file." )
    <*> switch
         ( long "csv-ignore-last-column"
        <> help "Skip last column of csv file." )         
    <*> switch
         ( long "csv-ignore-first-row"
        <> help "Skip head row of csv file." )
