module CLI where
import Options.Applicative

data HashOptions = HashOptions
  { hInputFp :: FilePath
  , hBound :: Int
  , hThreshold :: Int
  , hCorpusFp :: FilePath
  , hOCorpusFp :: FilePath
  , hOutputFp :: FilePath
  }

data SimOptions = SimOptions
  { sTargetFp :: FilePath
  , sQueryFp :: FilePath
  , sCorpusFp :: FilePath
  , sCorpus :: String
  , sBound :: Int
  , sThreshold :: Int
  , sOptBound :: Int
  , sSaveCorpus :: Bool 
  , sRawCorpus :: Bool
  } 

data GenCorpusOptions = GenCorpusOptions
  { gCorpus :: String
  , gCorpusFp :: FilePath
  , gOptBound :: Int 
  , gLargeCorpus :: Bool
  , gAppendCorpus :: Bool
  , gRawCorpus :: Bool
  }

data MergeCorpusOptions = MergeCorpusOptions
  { mCorpora :: [FilePath]
  , mCorpusFp :: FilePath
  , mOptBound :: Int
  , mAppendCorpus :: Bool
  , mRawCorpus :: Bool
  }

data EvalSimOptions = EvalSimOptions
  { eTargetFp :: FilePath
  , eNegative :: String 
  , ePositive :: String
  , eExcludePositive :: Bool
  , eSaveFp :: FilePath
  , eCorpusFp :: FilePath
  , eBound :: Int
  , eThreshold :: Int
  , eAcc :: Int
  , eASaveFp :: FilePath
  , eCached :: Bool
  }

data PrintSummaryOptions = PrintSummaryOptions
  { pCorpusFp :: FilePath
  , pOutputFp :: FilePath
  }
 
rawCorpusParser :: Parser Bool
rawCorpusParser = switch
                       ( long "raw"
                         <> short 'r'
                         <> help "Whether to save the corpus summary as a list of hashes rather than a frequency map (only available for unoptimized corpus)" )

cachedParser :: Parser Bool
cachedParser = switch
                       ( long "cached"
                         <> short 'c'
                         <> help "Save and load the hashing result of PATH/EXE in ./_cerrCache/PATH/EXE")


optBoundParser :: Parser Int
optBoundParser = option auto
              ( long "opt"
                <> metavar "NAT"
                <> value 1
                <> showDefault
                <> help "Optimize the corpus by disregarding any strands with frequency NAT or less (use --bound 0 to generate an unoptimized corpus)")


boundParser :: Parser Int
boundParser = option auto
              ( long "bound"
                <> short 'b'
                <> metavar "NAT"
                <> value 0
                <> help "If provided, use compressed similarity (at given bound) instead of Gitz similarity")

thresholdParser :: Parser Int
thresholdParser = option auto
              ( long "threshold"
                <> short 'h'
                <> metavar "PERCENT"
                <> value 0
                <> help "If provided, ignore bound option and find a bound to use compressed similarity that keeps at least % of the strands found in target")


printSummaryOptionsParser :: Parser PrintSummaryOptions
printSummaryOptionsParser = PrintSummaryOptions
                            <$> strOption
                            ( long "summary"
                            <> short 's'
                            <> metavar "SUMMARY"
                            <> help "Location of the corpus summary")
                            <*> strOption
                            ( long "output"
                            <> short 'o'
                            <> metavar "FILE.csv"
                            <> value ""
                            <> help "Where to save the output (if provided)")

printSummaryParser :: ParserInfo PrintSummaryOptions
printSummaryParser = info (printSummaryOptionsParser <**> helper)
                     ( fullDesc
                       <> progDesc "Print a corpus summary as CSV"
                       <> header "cerridwen - inspired by Gitz")

evalSimOptionsParser :: Parser EvalSimOptions
evalSimOptionsParser = EvalSimOptions
                <$> strOption
                ( long "target"
                  <> short 't'
                  <> metavar "TARGET"
                  <> help "Location of the target executable")
                <*> strOption
                ( long "negative"
                  <> short 'n'
                  <> metavar "NEGATIVE"
                  <> help "Location (regex) of executables that are dissimilar to the target")
                <*> strOption
                ( long "positive"
                  <> short 'p'
                  <> metavar "POSITIVE"
                  <> help "Location (regex) of executables that are similar to the target")
                <*> switch
                ( long "exclude"
                  <> short 'e'
                  <> help "Whether to remove the files matching POSITIVE (and TARGET) from NEGATIVE")
                <*> strOption
                ( long "output"
                  <> value ""
                  <> metavar "FILE.csv"
                  <> help "Where to save the computed similarity scores (if provided)")
                <*> strOption
                ( long "summary"
                  <> short 's'
                  <> metavar "SUMMARY"
                  <> help "Location where the corpus summary is located")
                <*> boundParser
                <*> thresholdParser
                <*> option auto
                ( long "accuracy"
                  <> short 'a'
                  <> metavar "PERCENT"
                  <> value 0
                  <> help "If provided, compute and calculate the accuracy of the similarity score when using PERCENT% as threshold for similarity")
                <*> strOption
                ( long "aoutput"
                  <> value ""
                  <> metavar "FILE.csv"
                  <> help "Where to save the computed accuracy scores (if provided)")

                <*> cachedParser
                

evalSimParser :: ParserInfo EvalSimOptions
evalSimParser = info (evalSimOptionsParser <**> helper)
             ( fullDesc
             <> progDesc "Evaluate the ability of searching for a target accross a range of queries"
             <> header "cerridwen - inspired by Gitz")
              
 
 

appendCorpusParser :: Parser Bool
appendCorpusParser = switch
                     ( long "append"
                       <> short 'a'
                       <> help "Whether to append the corpus to an existings (raw) corpus summary file" )


genCorpusOptionsParser :: Parser GenCorpusOptions
genCorpusOptionsParser = GenCorpusOptions
                         <$> strOption
                         ( long "corpus"
                           <> metavar "CORPUS"
                           <> help "Pattern for executables to be included in corpus")
                         <*> strOption
                         ( long "summary"
                           <> metavar "SUMMARY"
                           <> help "Location where the corpus summary should be saved")
                         <*> optBoundParser
                         <*> switch
                         ( long "large"
                           <> short 'l'
                           <> help "Whether to discover each executable and save their strands in the summary separately. Useful for very large corpus/processes" )
                         <*> appendCorpusParser
                         <*> rawCorpusParser

genCorpusParser :: ParserInfo GenCorpusOptions
genCorpusParser = info (genCorpusOptionsParser <**> helper)
             ( fullDesc
             <> progDesc "Generate a corpus summary from a collection of executables"
             <> header "cerridwen - inspired by Gitz")

mergeCorpusOptionsParser :: Parser MergeCorpusOptions
mergeCorpusOptionsParser = MergeCorpusOptions
                         <$> some (strOption
                                   ( long "input"
                                     <> short 'i'
                                     <> metavar "SUMMARY"
                                     <> help "Location of a corpus summary"))
                         <*> strOption
                             ( long "output"
                               <> short 'o'
                               <> metavar "SUMMARY"
                               <> help "Location where the merged summary should be saved")
                         <*> optBoundParser
                         <*> appendCorpusParser
                         <*> rawCorpusParser

mergeCorpusParser :: ParserInfo MergeCorpusOptions
mergeCorpusParser = info (mergeCorpusOptionsParser <**> helper)
             ( fullDesc
             <> progDesc "Merge multiple (unoptimized) corpus summaries"
             <> header "cerridwen - inspired by Gitz")

hashOptionsParser :: Parser HashOptions
hashOptionsParser = HashOptions
                    <$> strOption
                    ( long "input"
                      <> short 'i'
                      <> metavar "EXECUTABLE"
                      <> help "Location of executable to be hashed")
                    <*> boundParser
                    <*> thresholdParser
                    <*> strOption
                    ( long "summary"
                      <> metavar "SUMMARY"
                      <> value ""
                      <> help "Which corpus summary to use if -b or -t provided")
                    <*> strOption
                    ( long "osummary"
                      <> metavar "SUMMARY"
                      <> value ""
                      <> help "Where to save the summary (if provided)")
                    <*> strOption
                    ( long "output"
                      <> short 'o'
                      <> value ""
                      <> metavar "FILE.csv"
                      <> help "Where to save the output (if provided)")

simOptionsParser :: Parser SimOptions
simOptionsParser = SimOptions
                    <$> strOption
                    ( long "target"
                      <> metavar "TARGET"
                      <> help "Location of the target executable")
                    <*> strOption
                    ( long "query"
                      <> metavar "QUERY"
                      <> help "Location of the query executable")
                    <*> strOption
                    ( long "summary"
                      <> metavar "SUMMARY"
                      <> value ""
                      <> help "Location of the corpus summary")
                    <*> strOption
                    ( long "corpus"
                      <> metavar "CORPUS"
                      <> value ""
                      <> help "Pattern for executables to be included in corpus")
                    <*> boundParser
                    <*> thresholdParser
                    <*> optBoundParser
                    <*> switch
                       ( long "save"
                         <> short 's'
                         <> help "Whether to save the generated corpus (ignored without --corpus)")
                    <*> rawCorpusParser


simParser :: ParserInfo SimOptions
simParser = info (simOptionsParser <**> helper)
             ( fullDesc
             <> progDesc "Find the similary between a two executables w.r.t. a corpus"
             <> header "cerridwen - inspired by Gitz")

hashParser :: ParserInfo HashOptions
hashParser = info (hashOptionsParser <**> helper)
             ( fullDesc
             <> progDesc "Compute a list of hashes for the strands found in input executable"
             <> header "cerridwen - inspired by Gitz")

            
data Command
  = Sim SimOptions
  | Hash HashOptions
  | GenCorpus GenCorpusOptions
  | MergeCorpus MergeCorpusOptions
  | EvalSim EvalSimOptions
  | PrintSummary PrintSummaryOptions

parseSim :: ParserInfo Command
parseSim = Sim <$> simParser

parseHash :: ParserInfo Command
parseHash = Hash <$> hashParser


parseGenCorpus :: ParserInfo Command
parseGenCorpus = GenCorpus <$> genCorpusParser

parseMergeCorpus :: ParserInfo Command
parseMergeCorpus = MergeCorpus <$> mergeCorpusParser

parseEvalSim :: ParserInfo Command
parseEvalSim = EvalSim <$> evalSimParser

parsePrintSummary :: ParserInfo Command
parsePrintSummary = PrintSummary <$> printSummaryParser
  
commandParser :: Parser Command
commandParser = subparser $             
  command "sim" parseSim
  <> command "hash" parseHash
  <> command "gen" parseGenCorpus
  <> command "merge" parseMergeCorpus
  <> command "eval" parseEvalSim
  <> command "print" parsePrintSummary

parseCommand :: ParserInfo Command
parseCommand = info (commandParser <**> helper)
             ( fullDesc
             <> progDesc "Interact with the Cerridwen API"
             <> header "cerridwen - inspired by Gitz")
