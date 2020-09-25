import Distribution.Simple
import System.Environment
import System.Process

main :: IO ()
main = do
  result <- getEnvironment
  putStrLn (unlines (map (\(a, b) -> a ++ "=" ++ b) result))
  putStrLn "Compiling sources to generated-src. You'll need Java, Gradle, and unzip set up for this to work."
  result <- readProcessWithExitCode "../typechecker/gradlew" ["-p", "../typechecker", "distZip"] ""
  putStrLn (show result)
  result <- readProcessWithExitCode "unzip" ["-o", "../typechecker/build/distributions/lac.zip"] ""
  putStrLn (show result)
  result <- readProcessWithExitCode "./lac/bin/lac" ["hs", "--home=../typechecker/src/test/resources", "\".*\"", "generated-src"] ""
  putStrLn (show result)
  defaultMain
  defaultMain
