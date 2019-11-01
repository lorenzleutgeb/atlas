import Distribution.Simple
import System.Process

main :: IO ()
main = do
  putStrLn "Compiling sources to generated-src. You'll need Java, Gradle, and unzip set up for this to work."
  result <- readProcess "../typechecker/gradlew" ["-p", "../typechecker", "distZip"] ""
  putStrLn result
  result <- readProcess "unzip" ["-o", "../typechecker/build/distributions/lac.zip"] ""
  putStrLn result
  result <- readProcess "../typechecker/build/distributions/lac/bin/lac" ["hs", "--home=../typechecker/src/test/resources", "\".*\"", "generated-src"] ""
  putStrLn result
  defaultMain
