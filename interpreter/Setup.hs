import Distribution.Simple
import System.Environment
import System.Process

main :: IO ()
main = do
--  result <- getEnvironment
--  putStrLn (unlines (map (\(a, b) -> a ++ "=" ++ b) result))
  result <- readProcess "./generate.sh" [] ""
  putStrLn result
  defaultMain
