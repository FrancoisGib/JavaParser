import System.IO
import Parser
import System.Environment
import JavaParser

main :: IO ()
main = do
    args <- getArgs
    result <- tryReadFile (head args)
    case result of
        Just content -> do
            let parsed = parse content
            putStrLn parsed
        Nothing -> putStrLn "Erreur : Impossible de lire le fichier."

tryReadFile :: FilePath -> IO (Maybe String)
tryReadFile fileName = do
    content <- readFile fileName
    return (Just content)
