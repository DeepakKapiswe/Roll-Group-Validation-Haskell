> module Main where

> import ValidateGroups
> import System.Environment
> import Control.Exception

> main :: IO ()
> main = do
>  args <- getArgs
>  if args == [] then exitWithArgError  else processArg (head args)

> processArg::FilePath -> IO ()
> processArg fname = do
>  file <- safeLoadFile fname
>  case file of
>   Left e -> exitWithFileError $ "Can't Open The File :" ++ fname
>   Right content -> do result <- validateGroups content
>                       writeFile (fname++".report") result
>

> exitWithArgError :: IO ()
> exitWithArgError = do
>  putStrLn "usage: you must supply one argument"
>  putStrLn "in ghci you may supply the arguments in the following way:"
>  putStrLn ":main <arg>"

> exitWithFileError ::String-> IO ()
> exitWithFileError msg= do
>  putStrLn msg

> safeLoadFile :: FilePath -> IO (Either IOException String)
> safeLoadFile f = (Right <$> readFile f) `catch` (\ e -> pure (Left e) )
