{-# LANGUAGE CPP #-}
-- Build with:
-- nix-shell -p 'haskellPackages.ghcWithPackages (p: [p.directory p.time])'
-- ghc Main.hs -o hpinentry
import Control.Arrow (second)
import Control.Exception (bracket_)
import Control.Monad (when)
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import Data.Time
import System.Directory (doesFileExist, removeFile, renameFile)
import System.Exit (exitSuccess, ExitCode(..))
import System.IO (hFlush, stdout, hGetEcho, hSetEcho, stdin)
import System.Posix.Files (getFileStatus, fileSize)
import System.Posix.Process (getProcessID, executeFile)
import System.Posix.Types (FileOffset)
import System.Process (callCommand, readProcess, readProcessWithExitCode)

data PinEntry = PinEntry { desc      :: Maybe String
                         , prompt    :: Maybe String
                         , ok        :: Maybe String
                         , errorMsg  :: Maybe String
                         , touchFile :: Maybe String
                         , tty       :: Maybe String }

-- | The Assuan protocol uses percent escaping for ASCII control
-- codes.
clean :: String -> String
clean [] = []
clean ('%':x:y:xs) = toEnum (read ("0x"++[x,y])::Int) : clean xs
clean ('"':xs) = "\\\"" ++ clean xs
clean (x:xs) = x : clean xs

-- Build the command we pass to emacs
mkSexp :: String -> String -> String -> String -> String
mkSexp d p o e =
  "(pinentry-emacs \""++
  d++"\" \""++p++"\" \""++o++"\" \""++e++"\")"

pinEntry :: PinEntry -> IO ()
pinEntry pe = getLine >>= uncurry aux' . second (drop 1) . break isSpace
  where k = putStrLn "OK" >> hFlush stdout >> pinEntry pe
        k' pe' = putStrLn "OK" >> hFlush stdout >> pinEntry pe'
        aux' cmd arg = logMsg ("Got command "++cmd++" with argument " ++ arg)
                       >> aux cmd arg
        aux "SETDESC" x = k' pe { desc = Just (clean x) }
        aux "SETPROMPT" x = k' pe { prompt = Just x }
        aux "SETOK" x = k' pe { ok = Just x }
        aux "SETERROR" x = k' pe { errorMsg = Just x }
        aux "GETPIN" _ =
          do let sexp = mkSexp (maybe "DESC" sanitize (desc pe))
                               (fromMaybe "PROMPT" (prompt pe))
                               (fromMaybe "OK" (ok pe))
                               (fromMaybe "ERROR" (errorMsg pe))
             logMsg ("Asking emacs to evaluate: " ++ sexp)
             sout <- readProcess
                       "/Users/acowley/.nix-profile/bin/emacsclient"
                       ["-e", sexp]
                       []
             logMsg "emacs finished"
             let pw = takeWhile (/= '"') (dropWhile (== '"') sout)
             putStrLn ("D " ++ pw) >> k
        aux "OPTION" x = do
          case second (drop 1) $ break (== '=') x of
            ("ttyname", t) -> k' pe { tty = Just t }
            ("touch-file", f) -> k' pe { touchFile = Just f }
            _ -> k
        aux "BYE" _ = do putStrLn "OK"
                         case touchFile pe of
                           Nothing -> exitSuccess
                           Just f -> do callCommand ("touch "++f)
                                        exitSuccess
        aux "GETINFO" x =
          do case x of
               "pid" -> getProcessID >>= putStrLn . ("D "++) . show
               _ -> return ()
             k
        aux _ _ = k
        sanitize = foldMap (\c -> if c == '"'
                                     then "\\\""
                                     else [c])

emacsServerRunning :: IO Bool
emacsServerRunning =
  do (code, _, _) <- readProcessWithExitCode
                       "emacsclient"
                       ["-a", "false", "-e", "0"]
                       ""
     return (code == ExitSuccess)

#ifndef LOGGING
main :: IO ()
main =
  do putStrLn "OK"
     hFlush stdout
     pinEntry $ PinEntry Nothing Nothing Nothing Nothing Nothing Nothing

logMsg :: String -> IO ()
logMsg _ = return ()

#else

getFileSize :: String -> IO FileOffset
getFileSize path = do
    stat <- getFileStatus path
    return (fileSize stat)

logMsg :: String -> IO ()
logMsg msg =
  do t <- getCurrentTime
     let stamp = formatTime defaultTimeLocale "%F %X" t
     appendFile logFile ('[': stamp ++ "] " ++ msg ++ "\n")
     sz <- getFileSize logFile
     when (sz > 1024*1024) $
       do bakExists <- doesFileExist old
          when bakExists (removeFile old)
          renameFile logFile old
  where logFile = "/tmp/hpinentry-log"
        old = "/tmp/hpinentry-log-old"

main :: IO ()
main =
  do logMsg "Starting hpinentry"
     go <- emacsServerRunning
     if True || go
       then do logMsg "emacs server is running"
               putStrLn "OK"
               hFlush stdout
               pinEntry $ PinEntry Nothing Nothing Nothing Nothing Nothing Nothing
       else do logMsg "exec'ing pinentry-curses"
               executeFile "pinentry-curses" True [] Nothing
#endif
