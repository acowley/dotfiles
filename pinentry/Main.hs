import Control.Arrow (second)
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)
import System.Posix.Process (getProcessID)
import System.Process (callCommand, readProcess)

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
pinEntry pe = getLine >>= uncurry aux . second (drop 1) . break isSpace
  where k = putStrLn "OK" >> hFlush stdout >> pinEntry pe
        k' pe' = putStrLn "OK" >> hFlush stdout >> pinEntry pe'
        aux "SETDESC" x = k' pe { desc = Just (clean x) }
        aux "SETPROMPT" x = k' pe { prompt = Just x }
        aux "SETOK" x = k' pe { ok = Just x }
        aux "SETERROR" x = k' pe { errorMsg = Just x }
        aux "GETPIN" _ =
          do let sexp = mkSexp (fromMaybe "DESC" (desc pe))
                               (fromMaybe "PROMPT" (prompt pe))
                               (fromMaybe "OK" (ok pe))
                               (fromMaybe "ERROR" (errorMsg pe))
             r <- readProcess "/usr/local/bin/emacsclient"
                          [ "-e", sexp]
                          []
             putStrLn $ "D " ++ takeWhile (/= '"') (dropWhile (== '"') r)
             k
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

main :: IO ()
main = do putStrLn "OK"
          hFlush stdout
          pinEntry $ PinEntry Nothing Nothing Nothing Nothing Nothing Nothing
