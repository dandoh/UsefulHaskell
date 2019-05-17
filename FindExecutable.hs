import qualified Data.Text        as T
import qualified Data.Text.IO     as TextIO
import           Text.Regex.Posix
import System.Environment (getArgs)



main :: IO ()
main = do
    args <- getArgs
    if length args > 0 then do
        let fp = head args
        content <- TextIO.readFile fp
        let m = (T.unpack content) =~ "executables:[\n\t ]+([a-zA-Z-]+):" :: (String, String, String, [String])
        let (_, _, _, programName:_) = m
        print $ programName
    else
        print "Please provide file path"
