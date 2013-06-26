import Import
import Handler.Home
import Handler.Fib
import Handler.Markdown

mkYesodDispatch "App" resourcesApp
    
main :: IO ()
main = warpEnv App