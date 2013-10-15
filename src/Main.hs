import Import
import Handler.Home
import Handler.Fib
import Handler.Markdown
import Handler.Hello
import Handler.Aeson

mkYesodDispatch "App" resourcesApp

main :: IO ()
main = warpEnv App