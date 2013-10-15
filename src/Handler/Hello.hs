module Handler.Hello where

import Import

getHelloR :: String -> Handler Value
getHelloR i = return $ object ["value" .= i]