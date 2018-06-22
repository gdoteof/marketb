module Handler.React where

import Import

getReactR :: [Text] -> Handler Html
getReactR _ = do
    -- serve the file app/dist/index.html
    sendFile typeHtml "static/app/dist/index.html"
