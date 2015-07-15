--this is main.hs for www.fpcompleate.com
--Fractal are included from week2

module Main where

import Yesod
import Render (render)
import Fractals (drawMandelbrot, screen)
import Codec.Picture (encodePng, generateImage)
import Data.ByteString.Lazy.Internal (ByteString)

data App = App
instance Yesod App

mkYesod "App" [parseRoutes|
              / HomeR GET
              /hello HelloR GET
              /alex AlexR GET
              /hey/#String HeyR GET
              /mandelbrot MandelbrotR GET
              /mandelbrot/#Int/#Int MandelbrotWithSizeR GET
              |]

getHomeR = defaultLayout $ [whamlet| <p> Hello World! |]

getHeyR myname = defaultLayout $ [whamlet| <h1> Hello #{myname}! |]

getAlexR = defaultLayout $ [whamlet| <h1> Hello Alex! |]

mandelbrotHelper :: Int -> Int -> ByteString
mandelbrotHelper x y = encodePng $ generateImage (render (drawMandelbrot (fromIntegral x) (fromIntegral y))) x y

mandelbrotRHlp :: MonadHandler m => Int -> Int -> m TypedContent
mandelbrotRHlp x y = sendResponse $ toTypedContent (typePng, toContent $ mandelbrotHelper x y)

getMandelbrotR :: MonadHandler m => m TypedContent
getMandelbrotR = mandelbrotRHlp (fst screen) (snd screen)

getMandelbrotWithSizeR :: MonadHandler m => Int -> Int -> m TypedContent
getMandelbrotWithSizeR x y = mandelbrotRHlp (fromIntegral x) (fromIntegral y)

getHelloR :: MonadHandler m => m TypedContent
getHelloR = sendResponse $ toTypedContent (typePlain, toContent "Say Haskell!")

main = warpEnv App