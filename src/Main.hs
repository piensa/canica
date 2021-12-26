{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE RecursiveDo #-}
module Main where

import Hylogen.WithHylide (Vec4, vec4, mouse, time, x_, uvN, y_)
import Clay (Css, body, (?), background, black, green, border, dashed, yellow, px, compact, render,
             color, green)

import Data.Text.Lazy (unpack)
import Data.Text as Text (Text, unlines, pack)
import Control.Lens ((.~))

import Reflex.Dom.Old (MonadWidget, El)
import Reflex.Dom.Widget.Basic (elAttr', blank, text, el,
                                dynText, divClass)
import Reflex.Dom.Builder.Class (DomBuilder, _element_raw)
import Reflex.Dom.Widget.Input (textArea, textAreaConfig_initialValue, def, _textArea_value)
import Reflex.Dom.Main (mainWidgetWithHead)
import Reflex.Dynamic (updated, current, holdDyn)
import Reflex (performEvent, leftmost, tag, newTriggerEvent,
               getPostBuild, Dynamic
               )
import Language.Javascript.JSaddle.Warp (run)

import GHCJS.DOM.Document
import GHCJS.DOM.Types hiding (Text)
import GHCJS.DOM.HTMLCanvasElement
import GHCJS.DOM.WebGLRenderingContextBase
import GHCJS.DOM.CanvasRenderingContext2D
import GHCJS.DOM (currentDocumentUnchecked)
import qualified GHCJS.DOM.EventTargetClosures as DOM (EventName, unsafeEventName)
import Language.Javascript.JSaddle.Object (new, jsg)

-- Writes GLSL, without sharing
toGLSL' :: Vec4 -> Text
toGLSL' v = Text.unlines [ "void main() {"
                     , "    gl_FragColor = " <> show v <> ";"
                     , "}"
                     ]


vertexShaderSource :: Text
vertexShaderSource =
  "attribute vec2 a_position;\
  \void main() {\
  \  gl_Position = vec4(a_position, 0, 1);\
  \}"


colorShader :: Vec4
colorShader = vec4 (a, b, c, d)
  where
    a = 1
    b = 0
    c = 0
    d = 1

trivialFragmentShader :: Text
trivialFragmentShader = toGLSL' colorShader

-- | An example fragment shader program, drawing a red circle
trivialFragmentShaderOld :: Text
trivialFragmentShaderOld = Text.unlines
  [ "precision mediump float;"
  , "uniform vec2 u_windowSize;"
  , "void main() {"
  , "  float s = 2.0 / min(u_windowSize.x, u_windowSize.y);"
  , "  vec2 pos = s * (gl_FragCoord.xy - 0.5 * u_windowSize);"
  , "  // pos is a scaled pixel position, (0,0) is in the center of the canvas"
  , "  // If the position is outside the inscribed circle, make it transparent"
  , "  if (length(pos) > 1.0) { gl_FragColor = vec4(0,0,0,0); return; }"
  , "  // Otherwise, return red"
  , "  gl_FragColor = vec4(1.0,2.0,0.0,1.0);"
  , "}"
  ]

onOffScreenCanvas :: MonadDOM m => HTMLCanvasElement -> (HTMLCanvasElement -> m ()) -> m ()
onOffScreenCanvas onScreen paint = do
  doc <- currentDocumentUnchecked
  offScreen <- createElement doc ("canvas" :: JSString)
        >>= unsafeCastTo HTMLCanvasElement

  getWidth onScreen >>= setWidth offScreen
  getHeight onScreen >>= setHeight offScreen

  paint offScreen

  ctxRaw <- getContextUnsafe onScreen ("2d"::Text) ([]::[()])
  ctx <- unsafeCastTo CanvasRenderingContext2D ctxRaw
  drawImage ctx offScreen 0 0
  return ()


paintGL :: MonadDOM m => (Maybe Text -> m ()) -> Text -> HTMLCanvasElement -> m ()
paintGL printErr fragmentShaderSource canvas = do
  -- adaption of
  -- https://blog.mayflower.de/4584-Playing-around-with-pixel-shaders-in-WebGL.html

  getContext canvas ("experimental-webgl"::Text) ([]::[()]) >>= \case
    Nothing -> do
      -- jsg "console" ^. js1 "log" (gl ^. js1 "getShaderInfoLog" vertexShader)
      return ()
    Just glRaw -> do
      gl <- unsafeCastTo WebGLRenderingContext glRaw

      w <- getDrawingBufferWidth gl
      h <- getDrawingBufferHeight gl
      viewport gl 0 0 w h

      buffer <- createBuffer gl
      bindBuffer gl ARRAY_BUFFER (Just buffer)
      array <- liftDOM (new (jsg ("Float32Array"::Text))
            [[ -1.0, -1.0,
                1.0, -1.0,
               -1.0,  1.0,
               -1.0,  1.0,
                1.0, -1.0,
                1.0,  1.0 :: Double]])
        >>= unsafeCastTo Float32Array
      let array' = uncheckedCastTo ArrayBuffer array
      bufferData gl ARRAY_BUFFER (Just array') STATIC_DRAW

      vertexShader <- createShader gl VERTEX_SHADER
      shaderSource gl (Just vertexShader) vertexShaderSource
      compileShader gl (Just vertexShader)
      -- jsg "console" ^. js1 "log" (gl ^. js1 "getShaderInfoLog" vertexShader)

      fragmentShader <- createShader gl FRAGMENT_SHADER
      shaderSource gl (Just fragmentShader) fragmentShaderSource
      compileShader gl (Just fragmentShader)
      -- jsg "console" ^. js1 "log" (gl ^. js1 "getShaderInfoLog" fragmentShader)
      err <- getShaderInfoLog gl (Just fragmentShader)
      printErr err

      program <- createProgram gl
      attachShader gl (Just program) (Just vertexShader)
      attachShader gl (Just program) (Just fragmentShader)
      linkProgram gl (Just program)
      useProgram gl (Just program)
      -- jsg "console" ^. js1 "log" (gl ^. js1 "getProgramInfoLog" program)

      positionLocation <- getAttribLocation gl (Just program) ("a_position" :: Text)
      enableVertexAttribArray gl (fromIntegral positionLocation)
      vertexAttribPointer gl (fromIntegral positionLocation) 2 FLOAT False 0 0
      -- liftJSM $ jsg ("console"::Text) ^. js1 ("log"::Text) program

      windowSizeLocation <- getUniformLocation gl (Just program) ("u_windowSize" :: Text)
      uniform2f gl (Just windowSizeLocation) (fromIntegral w) (fromIntegral h)

      drawArrays gl TRIANGLES 0 6
      return ()

webglcontextrestored :: DOM.EventName HTMLCanvasElement WebGLContextEvent
webglcontextrestored = DOM.unsafeEventName "webglcontextrestored"

webglcontextlost :: DOM.EventName HTMLCanvasElement WebGLContextEvent
webglcontextlost = DOM.unsafeEventName "webglcontextlost"

fragmentShaderCanvas ::
    (MonadWidget t m) =>
    (Map Text Text) ->
    Dynamic t Text ->
    m (Dynamic t (Maybe Text))
fragmentShaderCanvas attrs fragmentShaderCanvasRaw
    = snd <$> fragmentShaderCanvas' attrs fragmentShaderCanvasRaw

fragmentShaderCanvas' ::
    (MonadWidget t m) =>
    (Map Text Text) ->
    Dynamic t Text ->
    m (El t, Dynamic t (Maybe Text))
fragmentShaderCanvas' attrs fragmentShaderSource = do
  (canvasEl, _) <- elAttr' "canvas" attrs $ blank
  (eError, reportError) <- newTriggerEvent
  pb <- getPostBuild

  domEl <- unsafeCastTo HTMLCanvasElement $ _element_raw canvasEl

  {-
  eContextBack <- wrapDomEvent domEl (`on` webglcontextrestored) (return ())
  eContextLost <- wrapDomEvent domEl (`on` webglcontextlost)     preventDefault

  performEvent $ (<$> eContextLost) $ \() -> do
    liftJSM $
        jsg ("console"::Text) ^. js1 ("log"::Text) ("lost" :: Text)

  performEvent $ (<$> eContextBack) $ \() -> do
    liftJSM $
        jsg ("console"::Text) ^. js1 ("log"::Text) ("back" :: Text)
  -}

  let eDraw = leftmost
                [ updated fragmentShaderSource
                , tag (current fragmentShaderSource) pb
  --              , tag (current fragmentShaderSource) eContextBack
                ]

  performEvent $ (<$> eDraw) $ \src -> do
    onOffScreenCanvas domEl $ paintGL (liftIO . reportError) src

  dErr <- holdDyn Nothing eError
  return (canvasEl, dErr)


main :: IO ()
main = run 3708 $ do
   mainWidgetWithHead htmlHead $ mdo
    inp <- divClass "left" $ do
        inpRaw <- textArea $ def
           & textAreaConfig_initialValue .~ trivialFragmentShader
        divClass "error" $ dynText (maybe "" id <$> dError)
        return inpRaw

    dError <- divClass "right" $ fragmentShaderCanvas
        -- Here we determine the resolution of the canvas
        -- It would be desireable to do so dynamically, based on the widget
        -- size. But Reflex.Dom.Widget.Resize messes with the CSS layout.
        -- _ -- constDyn ("width" =: "1000") -- (mconcat [ "width"  =: "1000" , "height" =: "1000" ])
        (fromList [("width","1000"), ("height", "1000")])
        (_textArea_value inp)
    return ()
  where
    htmlHead :: DomBuilder t m => m ()
    htmlHead = do
        el "style" (text cssOld)
        el "style" (text css)
        el "title" (text "Fragment Shader")


clayCss::Css
clayCss = body ?
    do background  black
       color       green
       border      dashed (px 2) yellow

css :: Text
css = Text.pack $ unpack $ (render clayCss)

cssOld :: Text.Text
cssOld = Text.unlines
    [ "html {"
    , "  margin: 0;"
    , "  height: 100%;"
    , "}"
    , "body {"
    , "  display: flex;"
    , "  height: 100%;"
    , "}"
    , ".left {"
    , "  flex:1 1 0;"
    , "  display:flex;"
    , "  flex-direction:column;"
    , "  padding:1em;"
    , "}"
    , ".right {"
    , "  flex:1 1 0;"
    , "}"
    , "textarea {"
    , "  resize:vertical;"
    , "  height:50%;"
    , "}"
    , ".error {"
    , "  margin-top:1em;"
    , "  text-align: left;"
    , "  font-family:mono;"
    , "  width:100%;"
    , "  overflow: auto;"
    , "}"
    , "canvas {"
    , "  height:100%;"
    , "  width:100%;"
    , "  object-fit:contain;"
    , "}"
    ]
