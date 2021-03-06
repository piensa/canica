{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Main where


import Hylogen.WithHylide
import Data.Profunctor
import Hylogen.Expr

import Clay (Css, body, (?), background, black, green, border, dashed, yellow, px, render,
             color, green, flex, display, height, html,
              margin, pct, column, direction, padding, em, 
              flexDirection, flexBasis, width, marginTop, 
              textAlign, fontFamily, overflow, auto, left, canvas,
               textarea, Auto (auto), matchParent, TextAlign)
import Clay.Flexbox as CF

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

import GHCJS.DOM.Document ( createElement )
import GHCJS.DOM.Types
    ( Element(Element),
      liftDOM,
      uncheckedCastTo,
      unsafeCastTo,
      JSString,
      ArrayBuffer(ArrayBuffer),
      CanvasRenderingContext2D(CanvasRenderingContext2D),
      Float32Array(Float32Array),
      HTMLCanvasElement(HTMLCanvasElement),
      MonadDOM,
      RenderingContext(RenderingContext),
      WebGLContextEvent,
      WebGLRenderingContext(WebGLRenderingContext) )
import GHCJS.DOM.HTMLCanvasElement
    ( getContext,
      getContextUnsafe,
      getHeight,
      getWidth,
      setHeight,
      setWidth )

import GHCJS.DOM.WebGLRenderingContextBase as WGL
import GHCJS.DOM.WebGLRenderingContextBase
    ( attachShader,
      bindBuffer,
      bufferData,
      compileShader,
      createBuffer,
      createProgram,
      createShader,
      drawArrays,
      enableVertexAttribArray,
      getAttribLocation,
      getDrawingBufferHeight,
      getDrawingBufferWidth,
      getShaderInfoLog,
      getUniformLocation,
      linkProgram,
      shaderSource,
      uniform2f,
      useProgram,
      vertexAttribPointer,
      viewport )
import GHCJS.DOM.CanvasRenderingContext2D ( drawImage )
import GHCJS.DOM (currentDocumentUnchecked)
import qualified GHCJS.DOM.EventTargetClosures as DOM (EventName, unsafeEventName)
import Language.Javascript.JSaddle.Object (new, jsg)
import qualified UnliftIO.Process as Clay.Text.TextAlign


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
      bindBuffer gl WGL.ARRAY_BUFFER (Just buffer)
      array <- liftDOM (new (jsg ("Float32Array"::Text))
            [[ -1.0, -1.0,
                1.0, -1.0,
               -1.0,  1.0,
               -1.0,  1.0,
                1.0, -1.0,
                1.0,  1.0 :: Double]])
        >>= unsafeCastTo Float32Array
      let array' = uncheckedCastTo ArrayBuffer array
      bufferData gl WGL.ARRAY_BUFFER (Just array') STATIC_DRAW

      vertexShader <- createShader gl WGL.VERTEX_SHADER
      shaderSource gl (Just vertexShader) vertexShaderSource
      compileShader gl (Just vertexShader)
      -- jsg "console" ^. js1 "log" (gl ^. js1 "getShaderInfoLog" vertexShader)

      fragmentShader <- createShader gl WGL.FRAGMENT_SHADER
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

      drawArrays gl WGL.TRIANGLES 0 6
      return ()

webglcontextrestored :: DOM.EventName HTMLCanvasElement WebGLContextEvent
webglcontextrestored = DOM.unsafeEventName "webglcontextrestored"

webglcontextlost :: DOM.EventName HTMLCanvasElement WebGLContextEvent
webglcontextlost = DOM.unsafeEventName "webglcontextlost"

fragmentShaderCanvas ::
    (MonadWidget t m) =>
    Map Text Text ->
    Dynamic t Text ->
    m (Dynamic t (Maybe Text))
fragmentShaderCanvas attrs fragmentShaderCanvasRaw
    = snd <$> fragmentShaderCanvas' attrs fragmentShaderCanvasRaw

fragmentShaderCanvas' ::
    (MonadWidget t m) =>
    Map Text Text ->
    Dynamic t Text ->
    m (El t, Dynamic t (Maybe Text))
fragmentShaderCanvas' attrs fragmentShaderSource = do
  (canvasEl, _) <- elAttr' "canvas" attrs blank
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
                , Reflex.tag (current fragmentShaderSource) pb
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
        el "style" (text css)
        el "style" (text "canvas {object-fit: contain;}")
        el "style" (text "textarea {resize: vertical;}")
        el "title" (text "Fragment Shader")


clayCss::Css
clayCss = html ? 
      do margin (px 0) (px 0) (px 0) (px 0)
         height (pct 100)
         body ?
            do  background  black
                display Clay.flex 
                height (pct 100)
         ".left" ?
            do  
                display Clay.flex
                padding (em 1) (em 0) (em 0) (em 0)   
                CF.flex 1 1 (em 0)
                CF.flexDirection (FlexDirection "column")
         ".right" ?
            do  
                CF.flex 1 1 (em 0)
         textarea ?
            do
-- TODO: Figure out how to do it properly              
--                "resize" "vertical"
                height (pct 50)
         canvas ?
            do
                height (pct 100)
                width (pct 100)
-- TODO: Figure out how to do it properly              
--                "object-fit" "contain"                

         ".error" ?
            do
                marginTop (em 1)
                textAlign matchParent
                width (pct 100)
                overflow auto


css :: Text
css = Text.pack $ unpack $ render clayCss

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


type Optic p s t a b = p a b -> p s t
type Optic' p a b = Optic p a b a b
type Iso s t a b = forall p. (Profunctor p) => Optic p s t a b
type Iso' a b = Iso a b a b
type Fold r s t a b = Optic (Forget r) s t a b
type Getter s t a b = Fold a s t a b
view :: forall s t a b. Getter s t a b -> s -> a
view l = runForget (l (Forget id))

norm :: (Floating a) => Iso' a a
norm = dimap (\x -> x * 0.5 + 0.5) (\x -> x * 2 - 1)

rep :: forall n. Veccable n => Vec n ->  Vec n -> Vec n
rep c p = mod_  p c - 0.5 * c

gate :: (Veccable n) => Vec n -> Vec n -> Vec n -> Vec n
gate s e x = ((x `geq` s) * (x `lt` e)) ??? (1, 0)

(???) :: (ToGLSLType a) => Booly -> (Expr a, Expr a) -> Expr a
(???) c (a, b) = sel c a b

queryTransformer :: (Vec2 -> Vec4) -> Vec2 -> Vec4
queryTransformer x = x
  & lmap (view norm)
  -- & lmap (^*(1 - x_ audio))
  -- & rgbF ((sin (rand time)) * 0.02)
  -- & lmap (\x -> g $ vec2(x_ x, y_ x))

g x = x
  & (\x -> x - rep 0.1 x)


-- rgbF :: Vec1 -> (Vec2 -> Vec4) -> (Vec2 -> Vec4)
rgbF :: Vec1 -> Optic' (->) Vec2 Vec4
rgbF offset q pos = vec4 (r, g, b, a)
  where
    r = q (pos + copy offset) & x_
    g = q pos & y_
    b = q (pos - copy offset) & z_
    a = q pos & w_


bb = bbqF (texture2D backBuffer) uvN
bbqF x = x
  & lmap (view norm)
  & lmap (*0.9)
  & rgbF 0.1
  & rmap desat & rmap desat & rmap desat & rmap desat
  & rmap (bpf 0.7 0.1)

desat = hsv $ modY (*0.9)

bpf pos delta = hsv (modX (clamp (pos - delta) (pos + delta)))

-- fixme: make polymorphic
modX :: (Vec1 -> Vec1) -> (Vec4 -> Vec4)
modX f v = vec4 (f (x_ v), y_ v, z_ v, w_ v)
modY :: (Vec1 -> Vec1) -> (Vec4 -> Vec4)
modY f v = vec4 (x_ v, f (y_ v), z_ v, w_ v)
modZ :: (Vec1 -> Vec1) -> (Vec4 -> Vec4)
modZ f v = vec4 (x_ v, y_ v, f (z_ v), w_ v)
modW :: (Vec1 -> Vec1) -> (Vec4 -> Vec4)
modW f v = vec4 (x_ v, y_ v, z_ v, f (w_ v))


less :: (Floating a) => Optic' (->) a a
less = dimap (id) (*0.1)

hsv :: Optic' (->) (Vec4) (Vec4)
hsv = dimap rgb2hsv hsv2rgb

v = vqF vq uvN

vqF x = x
  & lmap (\x -> x - rep 0.5 x)
  & lmap (\x -> x - rep 0.5 x)
  & lmap (cos)
  & lmap (*(10))
  & lmap (+(vec2 (0, less id time)))
  & lmap (clamp (-1) 1)

vq uv = y_ uv * 2  + sin (x_ uv + time)

colorShader :: Vec4
-- colorShader = vec4 (v, v, v, 1) & mix 0.1 bb
colorShader = vec4 (1, 1, 1, 1)

trivialFragmentShader :: Text
trivialFragmentShader = toGLSL' colorShader

