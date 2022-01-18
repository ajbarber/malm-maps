module Main where

import Prelude

import Data.Argonaut.Decode (JsonDecodeError, decodeJson, parseJson)
import Data.Array (deleteBy, range, snoc)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..), maybe, isNothing)
import Data.MediaType (MediaType(..))
import Data.Number.Format (toString)
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Debug (spy, traceM)
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.AVar (empty, new, tryPut, tryTake, tryRead) as EVar
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign.Generic (encodeJSON)
import Math ((%))
import P5 (draw, getP5, setup)
import P5.Color (background2, fill)
import P5.Events.Keyboard (keyIsDown, keyPressed)
import P5.Events.Mouse (mousePressed, mouseReleased)
import P5.Image (image, image2, loadImage)
import P5.Mouse (mouseX, mouseY)
import P5.Rendering (createCanvas)
import P5.Shape (ellipse, line, rect)
import P5.Types (ElementOrImage(..), Image, P5)
import Web.DOM.Element as Element
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.Event (target)
import Web.Event.EventTarget (EventListener, EventTarget, addEventListener, eventListener)
import Web.File.Blob (fromString)
import Web.File.File (toBlob)
import Web.File.FileList (FileList, item)
import Web.File.FileReader.Aff (readAsText)
import Web.File.Url (createObjectURL)
import Web.HTML (window)
import Web.HTML.HTMLDocument as Document
import Web.HTML.HTMLInputElement (files, fromEventTarget)
import Web.HTML.Window (document)
import Web.UIEvent.InputEvent.EventTypes as IETypes

type AppState = {
  p5 :: P5
}

initialState :: Maybe AppState
initialState = Nothing

data Coords = Coords (Maybe Number) (Maybe Number) (Maybe Number) (Maybe Number)

type ControlState = { deleteMode :: Boolean }

withDest
  :: Coords ->
     Number ->
     Number ->
     Coords
withDest (Coords x1 y1 _ _) destx desty =  Coords x1 y1 (Just destx) (Just desty)

rectHeight :: Number
rectHeight = 320.0

type TCoords ={ w :: Number,
               h :: Number,
               xpos :: Number,
               ypos :: Number }

type TCoordMap = { src :: TCoords, dest :: TCoords, wall :: Boolean }

ready :: Coords -> Boolean
ready (Coords (Just x1) (Just y1) (Just x2) (Just y2)) = x2 - x1 > 0.0 && y2 - y1> 0.0
ready (Coords _ _ _ _) = false

asRec :: Coords -> Number -> Number -> TCoordMap
asRec cutSt destX destY = let src = toRec cutSt in
  { src: src, dest: src{ xpos = destX, ypos = destY }, wall: false}

toRec :: Coords -> TCoords
toRec (Coords (Just x1) (Just y1) (Just x2) (Just y2)) = {
  w: x2 - x1,
  h: (y2 - y1) % rectHeight,
  xpos: x1,
  ypos: y1 % rectHeight}
toRec _ = { w: 0.0, h:0.0, xpos: 0.0, ypos: 0.0 }

mouseCoords :: P5 ->  Effect (Tuple Number Number)
mouseCoords p = do
  x <- mouseX p
  y <- mouseY p
  let x' =  x - x % 16.0
  let y' = y - y % 16.0
  pure $ Tuple x' y'

tileCornerEq :: TCoordMap -> TCoordMap -> Boolean
tileCornerEq proxy full = matchXPos proxy full && matchYPos proxy full
  where
    matchXPos p f = (p.dest.xpos == f.dest.xpos)
    matchYPos p f = (p.dest.ypos == f.dest.ypos)

tileEq :: TCoordMap -> TCoordMap -> Boolean
tileEq proxy full = matchXPos proxy full && matchYPos proxy full
  where
    matchXPos p f = (p.dest.xpos >= f.dest.xpos && p.dest.xpos < f.dest.xpos + f.dest.h)
    matchYPos p f = (p.dest.ypos >= f.dest.ypos && p.dest.ypos < f.dest.ypos + f.dest.h)

initCoordMap :: Number -> Number -> TCoordMap
initCoordMap x y = { src: rec' 0.0 0.0, dest: rec' x y, wall: false }
   where
     rec' _x _y = (toRec (Coords Nothing Nothing Nothing Nothing)){ xpos = _x, ypos = _y}

toggleWall :: Number -> Number -> AVar (Array TCoordMap) -> Effect Unit
toggleWall xpos ypos pasteStAVar = do
  mps <- EVar.tryTake pasteStAVar
  case mps of
    Just ps -> do
      let ps' = withWalls <$> ps
      void $ EVar.tryPut ps' pasteStAVar
    Nothing -> pure unit
  where
    withWalls a = if (tileEq (initCoordMap xpos ypos) a) then
                    spy "wall" a {wall = not a.wall} else spy "not wall" a

removeItem :: Number -> Number -> AVar (Array TCoordMap) -> Effect Unit
removeItem xpos ypos pasteStAVar = do
  mps <- EVar.tryTake pasteStAVar
  case mps of
    Just ps -> do
      let ps' = deleteBy tileCornerEq (initCoordMap xpos ypos) ps
      void $ EVar.tryPut ps' pasteStAVar
    Nothing -> pure unit

restoreSt ::
  Coords ->
  Array TCoordMap ->
  AVar (Coords) ->
  AVar (Array TCoordMap) ->
  Effect Boolean
restoreSt cutSt pasteSt cutStAVar pasteStAVar = do
  void $ EVar.tryPut cutSt cutStAVar
  EVar.tryPut pasteSt pasteStAVar

save :: AVar (Array TCoordMap) -> Effect Unit
save pasteStAVar = do
  mps <- EVar.tryRead pasteStAVar
  win <- window
  doc <- document win
  -- get the buttons
  let docAsParent = Document.toParentNode doc
  case mps of
    Just ps -> do
      let blob = fromString (encodeJSON ps) (MediaType "application/json")
      downloadUrl <- createObjectURL blob
      anchor <- querySelector (QuerySelector "#save") docAsParent
      for_ anchor \a -> do
        Element.setAttribute "href" downloadUrl a
        Element.setAttribute "download" downloadUrl a

    Nothing -> log "nothing"

load :: String -> AVar (Array TCoordMap) -> Effect Unit
load buf pasteStAVar = do
  case (decodeJson =<< parseJson buf) of
    Left e -> log "error decoding"
    Right r -> do
      log "loaded successfully"
      void $ EVar.tryTake pasteStAVar
      void $ EVar.tryPut r pasteStAVar

paste ::
  Number ->
  Number ->
  Coords ->
  AVar (Array TCoordMap) ->
  Effect (Array TCoordMap)
paste x y cutSt pasteSt = do
  mps <- EVar.tryTake pasteSt
  case mps of
    Just ps -> do
      traceM (asRec cutSt x y)
      ps <$ case (y <= rectHeight && ready cutSt) of
        true -> EVar.tryPut (snoc ps (asRec cutSt x y)) pasteSt
        false -> EVar.tryPut ps pasteSt
    _ -> do
      [] <$ EVar.tryPut [] pasteSt

renderTile ::
  P5 ->
  Image ->
  TCoordMap ->
  Effect Unit
renderTile p img n = do
  let d = n.dest
  let s = n.src
  image2 p (ElementOrImageImage img) d.xpos d.ypos d.w d.h s.xpos s.ypos (Just s.w) (Just s.h)
  if n.wall then do
    log "wall found"
    (ellipse p (d.xpos + 16.0) d.ypos 12.0 Nothing) else pure unit

renderIcon ::
  P5 ->
  Image ->
  TCoords ->
  Effect Unit
renderIcon p img dest =
  image p
  (ElementOrImageImage img)
  dest.xpos
  dest.ypos
  (Just 16.0)
  (Just 16.0)

-- | cutSt - buffer for the active selection from the tile map.
-- | pasteSt -- state representing what has been pasted. The constructed map in other words. This is
-- | what is saved.
-- | controlSt - state representing the control modes available. Currently just delete mode and default.
main :: Maybe AppState -> Effect (Maybe AppState)
main mAppState = do
  p <- maybe getP5 (\x -> pure x.p5) mAppState
  cutSt <- EVar.empty
  pasteSt <- EVar.new []
  controlSt <- EVar.new { deleteMode: false }
  hookLoadButton pasteSt

  let img = loadImage p "assets/Overworld.png" Nothing Nothing
  let icon = loadImage p "assets/close.png" Nothing Nothing

  setup p do
    _ <- createCanvas p 700.0 700.0 Nothing
    pure unit

  keyPressed p do
    log "here"
    let dPressed = keyIsDown p 68.0
    if dPressed then do
      res <- EVar.tryTake controlSt
      false <$ for_ res \n -> do
        EVar.tryPut { deleteMode: not n.deleteMode } controlSt
    else pure false

  mousePressed p do
    Tuple x y <- mouseCoords p
    log "press"
    save pasteSt
    let belowRect = y >= rectHeight
    mcs <- EVar.tryTake cutSt
    mmode <-EVar.tryRead controlSt
    let deleteMode = maybe false _.deleteMode mmode
    case (Tuple mcs belowRect) of
      Tuple (Just cs) false -> do
        ps <- paste x y cs pasteSt
        restoreSt cs ps cutSt pasteSt
      Tuple Nothing true -> do
          log $ "saving initial cut " <> toString x <> toString y
          EVar.tryPut (Coords (Just x) (Just y) Nothing Nothing) cutSt
      Tuple Nothing false -> do
        toggleWall x y pasteSt
        if (deleteMode) then removeItem x y pasteSt else pure unit
        pure false
      Tuple _ _ -> pure false

  mouseReleased p do
    log "release"
    Tuple x y <- mouseCoords p
    log $ "x" <> toString x <> "y" <> toString y
    case (y > rectHeight) of
      true -> do
        mv <- EVar.tryTake cutSt
        case mv of
          Nothing -> spy "why here" pure false
          Just inner -> EVar.tryPut (withDest inner x y) cutSt
      false -> spy "clicking in paste region" pure false

  draw p do
    background2 p [135.0, 206.0, 205.0]
    rect p 0.0 0.0 700.0 rectHeight Nothing Nothing
    mps <- EVar.tryRead pasteSt
    mcs <- EVar.tryRead cutSt
    mmode <-EVar.tryRead controlSt
    let deleteMode = maybe false _.deleteMode mmode
    case mps of
      Just ps -> for_ ps \n -> do
        renderTile p img n
        if (isNothing mcs && deleteMode) then renderIcon p icon n.dest else pure unit
      Nothing -> pure unit

    image p (ElementOrImageImage img) 0.0 rectHeight Nothing Nothing
    --grid lines
    fill p "#e1dddd"
    let arr = map (\x -> toNumber $ 16 * x) $ range 1 $ round (50.0)
    traverse_ (\a -> line p a 320.0 a 700.0) arr
    traverse_ (\a -> line p 0.0 (a+320.0) 700.0 (a+320.0)) arr

    curX <- mouseX p
    curY <- mouseY p
    fill p "#ef62373b"
    case mcs of
      Just x -> case x of
        Coords (Just x1) (Just y1) (Just x2) (Just y2) -> do
          rect p x1 y1 (x2-x1) (y2-y1) Nothing Nothing
        Coords (Just x1) (Just y1) Nothing Nothing -> do
          rect p x1 y1 (curX-x1) (curY-y1) Nothing Nothing
        _ -> pure unit
      Nothing -> pure unit

  pure $ Just { p5: p }

hookLoadButton ::
  AVar (Array TCoordMap) ->
  Effect Unit
hookLoadButton pasteSt = do
 -- get the document
  win <- window
  doc <- document win
  -- get the buttons
  let docAsParent = Document.toParentNode doc
  input <- querySelector (QuerySelector "#fileinput") docAsParent
  for_ input \n -> do
    hdlr <- loadFile pasteSt
    let target = Element.toEventTarget n
    addEventListener IETypes.input hdlr false target

fileList :: EventTarget -> Effect (Maybe FileList)
fileList tar = case fromEventTarget tar of
  Just elem -> files elem
  _         -> pure Nothing

loadFile :: AVar (Array TCoordMap) -> Effect EventListener
loadFile pasteSt = eventListener $ \e -> do
  for_ (target e) \t -> do
    mfs <- fileList t
    launchAff_ do
      content <- maybe (pure "") readAsText $ (Just <<< toBlob) =<< item 0 =<< mfs
      liftEffect $ load content pasteSt

handleEvent :: Effect EventListener
handleEvent = eventListener $ \e -> do
  log "hello"
