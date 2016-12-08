-- | Remember the last slide you saw

module Slides.Remember
  (runSlidesAndRemember)
  where

import Prelude (Unit, bind, flip, map, otherwise, void, ($), (+), (-), (<), (<$>), (>=))
import Slides (Move(..), Slide, mkSlides, runSlidesWithMoves)
import Slides.Internal.Input as I
import Control.Monad.Aff (forkAff, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array (length)
import Data.Generic (class Generic)
import Data.List.Lazy (replicateM)
import Data.Maybe (fromMaybe)
import Signal (merge, runSignal)
import Signal.Channel (CHANNEL, channel, send, subscribe)
import DOM.WebStorage hiding (length)

data Key a = PursSlideIndexKey

ksi :: Key Int
ksi = PursSlideIndexKey

derive instance genericKey :: Generic (Key a)

-- | Same as runSlides but will remember the last slide you saw.
-- | Useful when writing slides
runSlidesAndRemember
  :: Array Slide
  -> forall e. Eff
      ( dom :: DOM
      , storage :: STORAGE
      , channel :: CHANNEL
      , err :: EXCEPTION
      | e
      )
      Unit
runSlidesAndRemember slides = do
  localStorage' <- getLocalStorage
  si' <- fromMaybe 1 <$> getItem localStorage' ksi
  initChannel <- channel None
  inn <- I.input
  let userMoves = inputToMove <$> inn
      moves = merge (subscribe initChannel) userMoves
      len = length slides
  void $ launchAff do
    forkAff $ liftEff $ runSlidesWithMoves moves (mkSlides slides)
    liftEff $ void $ replicateM (si' - 1) (send initChannel Next)
    liftEff $ runSignal $ flip map userMoves \move -> do
      localStorage <- getLocalStorage
      si <- fromMaybe 1 <$> getItem localStorage ksi
      let nextSi = moveToNum si len move
      setItem localStorage ksi nextSi

moveToNum :: Int -> Int -> Move -> Int
moveToNum i len = case _ of
  Next | i + 1 >= len -> len
  Next -> i + 1
  Back | i - 1 < 1 -> 1
  Back -> i - 1
  Start -> 1
  End -> len
  NextOrStart | i + 1 < len -> 1
  NextOrStart -> i + 1
  BackOrEnd | i - 1 < 1 -> len
  BackOrEnd -> i - 1
  _ -> i

inputToMove :: I.Input -> Move
inputToMove i
  | I.clickOrHold (i.arrows.right) = Next
  | I.clickOrHold (i.arrows.left)  = Back
  | I.clickOrHold (i.arrows.down)  = Start
  | I.clickOrHold (i.arrows.up)    = End
  | otherwise = None

