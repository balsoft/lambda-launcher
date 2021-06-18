-- λauncher
-- A GTK application for doing stuff
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LambdaLauncher.Main ( runApp ) where

import Control.Monad (void)
import qualified Data.Vector as Vector
import Data.Vector (Vector)

import Control.Applicative (optional)

import GI.Gtk
  ( Box(..)
  , Button(..)
  , Orientation(..)
  , PolicyType(..)
  , ScrolledWindow(..)
  , SearchEntry(..)
  , Window(..)
  , WindowPosition(..)
  , getEntryText
  )

import qualified GI.Gtk as Gtk
import qualified GI.GObject as GI

import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple
import GI.Gtk.Declarative.EventSource ( fromCancellation )

import Data.Functor (($>))
import Data.Text (Text, lines)
import Data.List (nub, genericLength, sortOn)

import Streamly
import qualified Streamly.Prelude as S

import LambdaLauncher.Types

import qualified Data.Text as T

data Event
  = QueryChanged Text
  | ResultAdded Text
                [Result]
                (SerialT IO [Result])
  | Activated (IO ())
  | Closed

data State = State
  { query :: Text
  , results :: [Result]
  }

cutOffAt :: Text -> Int -> Text
s `cutOffAt` i =
  if T.length s < i
    then s
    else T.append (T.take (i - 3) s) "..."

searchWidgetAutoFocus
  :: Vector (Attribute Gtk.SearchEntry Event)
  -> Widget Event
searchWidgetAutoFocus customAttributes =
  Widget
  (CustomWidget { customWidget
                , customCreate
                , customPatch
                , customSubscribe
                , customAttributes
                , customParams
                }
  )
 where
  customWidget = SearchEntry

  customCreate _ = do
    entry <- Gtk.new SearchEntry [  ]
    _ <- Gtk.on entry #map $ #grabFocus entry
    return (entry, entry)

  customPatch _ _ _ = CustomKeep

  customSubscribe _ _ entry cb = do
    let fc = fromCancellation . GI.signalHandlerDisconnect entry

    hSC <- Gtk.on entry #searchChanged $ cb . QueryChanged =<< getEntryText entry
    hSS <- Gtk.on entry #stopSearch $ cb Closed

    return $ fc hSC <> fc hSS

  customParams = ()

searchView :: Configuration ->  State -> AppView Window Event
searchView Configuration {..} State {results} =
  bin
    Window
    [ #title := "λauncher"
    , on #deleteEvent (const (True, Closed))
    , #widthRequest := width
    , #heightRequest :=
      (32 + min maxHeight (32 * genericLength results))
    , #resizable := False
    , #canFocus := False
    , #decorated := showBorder
    , #windowPosition := WindowPositionCenter
    , on #map (QueryChanged "")
    , on #focusOutEvent (const (True, Closed))
    ] $
  bin
    ScrolledWindow
    [ #hscrollbarPolicy := PolicyTypeNever
    , #vscrollbarPolicy := PolicyTypeAlways
    ] $
  container Box [#orientation := OrientationVertical] $
  searchEntry `Vector.cons` buildResults results
  where
    searchEntry =
      BoxChild defaultBoxChildProperties $
      searchWidgetAutoFocus
        [
        ]
    buildResults res = actionToButton <$> Vector.fromList res
    actionToButton (Action r _ a) =
      widget
      Button
      [ #label := (head (Data.Text.lines r) `cutOffAt` maxChars)
      , on #clicked $ Activated a
      ]

runPlugin :: Text -> Plugin -> IO (Maybe [Result])
runPlugin q plugin = optional $ plugin q

updateResults :: Text -> SerialT IO [Result] -> IO (Maybe Event)
updateResults q s = fmap (uncurry $ ResultAdded q) <$> S.uncons s

update' :: Configuration -> [Plugin] -> State -> Event -> Transition State Event
update' _ _ state (QueryChanged "") =
  Transition state {query = "", results = []} $ return Nothing
update' _ plugins state (QueryChanged s) =
  Transition state {query = s, results = []} $ do
    updateResults s $ parallely $ S.mapMaybeM (runPlugin s) $ S.fromList plugins
update' Configuration {..} _ state (ResultAdded q x chan) =
  Transition
    state
      { results =
          if anyTriggered
            then newResults
            else if queryMatch
                   then sortOn priority $ results state ++ take maxItemsPerPlugin newResults
                   else results state
      } $
  if queryMatch && not anyTriggered
    then updateResults q chan
    else return Nothing
  where
    queryMatch = q == query state
    anyTriggered = any  ((== 0) . priority) newResults
    newResults = nub x

update' _ _ state (Activated a) = Transition state $ a $> Just Closed
update' _ _ _ Closed = Exit

runApp :: Configuration -> [Plugin] -> IO ()
runApp c p =
  void $
  run
    App
      { view = searchView c
      , update = update' c p
      , inputs = []
      , initialState = State "" []
      }
