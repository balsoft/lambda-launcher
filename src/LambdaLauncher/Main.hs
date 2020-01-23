-- λauncher
-- A GTK application for doing stuff
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module LambdaLauncher.Main ( runApp ) where

import Control.Monad (void)
import qualified Data.Vector as Vector

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
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple

import Control.Concurrent
import Control.Concurrent.Chan

import Data.Functor (($>))
import Data.Text (Text, lines)
import Data.List (genericLength, sortOn)

import LambdaLauncher.Types

import qualified Data.Text as T

data Event
  = QueryChanged Text
  | ResultAdded Text
                [Result]
                (Chan [Result])
  | Activated (IO ())
  | Closed

data State = State
  { query :: Text
  , results :: [Result]
  }

instance Eq Result where
  a == b = shownText a == shownText b

removeSame :: Eq a => [a] -> [a] -> [a]
removeSame found (x:xs) =
  if x `elem` found
  then removeSame found xs
  else removeSame (x:found) xs
removeSame found _ = found

cutOffAt :: Text -> Int -> Text
s `cutOffAt` i =
  if T.length s < i
    then s
    else T.append (T.take (i - 3) s) "..."

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
      widget
        SearchEntry
        [ onM #searchChanged toQueryChangedEvent
        , on #stopSearch Closed
        , afterCreated (\w -> void $ Gtk.on w #map $ #grabFocus w)
        ]
    buildResults res = actionToButton <$> Vector.fromList res
    actionToButton (Action r _ a) =
      widget
      Button
      [ #label := ((head $ Data.Text.lines r) `cutOffAt` maxChars)
      , on #clicked $ Activated a
      ]
    toQueryChangedEvent :: SearchEntry -> IO Event
    toQueryChangedEvent w = QueryChanged <$> getEntryText w

runPlugin :: Chan [Result] -> Text -> Plugin -> IO ()
runPlugin chan q plugin = void $ forkIO $ plugin q >>= writeChan chan

updateResults :: Text -> Chan [Result] -> IO (Maybe Event)
updateResults q chan = fmap (\x -> ResultAdded q x chan) <$> (optional $ readChan chan)

update' :: Configuration -> [Plugin] -> State -> Event -> Transition State Event
update' _ _ state (QueryChanged "") =
  Transition state {query = "", results = []} $ return Nothing
update' _ plugins state (QueryChanged s) =
  Transition state {query = s, results = []} $ do
    chan <- newChan
    void $ sequence $ runPlugin chan s <$> plugins
    updateResults s chan
update' Configuration {..} _ state (ResultAdded q x chan) =
  Transition
    state
      { results =
          if anyTriggered
            then newResults
            else if queryMatch
                   then sortOn priority $ (results state) ++ take maxItemsPerPlugin newResults
                   else results state
      } $
  if queryMatch && (not anyTriggered)
    then updateResults q chan
    else return Nothing
  where
    queryMatch = q == query state
    anyTriggered = any  ((== 0) . priority) newResults
    newResults = removeSame [] x
    
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
