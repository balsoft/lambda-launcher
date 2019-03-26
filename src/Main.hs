-- λauncher
-- A GTK application for doing stuff
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (void)
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import Control.Applicative (optional)

import Control.Concurrent.Async (mapConcurrently)
import GI.Gtk
  ( Box(..)
  , Button(..)
  , Orientation(..)
  , PolicyType(..)
  , ScrolledWindow(..)
  , SearchEntry(..)
  , Window(..)
  , WindowPosition(..)
  , WindowType(..)
  , getEntryText
  )
import qualified GI.Gtk as Gtk
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple
import System.IO.Unsafe (unsafeInterleaveIO)

import Data.List (genericLength, sortOn)

import Types

import Plugins.Main

data Event
  = QueryChanged String
  | ResultAdded String
                [Result]
                [IO [Result]]
  | Activated (IO ())
  | Closed

data State = State
  { query :: String
  , results :: [Result]
  }

cutOffAt :: String -> Int -> String
s `cutOffAt` i =
  if length s < i
    then s
    else (take (i - 3) s) ++ "..."

searchView :: State -> AppView Window Event
searchView State {results} =
  bin
    Window
    [ #title := "λauncher"
    , on #deleteEvent (const (True, Closed))
    , #widthRequest := 500
    , #heightRequest := (32 + (min 400 (32 * genericLength results)))
    , #resizable := False
    , #canFocus := False
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
        , afterCreated
            (\w -> do
               void $ Gtk.on w #map $ #grabFocus w
               return ())
        ]
    buildResults res =
      Vector.fromList $
      map
        (\(Action r _ a) ->
           widget
             Button
             [ #label := (Text.pack $ r `cutOffAt` 60)
             , on #clicked $ Activated a
             ])
        res
    toQueryChangedEvent :: SearchEntry -> IO Event
    toQueryChangedEvent w = QueryChanged <$> Text.unpack <$> getEntryText w

updateResults :: String -> [IO [Result]] -> IO (Maybe Event)
updateResults _ [] = return Nothing
updateResults q (result:results) = do
  first <- unsafeInterleaveIO $ optional $ result
  case first of
    Nothing -> updateResults q results
    Just f -> return $ Just $ ResultAdded q f results

update' :: State -> Event -> Transition State Event
update' state (QueryChanged "") =
  Transition state {query = "", results = []} $ return Nothing
update' state (QueryChanged s) =
  Transition state {query = s, results = []} $
  updateResults s $ ($ s) <$> plugins
update' state (ResultAdded q x xs) =
  Transition
    state
      { results =
          if q == query state
            then sortOn priority $ (results state) ++ x
            else results state
      } $
  if q == query state
    then updateResults q xs
    else return Nothing
update' state (Activated a) = Transition state $ seq a $ return $ Just Closed
update' _ Closed = Exit

main :: IO ()
main = do
  void $
    run
      App
        { view = searchView
        , update = update'
        , inputs = []
        , initialState = State "" []
        }
