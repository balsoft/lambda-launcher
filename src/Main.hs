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

import GI.Gtk
  ( Box(..)
  , Button(..)
  , Orientation(..)
  , SearchEntry(..)
  , Window(..)
  , WindowPosition(..)
  , getEntryText
  )
import qualified GI.Gtk as Gtk
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple

import Types

import Plugins.Main

data Event
  = QueryChanged String
  | ResultsChanged [Result]
  | Activated (IO ())
  | Closed

data State = State
  { query :: String
  , results :: [Result]
  }

searchView :: State -> AppView Window Event
searchView State {results} =
  bin
    Window
    [ #title := "λauncher"
    , on #deleteEvent (const (True, Closed))
    , #widthRequest := 500
    , #heightRequest := 35
    , #resizable := False
    , #canFocus := False
    , #windowPosition := WindowPositionCenterAlways
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
        (\(Action r a) ->
           widget Button [#label := Text.pack r, on #clicked $ Activated a])
        res
    toQueryChangedEvent :: SearchEntry -> IO Event
    toQueryChangedEvent w = QueryChanged <$> Text.unpack <$> getEntryText w

update' :: State -> Event -> Transition State Event
update' state (QueryChanged s) =
  Transition state {query = s} $
  Just <$> ResultsChanged <$> concat <$> fmap concat <$>
  (sequence $ optional <$> ($ s) <$> plugins)
update' state (ResultsChanged xs) =
  Transition state {results = xs} $ return Nothing
update' state (Activated a) =
  Transition state $ do
    a
    return $ Just Closed
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
