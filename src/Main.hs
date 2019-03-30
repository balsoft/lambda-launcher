-- λauncher
-- A GTK application for doing stuff
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import Control.Monad (void)
import qualified Data.Text as Text
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
import System.IO.Unsafe (unsafeInterleaveIO)

import Data.Functor (($>))

import Data.List (genericLength, sortOn)

import Types

import Plugins.Main

import GHC.Int (Int32)

-- Configuration!
data Configuration = Configuration
  { width :: Int32
  , maxHeight :: Int32
  , maxChars :: Int
  }

configuration :: Configuration
-- Edit this
configuration = Configuration 800 1700 100

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
    , #widthRequest := (width configuration)
    , #heightRequest :=
      (32 + (min (maxHeight configuration) (32 * genericLength results)))
    , #resizable := False
    , #canFocus := False
    , #decorated := False
    , #windowPosition := WindowPositionCenter
    , on #map (QueryChanged "")
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
    buildResults res =
      Vector.fromList $
      map
        (\(Action r _ a) ->
           widget
             Button
             [ #label := (Text.pack $ r `cutOffAt` (maxChars configuration))
             , on #clicked $ Activated a
             ])
        res
    toQueryChangedEvent :: SearchEntry -> IO Event
    toQueryChangedEvent w = QueryChanged . Text.unpack <$> getEntryText w

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
          if anyTriggered
            then x
            else if queryMatch
                   then sortOn priority $ (results state) ++ x
                   else results state
      } $
  if queryMatch && (not anyTriggered)
    then updateResults q xs
    else return Nothing
  where
    queryMatch = q == query state
    anyTriggered = any ((== 0) . priority) x
update' state (Activated a) = Transition state $ a $> Just Closed
update' _ Closed = Exit

main :: IO ()
main =
  void $
  run
    App
      { view = searchView
      , update = update'
      , inputs = []
      , initialState = State "" []
      }
