-- λauncher
-- A GTK application for doing stuff
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}

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
import System.IO.Unsafe (unsafeInterleaveIO)

import Data.Functor (($>))
import Data.Text (Text)
import Data.List (genericLength, sortOn)

import LambdaLauncher.Types

import qualified Data.Text as T


data Event
  = QueryChanged Text
  | ResultAdded Text
                [Result]
                [IO [Result]]
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

searchView :: Configuration ->  State -> AppView Window Event
searchView configuration State {results} =
  bin
    Window
    [ #title := "λauncher"
    , on #deleteEvent (const (True, Closed))
    , #widthRequest := width configuration
    , #heightRequest :=
      (32 + min (maxHeight configuration) (32 * genericLength results))
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
             [ #label := (r `cutOffAt` maxChars configuration)
             , on #clicked $ Activated a
             ])
        res
    toQueryChangedEvent :: SearchEntry -> IO Event
    toQueryChangedEvent w = QueryChanged <$> getEntryText w

updateResults :: Text -> [IO [Result]] -> IO (Maybe Event)
updateResults _ [] = return Nothing
updateResults q (result:results) = do
  first <- unsafeInterleaveIO $ optional $ result
  case first of
    Nothing -> updateResults q results
    Just f -> return $ Just $ ResultAdded q f results

update' :: [Plugin] -> State -> Event -> Transition State Event
update' _ state (QueryChanged "") =
  Transition state {query = "", results = []} $ return Nothing
update' plugins state (QueryChanged s) =
  Transition state {query = s, results = []} $
  updateResults s $ ($ s) <$> plugins
update' _ state (ResultAdded q x xs) =
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
update' _ state (Activated a) = Transition state $ a $> Just Closed
update' _ _ Closed = Exit

runApp :: Configuration -> [Plugin] -> IO ()
runApp c p =
  void $
  run
    App
      { view = searchView c
      , update = update' p
      , inputs = []
      , initialState = State "" []
      }
