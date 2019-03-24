module Types where

data Result =
  Action String
         (IO ())
