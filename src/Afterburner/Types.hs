module Afterburner.Types
       ( Type (..)
       )
where

data Type = Integer
          | Real
          | Boolean
          | Char
          | String
          | Array [(Integer, Integer)] Type
