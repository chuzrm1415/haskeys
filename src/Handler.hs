{-# LANGUAGE DeriveGeneric #-}

module Vault where

import GHC.Generics (Generic)
import Data.Binary (Binary)

-- Entrada de una contraseña
data PasswordEntry = PasswordEntry
  { title    :: String
  , password :: String
  } deriving (Show, Eq, Generic)

instance Binary PasswordEntry

type Vault = [(String, [PasswordEntry])]