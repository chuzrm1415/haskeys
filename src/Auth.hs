{-# LANGUAGE DeriveGeneric #-}

module Auth (
    registerUser,
    checkPIN
) where

import Crypto.Hash (Digest, SHA256, hash)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base16 as B16
import System.Directory (doesFileExist)
import GHC.Generics (Generic)
import Data.Binary
import Data.List (lookup)
import qualified Data.List as List

type User = String
type HashPIN = BS.ByteString
type DBusers = [(User, HashPIN)]

userPathDB :: FilePath
userPathDB = "data/users.bin"

-- Hashea un PIN usando SHA256
hashPIN :: String -> BS.ByteString
hashPIN pin =
    let digest = hash (BS.pack pin) :: Digest SHA256
    in B16.encode (BS.pack (show digest))

-- Cargar los usuarios desde archivo binario
loadUsers :: IO DBusers
loadUsers = do
    existFlag <- doesFileExist userPathDB
    if not existFlag
        then return []
        else decodeFile userPathDB

-- Guardar los usuarios en el archivo binario
saveUsers :: DBusers -> IO ()
saveUsers = encodeFile userPathDB

-- Registrar un nuevo usuario
registerUser :: User -> String -> IO ()
registerUser user pin = do
    db <- loadUsers
    if any ((== user) . fst) db
        then putStrLn "El usuario ya existe."
        else do
            let newUser = (user, hashPIN pin)
            saveUsers (newUser : db)
            putStrLn "Usuario registrado correctamente."

-- Verificar PIN para un usuario existente
checkPIN :: User -> String -> IO Bool
checkPIN user pin = do
    db <- loadUsers
    case lookup user db of
        Nothing -> do
            putStrLn "Usuario no encontrado."
            return False
        Just savedHash -> return (savedHash == hashPIN pin)
