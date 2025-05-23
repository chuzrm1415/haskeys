--{-# LANGUAGE DeriveGeneric #-}

module Auth (
    registerUser,
    checkPIN,
    getUserSalt
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Base16 as B16
import Crypto.Hash (Digest, SHA256, hash)
import Crypto.Random (getRandomBytes)
import System.Directory (doesFileExist)
import Data.Binary (decodeFile, encodeFile)
import Data.List (lookup)

type User = String
type HashPIN = BS.ByteString
type Salt = BS.ByteString
type DBusers = [(User, HashPIN, Salt)]

userPathDB :: FilePath
userPathDB = "data/users.bin"

-- Hashea un PIN usando SHA256 y un salt
hashPIN :: String -> Salt -> BS.ByteString
hashPIN pin salt =
    let digest = hash (BSC.pack pin <> salt) :: Digest SHA256
    in B16.encode (BSC.pack (show digest))

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
    if any (\(u,_,_) -> u == user) db
        then putStrLn "El usuario ya existe."
        else do
            salt <- getRandomBytes 16
            let hash = hashPIN pin salt
            let newUser = (user, hash, salt)
            saveUsers (newUser : db)
            putStrLn "Usuario registrado correctamente."

-- Verificar PIN para un usuario existente
checkPIN :: User -> String -> IO Bool
checkPIN user pin = do
    db <- loadUsers
    case lookup user [(u, (h, s)) | (u, h, s) <- db] of
        Nothing -> do
            putStrLn "Usuario no encontrado."
            return False
        Just (savedHash, salt) -> return (savedHash == hashPIN pin salt)

-- Devuelve el salt del usuario si existe
getUserSalt :: User -> IO (Maybe Salt)
getUserSalt user = do
    db <- loadUsers
    return $ lookup user [(u, s) | (u, _, s) <- db]
