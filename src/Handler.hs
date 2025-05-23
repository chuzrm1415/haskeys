{-# LANGUAGE DeriveGeneric #-}

module Handler where

import Auth
import Crypto
import GHC.Generics (Generic)
import Data.Binary (Binary, decodeFile, encodeFile)
import System.Directory (doesFileExist)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Base16 as B16
import System.IO (hFlush, stdout)

-- Entrada de una contraseña
data PasswordEntry = PasswordEntry
  { title    :: String
  , password :: String
  , iv       :: String
  } deriving (Show, Eq, Generic)

instance Binary PasswordEntry

type PassVault = [(String, [PasswordEntry])]

passVaultPath :: FilePath
passVaultPath = "data/vault.bin"

-- Cargar el vault desde archivo binario
loadPass :: IO PassVault
loadPass = do
    exists <- doesFileExist passVaultPath
    if exists
        then decodeFile passVaultPath
        else return []

-- Guardar el vault en el archivo binario
savePass :: PassVault -> IO ()
savePass = encodeFile passVaultPath

getUserKey :: String -> BS.ByteString -> IO BS.ByteString
getUserKey pin salt = return $ deriveKey (BSC.pack pin) salt

addPassword :: String -> BS.ByteString -> IO ()
addPassword user key = do
    putStr "Título: "
    hFlush stdout
    titulo <- getLine
    putStr "Contraseña: "
    hFlush stdout
    plainPwd <- getLine
    (iv, cipherPwd) <- encryptPassword key (BSC.pack plainPwd)
    let entry = PasswordEntry
                  { title = titulo
                  , password = BSC.unpack (B16.encode cipherPwd)
                  , iv = BSC.unpack (B16.encode iv)
                  }
    vault <- loadPass
    let updatedVault = case lookup user vault of
            Just entries -> (user, entry : entries) : filter ((/= user) . fst) vault
            Nothing      -> (user, [entry]) : vault
    savePass updatedVault
    putStrLn "Contraseña agregada y cifrada correctamente."

showPasswords :: String -> BS.ByteString -> IO ()
showPasswords user key = do
    vault <- loadPass
    case lookup user vault of
        Nothing -> putStrLn "No hay contraseñas guardadas para este usuario."
        Just entries -> do
            putStrLn $ "Contraseñas para " ++ user ++ ":"
            mapM_ (printEntry key) entries
  where
    printEntry k (PasswordEntry t pwdHex ivHex) = do
        case (B16.decode (BSC.pack pwdHex), B16.decode (BSC.pack ivHex)) of
            (Right cipherPwd, Right iv) ->
                case decryptPassword k iv cipherPwd of
                    Just plainPwd -> putStrLn $ "- " ++ t ++ ": " ++ BSC.unpack plainPwd
                    Nothing       -> putStrLn $ "- " ++ t ++ ": [ERROR al desencriptar]"
            _ -> putStrLn $ "- " ++ t ++ ": [ERROR al decodificar hex]"