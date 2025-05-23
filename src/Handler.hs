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
import Text.Read (readMaybe)

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

-- Función para enmascarar el usuario (ej: "jo****do")
maskUser :: String -> String
maskUser u
    | length u <= 2 = replicate (length u) '*'
    | otherwise     = take 2 u ++ replicate (length u - 4) '*' ++ drop (length u - 2) u

-- Función para enmascarar la contraseña (siempre muestra 8 asteriscos)
maskPassword :: String -> String
maskPassword _ = replicate 8 '*'

showPasswords :: String -> BS.ByteString -> IO ()
showPasswords user key = do
    vault <- loadPass
    case lookup user vault of
        Nothing -> putStrLn $ "No hay contraseñas guardadas para el usuario: " ++ user
        Just entries -> do
            putStrLn $ "Contraseñas para " ++ user ++ ":"
            mapM_ (uncurry (printEntry key)) (zip [1..] entries)
  where
    printEntry k idx (PasswordEntry t pwdHex ivHex) = do
        case (B16.decode (BSC.pack pwdHex), B16.decode (BSC.pack ivHex)) of
            (Right cipherPwd, Right iv) ->
                case decryptPassword k iv cipherPwd of
                    Just plainPwd -> putStrLn $ show idx ++ ". " ++ t ++ ": " ++ BSC.unpack plainPwd
                    Nothing       -> putStrLn $ show idx ++ ". " ++ t ++ ": [ERROR al desencriptar]"
            _ -> putStrLn $ show idx ++ ". " ++ t ++ ": [ERROR al decodificar]"

editPassword :: String -> BS.ByteString -> IO ()
editPassword user key = do
    vault <- loadPass
    case lookup user vault of
        Nothing -> putStrLn "No hay contraseñas guardadas para este usuario."
        Just entries -> do
            putStrLn $ "Contraseñas para " ++ user ++ ":"
            mapM_ (uncurry printEntryTitleOnly) (zip [1..] entries)
            putStr "Ingrese el número de la contraseña a editar: "
            hFlush stdout
            idxStr <- getLine
            case readMaybe idxStr of
                Nothing -> putStrLn "Entrada inválida."
                Just idx ->
                    if idx < 1 || idx > length entries
                        then putStrLn "Número fuera de rango."
                        else do
                            putStr "Nueva contraseña: "
                            hFlush stdout
                            newPwd <- getLine
                            (iv, cipherPwd) <- encryptPassword key (BSC.pack newPwd)
                            let (before, entry:after) = splitAt (idx - 1) entries
                                updatedEntry = entry { password = BSC.unpack (B16.encode cipherPwd)
                                                     , iv = BSC.unpack (B16.encode iv)
                                                     }
                                newEntries = before ++ (updatedEntry : after)
                                newVault = map (\(u, es) -> if u == user then (u, newEntries) else (u, es)) vault
                            savePass newVault
                            putStrLn "Contraseña actualizada correctamente."
  where
    printEntryTitleOnly idx (PasswordEntry t _ _) =
        putStrLn $ show idx ++ ". " ++ t

deletePassword :: String -> BS.ByteString -> IO ()
deletePassword user key = do
    vault <- loadPass
    case lookup user vault of
        Nothing -> putStrLn "No hay contraseñas guardadas para este usuario."
        Just entries -> do
            putStrLn $ "Contraseñas para " ++ user ++ ":"
            mapM_ (uncurry printEntryTitleOnly) (zip [1..] entries)
            putStr "Ingrese el número de la contraseña a eliminar: "
            hFlush stdout
            idxStr <- getLine
            case readMaybe idxStr of
                Nothing -> putStrLn "Entrada inválida."
                Just idx ->
                    if idx < 1 || idx > length entries
                        then putStrLn "Número fuera de rango."
                        else do
                            let (before, _:after) = splitAt (idx - 1) entries
                                newEntries = before ++ after
                                newVault = if null newEntries
                                           then filter ((/= user) . fst) vault
                                           else map (\(u, es) -> if u == user then (u, newEntries) else (u, es)) vault
                            savePass newVault
                            putStrLn "Contraseña eliminada correctamente."
  where
    printEntryTitleOnly idx (PasswordEntry t _ _) =
        putStrLn $ show idx ++ ". " ++ t