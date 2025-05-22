module Crypto
  ( encryptPassword
  , decryptPassword
  , deriveKey
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteArray as BA
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (BlockCipher(..), Cipher(..), IV, makeIV)
import Crypto.Error (CryptoFailable(..))
import Crypto.Random (getRandomBytes)
import Crypto.KDF.PBKDF2 (fastPBKDF2_SHA256, Parameters(..))

-- | Encrypt a password using AES256 CBC
encryptPassword :: BS.ByteString -- ^ Key (32 bytes)
                -> BS.ByteString -- ^ Plain password
                -> IO (BS.ByteString, BS.ByteString) -- ^ (IV, Ciphertext)
encryptPassword key plain = do
  ivBytes <- getRandomBytes 16
  let Just iv = makeIV ivBytes :: Maybe (IV AES256)
      cipher = cipherInitNoErr key
      ciphertext = cbcEncrypt cipher iv (pad plain)
  return (ivBytes, ciphertext)

-- | Decrypt a password using AES256 CBC
decryptPassword :: BS.ByteString -- ^ Key (32 bytes)
                -> BS.ByteString -- ^ IV (16 bytes)
                -> BS.ByteString -- ^ Ciphertext
                -> Maybe BS.ByteString -- ^ Plain password
decryptPassword key ivBytes ciphertext =
  case makeIV ivBytes :: Maybe (IV AES256) of
    Nothing -> Nothing
    Just iv ->
      let cipher = cipherInitNoErr key
          plainPadded = cbcDecrypt cipher iv ciphertext
      in unpad plainPadded

-- | Deriva una clave de 32 bytes usando PBKDF2 y SHA256
deriveKey :: BS.ByteString -- ^ PIN (como ByteString)
          -> BS.ByteString -- ^ Salt
          -> BS.ByteString -- ^ Key (32 bytes)
deriveKey pin salt =
  fastPBKDF2_SHA256 (Parameters 10000 32) pin salt

-- Helpers

cipherInitNoErr :: BS.ByteString -> AES256
cipherInitNoErr key =
  case cipherInit key of
    CryptoPassed a -> a
    CryptoFailed e -> error (show e)

-- PKCS7 padding for block size 16
pad :: BS.ByteString -> BS.ByteString
pad bs =
  let padLen = 16 - (BS.length bs `mod` 16)
      padByte = fromIntegral padLen
  in bs <> BS.replicate padLen padByte

unpad :: BS.ByteString -> Maybe BS.ByteString
unpad bs =
  if BS.null bs then Nothing
  else
    let padLen = fromIntegral (BS.last bs)
        (msg, padBytes) = BS.splitAt (BS.length bs - padLen) bs
    in if BS.all (== BS.last bs) padBytes then Just msg else Nothing