import Auth
import Handler
import System.IO (hFlush, stdout)
import qualified Data.ByteString as BS

main :: IO ()
main = mainMenu

mainMenu :: IO ()
mainMenu = do
    putStrLn "¿Qué querés hacer? (1: Registrar, 2: Ingresar, 3: Salir)"
    opcion <- getLine
    case opcion of
        "1" -> do
            register
            mainMenu
        "2" -> do
            verification
            mainMenu
        "3" -> putStrLn "Saliendo del programa."
        _   -> do
            putStrLn "Opción no válida"
            mainMenu

register :: IO ()
register = do
    putStr "Elegí un nombre de usuario: "
    hFlush stdout
    user <- getLine
    putStr "Elegí un PIN: "
    hFlush stdout
    pin <- getLine
    registerUser user pin

verification :: IO ()
verification = do
    putStr "Usuario: "
    hFlush stdout
    user <- getLine
    putStr "PIN: "
    hFlush stdout
    pin <- getLine
    ok <- checkPIN user pin
    if ok
        then do
            mUserSalt <- getUserSalt user
            case mUserSalt of
                Nothing -> putStrLn "No se encontró el salt del usuario."
                Just userSalt -> do
                    userKey <- getUserKey pin userSalt
                    userMenu user userKey
        else putStrLn "Acceso denegado."

userMenu :: String -> BS.ByteString -> IO ()
userMenu user userKey = do
    putStrLn "¿Qué querés hacer? (1: Agregar contraseña, 2: Ver contraseñas, 3: Salir)"
    opcion <- getLine
    case opcion of
        "1" -> do
            addPassword user userKey
            userMenu user userKey
        "2" -> do
            showPasswords user userKey
            userMenu user userKey
        "3" -> putStrLn "Saliendo del programa."
        _   -> do
            putStrLn "Opción no válida"
            userMenu user userKey