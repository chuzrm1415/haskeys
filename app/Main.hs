import Auth (registerUser, checkPIN)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
    putStrLn "¿Qué querés hacer? (1: Registrar, 2: Ingresar, 3: Salir)"
    opcion <- getLine
    case opcion of
        "1" -> do
            putStr "Elegí un nombre de usuario: "
            hFlush stdout
            user <- getLine
            putStr "Elegí un PIN: "
            hFlush stdout
            pin <- getLine
            registerUser user pin
        "2" -> do
            putStr "Usuario: "
            hFlush stdout
            user <- getLine
            putStr "PIN: "
            hFlush stdout
            pin <- getLine
            ok <- checkPIN user pin
            if ok
                then putStrLn "¡Acceso concedido!"
                else putStrLn "Acceso denegado."
        "3" -> putStrLn "Saliendo del programa."
        _   -> putStrLn "Opción no válida"
