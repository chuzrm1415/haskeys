import Auth

main :: IO ()
main = do
    putStrLn "¿Qué querés hacer? (1: registrar, 2: ingresar)"
    opcion <- getLine
    case opcion of
        "1" -> do
            putStr "Elegí un nombre de usuario: "
            user <- getLine
            putStr "Elegí un PIN: "
            pin <- getLine
            registerUser user pin
        "2" -> do
            putStr "Usuario: "
            user <- getLine
            putStr "PIN: "
            pin <- getLine
            ok <- checkPIN user pin
            if ok
                then putStrLn "¡Acceso concedido!"
                else putStrLn "Acceso denegado."
        _ -> putStrLn "Opción no válida"
