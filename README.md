# haskeys

**haskeys** es un gestor de contraseñas simple escrito en Haskell. Permite almacenar, cifrar, mostrar, editar y eliminar contraseñas de manera segura usando cifrado AES y una clave derivada de un PIN.

## Características

- Almacenamiento seguro de contraseñas cifradas.
- Cada usuario puede tener múltiples contraseñas, cada una con su propio título.
- Las contraseñas se almacenan en un archivo binario (`data/vault.bin`).
- Permite agregar, mostrar, editar y eliminar contraseñas.
- Las contraseñas se cifran usando una clave derivada de un PIN y un salt.
- Interfaz de línea de comandos sencilla.

## Requisitos

- [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/)
- [Stack](https://docs.haskellstack.org/en/stable/README/) o [Cabal](https://www.haskell.org/cabal/)

## Instalación

Clona el repositorio y entra en la carpeta del proyecto:

```bash
git clone https://github.com/tuusuario/haskeys.git
cd haskeys
```

Instala las dependencias y compila el proyecto:

Con **Stack**:
```bash
stack build
```

Con **Cabal**:
```bash
cabal build
```

## Ejecución

Con **Stack**:
```bash
stack run
```

Con **Cabal**:
```bash
cabal run
```

O ejecuta el binario directamente si ya fue compilado:

```bash
./haskeys-exe
```

## Uso básico

Al iniciar el programa, podrás:

- **Agregar una contraseña:** Ingresa el usuario, PIN y los datos de la nueva contraseña.
- **Mostrar contraseñas:** Visualiza todas las contraseñas almacenadas para un usuario.
- **Editar una contraseña:** Selecciona una contraseña por número y actualiza su valor.
- **Eliminar una contraseña:** Selecciona una contraseña por número y elimínala del vault.

Las contraseñas se almacenan cifradas y solo pueden ser desencriptadas con la clave correcta.

## Notas

- El archivo de contraseñas se guarda en `data/vault.bin`. No lo compartas ni lo subas a repositorios públicos.


## Licencia

Este proyecto se distribuye bajo la licencia MIT.

---

¡Contribuciones y sugerencias son bienvenidas!