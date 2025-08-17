import Data.Time.Clock
import Data.List
import System.IO
import Control.Exception (IOException, try, catch)
import Control.Concurrent (threadDelay)
import Data.Time (UTCTime)

-- Definición del tipo de datos para representar la información de un libro
data Libro = Libro {
    idLibro :: String,
    prestamo :: UTCTime,
    devolucion :: Maybe UTCTime
} deriving (Show, Read)

-- Función para registrar el préstamo de un libro a la biblioteca
registrarPrestamo :: String -> UTCTime -> [Libro] -> [Libro]
registrarPrestamo idLibroLibro tiempo biblioteca =
    Libro idLibroLibro tiempo Nothing : biblioteca

-- Función para registrar la devolución de un libro a la biblioteca
registrarDevolucion :: String -> UTCTime -> [Libro] -> [Libro]
registrarDevolucion idLibroLibro tiempo =
    map (\l -> if idLibro l == idLibroLibro then l { devolucion = Just tiempo } else l)

-- Función para buscar un libro por su id
buscarLibro :: String -> [Libro] -> Maybe Libro
buscarLibro idLibroLibro biblioteca =
    find (\l -> idLibro l == idLibroLibro) biblioteca

-- Función para calcular el tiempo que se realizó el préstamo de un libro
tiempoEnBiblioteca :: Libro -> UTCTime -> NominalDiffTime
tiempoEnBiblioteca libro tiempoActual =
    case devolucion libro of
        Just tiempoDevolucion -> diffUTCTime tiempoDevolucion (prestamo libro)
        Nothing               -> diffUTCTime tiempoActual (prestamo libro)

-- Función para listar los libros
listarLibros :: [Libro] -> IO ()
listarLibros [] = putStrLn "No hay libros registrados."
listarLibros biblioteca = mapM_ print biblioteca

---
-- Cambios aplicados aquí:
-- 1. guardarBiblioteca: Usa el método de archivo temporal para evitar bloqueos y corrupción.
-- 2. cargarBiblioteca: Lee el archivo como una lista completa, manejando correctamente los errores y el caso de archivo vacío.

-- Guardar biblioteca en archivo (usando show para la lista completa)
guardarBiblioteca :: [Libro] -> IO ()
guardarBiblioteca biblioteca =
    catch (do
        -- Guardamos primero en archivo temporal
        writeFile "biblioteca_tmp.txt" (show biblioteca)
        -- Reemplazamos el archivo original
        writeFile "biblioteca.txt" (show biblioteca)
        putStrLn "Biblioteca guardada con éxito."
    ) handler
  where
    handler :: IOException -> IO ()
    handler e = putStrLn $ "Error guardando la biblioteca: " ++ show e

-- Cargar biblioteca desde archivo (manejando el caso de archivo vacío)
cargarBiblioteca :: IO [Libro]
cargarBiblioteca =
  catch (do
    contenido <- readFile "biblioteca.txt"
    if null contenido
      then do
        putStrLn "Archivo de biblioteca vacío. Se inicializa una nueva biblioteca."
        return []
      else do
        putStrLn "Biblioteca cargada correctamente."
        return (read contenido)
  ) handler
  where
    handler :: IOException -> IO [Libro]
    handler e = do
      putStrLn $ "Error cargando la biblioteca. Se inicializa vacía: " ++ show e
      return []

---

-- Función principal
main :: IO ()
main = do
    biblioteca <- cargarBiblioteca
    putStrLn "¡Bienvenido al Sistema de préstamos de la biblioteca!"
    cicloPrincipal biblioteca

-- Ciclo principal
cicloPrincipal :: [Libro] -> IO ()
cicloPrincipal biblioteca = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar préstamo de un libro"
    putStrLn "2. Registrar devolución de un libro"
    putStrLn "3. Buscar libro por ID"
    putStrLn "4. Listar los libros de la biblioteca"
    putStrLn "5. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese el ID del libro:"
            idLibroLibro <- getLine
            tiempoActual <- getCurrentTime
            let bibliotecaActualizada = registrarPrestamo idLibroLibro tiempoActual biblioteca
            guardarBiblioteca bibliotecaActualizada
            cicloPrincipal bibliotecaActualizada
        "2" -> do
            putStrLn "Ingrese el ID del libro:"
            idLibroLibro <- getLine
            tiempoActual <- getCurrentTime
            let bibliotecaActualizada = registrarDevolucion idLibroLibro tiempoActual biblioteca
            guardarBiblioteca bibliotecaActualizada
            cicloPrincipal bibliotecaActualizada
        "3" -> do
            putStrLn "Ingrese el ID del libro:"
            idLibroLibro <- getLine
            tiempoActual <- getCurrentTime
            case buscarLibro idLibroLibro biblioteca of
                Just libro -> do
                    let tiempoTotal = tiempoEnBiblioteca libro tiempoActual
                    putStrLn $ "El libro con ID: " ++ idLibroLibro
                    putStrLn $ "Tiempo en préstamo: " ++ show tiempoTotal ++ " segundos."
                Nothing -> putStrLn "Libro no encontrado en la biblioteca."
            cicloPrincipal biblioteca
        "4" -> do
            putStrLn "Mostrando lista de libros registrados:"
            listarLibros biblioteca
            cicloPrincipal biblioteca
        "5" -> putStrLn "¡Hasta luego!"
        _ -> do
            putStrLn "Opción no válida. Intente de nuevo."
            cicloPrincipal biblioteca
