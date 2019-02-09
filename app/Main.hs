{-# OPTIONS_GHC -funbox-strict-fields #-}

-------------------------------------------------------------------------------
-- Autor: José Franco Campos
--
-- Generación, análisis y optimización de escenarios de migración
-- de redes Ipv4/IPv6 mediante programación funcional.
--
-- Diciembre 2008
--
--
-- Descripción: módulo principal del programa, llama al resto de funciones
--              según se le pida.
-------------------------------------------------------------------------------
module Main where

-- Módulos externos
import           System.Directory
import           System.Environment

-- Módulos del programa
import           Lib

-- Ejecuta el programa.
main :: IO ()
main = do
  args <- getArgs
  case args of
    -- Ejecución de la batería de pruebas.
    --["-test"] -> do
    --  Topologia.probar
    --  Reglas.probar
    -- Calcula las estadísticas del conjunto de ficheros.
    ("-estadisticas":files) -> procesarFicheros files
    -- Carga un fichero GML inicial, si existe.
    (file:_) -> do
      ok <- doesFileExist file
      if ok
        then crearEditor file
        else crearEditor ""
    -- Crea un nuevo grafo vacío.
    [] -> crearEditor ""
