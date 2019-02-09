-------------------------------------------------------------------------------
-- Autor: José Franco Campos
-- 
-- Generación, análisis y optimización de escenarios de migración
-- de redes Ipv4/IPv6 mediante programación funcional.
-- 
-- Diciembre 2008
--
--
-- Descripción: obtención de estadísticas de conectividad a partir de un grafo.
-------------------------------------------------------------------------------

module Topologia.Estadisticas (
	Estadisticas,
	cabecera,
	calcularEstadisticas,
	procesarFicheros,
	show
	) where


-- Módulos externos
import qualified Data.IntMap as IntMap
import Control.Exception
import Control.Monad
import Text.Printf


-- Módulos del programa
import Topologia.GML
import Topologia.Grafo



-------------------------------------------------------------------------------
-- Tipos de datos
-------------------------------------------------------------------------------

-- Parámetros que se guardan de una estadística.
data Estadisticas = Estadisticas {
	nombre :: FilePath,                            -- Nombre del fichero.
	num_nodos, num_enlaces :: Int,                 -- Número de nodos y enlaces.
	conectividad_abs :: !(Double, Double, Double), -- Grado de conectividad absoluta.
	conectividad_rel :: !(Double, Double, Double), -- Grado de conectividad relativa.
	densidad :: !Double                            -- Densidad del grafo.
	}


-- Conversión de los resultados estadísticos a texto.
instance Show Estadisticas where
	show e = p' ++ a' ++ b' ++ c' ++ d' ++ e' ++ f' ++ g' ++ h' ++ i'
		where
		p' = printf "%s\t" (nombre e)
		a' = printf "%d\t" (num_nodos e)
		b' = printf "%d\t" (num_enlaces e)
		c' = printf "%d\t" (round amin :: Int)
		d' = printf "%.3f\t" amed
		e' = printf "%d\t" (round amax :: Int)
		f' = printf "%.3f%%\t" rmin
		g' = printf "%.3f%%\t" rmed
		h' = printf "%.3f%%\t" rmax
		i' = printf "%f\t" (densidad e)
		(amin, amed, amax) = conectividad_abs e
		(rmin, rmed, rmax) = conectividad_rel e



-------------------------------------------------------------------------------
-- Funciones
-------------------------------------------------------------------------------

-- Encabezado del fichero, aparecerá en la primera línea para indicar el
-- significado de cada campo.
cabecera :: String
cabecera = "Fichero\t" ++ a' ++ b' ++ c' ++ d' ++ e' ++ f' ++ g' ++ h' ++ i'
	where
	a' = "Numero de nodos\t"
	b' = "Numero de enlaces\t"
	c' = "Conectividad absoluta minima\t"
	d' = "Conectividad absoluta media\t"
	e' = "Conectividad absoluta maxima\t"
	f' = "Conectividad relativa minima\t"
	g' = "Conectividad relativa media\t"
	h' = "Conectividad relativa maxima\t"
	i' = "Densidad\t"


-- Cuenta el número de nodos que hay en el grafo.
contarNodos :: Grafo -> Double
contarNodos g = fromIntegral $ IntMap.size (nodos g)


-- Cuenta el número de enlaces por nodo que hay en el grafo.
contarArcos :: Grafo -> [Double]
contarArcos g = Prelude.map fromIntegral $ IntMap.elems $ IntMap.map length (arcos g)


-- Calcula las estadísticas de un grafo
calcularEstadisticas :: FilePath -> Grafo -> Estadisticas
calcularEstadisticas path grafo = Estadisticas {
	nombre = path,
	num_nodos = round nodos',
	num_enlaces = round agregados,
	conectividad_abs = (
		minimum arcos',
		(sum arcos') / (fromIntegral $ length arcos'),
		maximum arcos'),
	conectividad_rel = (
		100.0 * (minimum arcos') / nodos',
		100.0 * (sum arcos' / (fromIntegral $ length arcos')) / nodos',
		100.0 * (maximum arcos') / nodos'),
	densidad = (2.0 * nodos') / (agregados * (agregados - 1.0)) }
	where
	nodos' = contarNodos grafo
	arcos' = contarArcos grafo
	agregados = sum arcos' / 2.0


-- Procesa una lista de ficheros GML, calculando las estadísticas de los grafos
-- que encuentra y mostrando el resultado en pantalla, en formato CSV.
procesarFicheros :: [FilePath] -> IO ()
procesarFicheros paths = do
	putStrLn cabecera
	forM_ paths $ \path -> (do
		grafo <- cargarGrafo path
		print $ calcularEstadisticas path grafo
		) `Control.Exception.catch` (\e -> putStrLn $ "Error en el fichero " ++ path ++ ": " ++ show (e :: SomeException))
