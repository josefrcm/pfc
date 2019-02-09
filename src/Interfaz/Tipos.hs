-------------------------------------------------------------------------------
-- Autor: José Franco Campos
-- 
-- Generación, análisis y optimización de escenarios de migración
-- de redes Ipv4/IPv6 mediante programación funcional.
-- 
-- Diciembre 2008
--
--
-- Descripción: definición de los tipos de datos usados en la implementación
--              de la interfaz de usuario.
-------------------------------------------------------------------------------

module Interfaz.Tipos where


-- Módulos externos
import Data.Map as Map
import Data.Set as Set
import Data.IntSet as IntSet
import Graphics.UI.Gtk

-- Módulos del programa
import Reglas
import Topologia



-------------------------------------------------------------------------------
-- Tipos de datos.
-------------------------------------------------------------------------------

-- Correspondencia entre tipos de routers y colores.
type Colores = Map.Map Router Color


-- Modo actual del editor.
data Modo
	= EditarNodos
	| EditarEnlaces
	| CrearNodos
	| CrearEnlaces
	deriving (Eq, Show)


-- Conjunto de nodos seleccionados.
data Seleccion = Sel {
	lista_ids :: IntSet,
	pos_vieja :: Vector,
	pos_nueva :: Vector
	}
	deriving Show


-- Configuración del programa.
data Configuracion = Conf {
	sel_nodos :: Seleccion,          -- Conjunto de nodos seleccionados
	sel_arcos :: Set (Id, Id),       -- Conjunto de arcos seleccionados
	zoom :: Double,                  -- Nivel de zoom
	modo :: Modo,                    -- Modo de edición activo
	prot :: Router,                  -- Protocolo de los nuevos routers
	colores :: Colores,              -- Asignación de colores a los routers
	opciones :: Maybe Opciones,      -- Opciones de búsqueda
	costes :: (Coste, Coste, Coste), -- Tablas de costes
	soluciones :: [Solucion],        -- Lista de soluciones encontradas
	sol_sel :: Maybe Solucion,       -- Solución seleccionada
	pulsado :: Bool,                 -- Indica si el usuario mantiene pulsado el ratón
	fichero :: Maybe FilePath,       -- Nombre del fichero GML abierto
	modificado :: Bool,              -- Indica si el grafo ha sido modificado y no se ha guardado aún
	topologia :: !Grafo              -- Topología del escenario
	}
