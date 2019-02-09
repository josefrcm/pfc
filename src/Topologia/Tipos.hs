-------------------------------------------------------------------------------
-- Autor: José Franco Campos
--
-- Generación, análisis y optimización de escenarios de migración
-- de redes Ipv4/IPv6 mediante programación funcional.
--
-- Diciembre 2008
--
--
-- Descripción: definición de los tipos básicos usados en el almacenamiento
--              de topologías.
-------------------------------------------------------------------------------

module Topologia.Tipos where


import Data.Map


-------------------------------------------------------------------------------
-- Definición del árbol sintáctico GML
-------------------------------------------------------------------------------

-- | Posibles tipos de nodos de un árbol sintáctico.
data Valor
	= VI Int
	| VR Double
	| VS String
	| VM Atributos
	deriving (Eq, Show)

-- | Lista de pares clave-valor, indexados por clave.
type Atributos = Map String [Valor]



-------------------------------------------------------------------------------
-- Tipos básicos
-------------------------------------------------------------------------------

-- | Identificador de un nodo.
type Id = Int

-- | Ruta en el grafo (lista de nodos por los que pasa).
type Ruta = [Id]



-------------------------------------------------------------------------------
-- Representación en memoria de un grafo
-------------------------------------------------------------------------------

-- | Representación de un vector en el espacio euclídeo R³.
data Vector = Vector {
	x, y, z :: !Double
	}
	deriving (Eq, Show)


-- | Atributos gráficos de un nodo
data Graficos = Graficos {
	center :: Vector,
	atr_graficos :: Atributos
	}
	deriving (Eq, Show)


-- | Clases de nodos intermedios (routers).
data Router
	= R4        -- ^ Router IPv4
	| R6        -- ^ Router IPv6
	| Rd        -- ^ Router dual
	| R4_td     -- ^ Router IPv4 con traducción de direcciones
	| Rd_td     -- ^ Router dual con traducción de direcciones
	| Rd_tp     -- ^ Router dual con traducción de protocolos
	| Rd_tptd   -- ^ Router dual con traducción de protocolos y direcciones
	| Z4        -- ^ Zona IPv4
	| Z6        -- ^ Zona IPv6
	| Zd        -- ^ Zona dual
	deriving (Eq, Ord, Enum, Show, Read)


-- | Atributos de un nodo.
data Nodo = Nodo {
	ident :: !Id,
	protocolo :: Router,
	graficos :: Graficos,
 	nombre :: String,
 	etiqueta :: String,
	atr_nodo :: Atributos
	}
	deriving (Eq, Show)


-- | Atributos de un arco.
data Arco = Arco {
	origen :: !Id,
	destino :: !Id,
	latencia :: !Double,
	anchoBanda :: !Double,
	metrica :: !Double,
	atr_arco :: Atributos
	}
	deriving (Eq, Show)
