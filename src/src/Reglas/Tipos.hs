-------------------------------------------------------------------------------
-- Autor: José Franco Campos
-- 
-- Generación, análisis y optimización de escenarios de migración
-- de redes Ipv4/IPv6 mediante programación funcional.
-- 
-- Diciembre 2008
--
--
-- Descripción: definición de los tipos de datos necesarios para la resolución
--              de escenarios de transición.
-------------------------------------------------------------------------------

module Reglas.Tipos where


-- Módulos del programa.
import Topologia


-- | Clases de aplicaciones.
data Aplicacion
	= A4 -- ^ Aplicación IPv4.
	| A6 -- ^ Aplicación IPv6.
	| Ad -- ^ Aplicación dual.
	deriving (Show, Read, Eq, Enum)


-- | Clases de nodos terminales.
data NodoTerminal
	= N4   -- ^ Nodo IPv4.
	| N6   -- ^ Nodo IPv6.
	| Nd   -- ^ Nodo dual.
	| Nmap -- ^ Nodo dual con direcciones IPv4-mapped IPv6.
	deriving (Show, Read, Eq, Enum)


-- | Operadores de conexión entre los routers.
data Conexion = Directa    -- ^ Conexión directa.
              | OpD        -- ^ Conexión dual.
              | Op4_td     -- ^ Conexión IPv4 con traducción de direcciones.
              | OpD_td     -- ^ Conexión dual con traducción de direcciones.
              | OpD_tp     -- ^ Conexión dual con traducción de protocolos.
              | OpD_tptd   -- ^ Conexión dual con traducción de protocolos y direcciones.
              deriving (Show, Eq, Enum)


-- | Red entre dos nodos finales
type Red = [(Conexion, Router)]


-- | Escenario de transición.
type Escenario = (Aplicacion, NodoTerminal, Red, NodoTerminal, Aplicacion)


-- | Cambios resultado de aplicar una regla.
data TipoCambio
	= Formalizar
	| Zona4
	| Zona6
	| ZonaD
	| Operador1
	| Operador2
	| Operador3
	| Operador4
	| Operador5
	| Canonizacion1
	| Canonizacion2
	| Canonizacion3
	| Canonizacion4
	| Canonizacion5
	| Canonizacion6
	| Canonizacion7
	| Canonizacion8
	| ZZ_manual_4en6 -- ^ Túnel manual IPv4 dentro de IPv6.
	| ZZ_manual_6en4 -- ^ Túnel manual IPv6 dentro de IPv4.
	| ZZ_manual_udp  -- ^ Túnel manual IPv6 dentro de IPv4 UDP.
	| ZZ_broker_4en6 -- ^ Broker de túneles IPv4 dentro de IPv6.
	| ZZ_broker_6en4 -- ^ Broker de túneles IPv6 dentro de IPv4.
	| ZZ_broker_udp  -- ^ Broker de túneles IPv6 dentro de IPv4 UDP.
	| ZZ_6to4        -- ^ Túnel 6to4.
	| ZN_manual_4en6 -- ^ Túnel manual IPv4 dentro de IPv6.
	| ZN_manual_6en4 -- ^ Túnel manual IPv6 dentro de IPv4.
	| ZN_manual_udp  -- ^ Túnel manual IPv6 dentro de IPv4 UDP.
	| ZN_broker_4en6 -- ^ Broker de túneles IPv4 dentro de IPv6.
	| ZN_broker_6en4 -- ^ Broker de túneles IPv6 dentro de IPv4.
	| ZN_broker_udp  -- ^ Broker de túneles IPv6 dentro de IPv4 UDP.
	| ZN_6to4        -- ^ Túnel 6to4.
	| ZN_dstm        -- ^ Túnel DSTM.
	| ZN_teredo      -- ^ Túnel Teredo.
	| ZN_isatap      -- ^ Tunel ISATAP.
	| NN_manual_4en6 -- ^ Túnel manual IPv4 dentro de IPv6.
	| NN_manual_6en4 -- ^ Túnel manual IPv6 dentro de IPv4.
	| NN_manual_udp  -- ^ Túnel manual IPv6 dentro de IPv4 UDP.
	| NN_isatap      -- ^ Túnel ISATAP.
	| NN_teredo      -- ^ Túnel Teredo.
	| Dual_stack     -- ^ Instalación de mecanismo Dual Stack.
	| Mapped         -- ^ Instalación de mecanismo Mapped.
	| Traduccion     -- ^ Mecanismo de traducción genérico.
	| NAT_PT         -- ^ Traducción NAT-PT.
	| SIIT           -- ^ Traducción SIIT.
	| SOCKS          -- ^ Traducción SOCKS.
	| TRT            -- ^ Traducción TRT.
	| Nada           -- ^ Se ha modificado el escenario sin aplicar ningún mecanismo.
	deriving (Show, Eq, Ord)


-- | Tipos de mecanismos aplicados. Se usan para calcular el coste de las
-- soluciones a partir de los tipos de cambios.
data Mecanismo
	= MT_nada       -- ^ Mecanismo nulo.
	| MT_dual_stack -- ^ Mecanismo Dual Stack
	| MT_mapped     -- ^ Mecanismo Mapped.
	| MT_broker     -- ^ Broker de túneles.
	| MT_dstm       -- ^ Túnel DSTM.
	| MT_isatap     -- ^ Túnel ISATAP.
	| MT_6to4       -- ^ Túnel 6to4.
	| MT_teredo     -- ^ Túnel Teredo.
	| MT_auto       -- ^ Túnel automático.
	| MT_nat_pt     -- ^ Traducción NAT-PT.
	| MT_siit       -- ^ Traducción SIIT.
	| MT_socks      -- ^ Traducción SOCKS.
	| MT_trt        -- ^ Traducción TRT.
	deriving (Show, Eq, Ord)


-- | Los cambios se representan como una tupla:
--	* Regla aplicada.
--	* Nodo a partir del cual se aplica la regla.
--	* Número de nodos a los que se aplica la regla.
--	* Número de nodos después de aplicar la regla.
--    Nota: el primer nodo terminal tiene la posición 0, los routers inter-
--    medios las posiciones 1..n, y el segundo nodo terminal la posición n+1.
data Cambio = Cambio {
		tipo :: TipoCambio,
		inicio, antes, despues :: !Int
	}
	deriving (Show, Eq)


-- | Representación de una posible solución:
--	* Ruta por la que pasa la solución.
--	* Escenario antes y después de aplicar los cambios.
--	* Lista de cambios.
--	* Coste de la solución.
data Solucion = Solucion {
	ruta :: [Int],
	inicial, final :: Escenario,
	lcambios :: [Cambio],
	coste_lat, coste_bw, coste_met :: Double
	} deriving (Show, Eq)


-- | Representación de una regla de transición. Las reglas devuelven Nothing
-- si no se pueden aplicar a un escenario, y Just (e, c) si se han podido
-- aplicar, siendo 'e' el nuevo escenario y 'c' el cambio resultante.
type ReglaC = Escenario -> Maybe (Escenario, Cambio)
type ReglaP = Red -> Maybe (Red, Cambio)
