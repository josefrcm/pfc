-------------------------------------------------------------------------------
-- Autor: José Franco Campos
--
-- Generación, análisis y optimización de escenarios de migración
-- de redes Ipv4/IPv6 mediante programación funcional.
--
-- Diciembre 2008
--
--
-- Descripción: implementación de la estrategia de resolución de escenarios.
-------------------------------------------------------------------------------

module Reglas.Solucion (
	Opciones (
		Opciones,
		nodo1,
		nodo2,
		aplicacion1,
		aplicacion2,
		protocolo1,
		protocolo2,
		longitud,
		elim_bucles,
		forzar_ady),
	Coste,
	buscarSoluciones,
	comparar_lat,
	comparar_bw,
	comparar_met,
	comparar_ruta
	) where


-- Módulos externos
import Data.List
import Data.Maybe
import qualified Data.Map as Map

-- Módulos del programa
import Reglas.Base
import Reglas.Formalizacion
import Reglas.Canonizacion
import Reglas.Mecanismos
import Reglas.Tipos
import Reglas.Tuneles
import Topologia



------------------------------------------------------------------------------
-- Representación y generación del árbol de búsqueda.
------------------------------------------------------------------------------

-- Representación de un árbol n-ario
data Arbol = Rama Escenario [Cambio] [Arbol]


-- Aplica una regla completa a un escenario. Devuelve una lista vacía si no
-- se ha podido aplicar.
aplicarReglaC :: ReglaC -> Escenario -> [(Escenario, Cambio)]
aplicarReglaC regla escenario = maybeToList (regla escenario)


-- Aplica una regla parcial a todas las posiciones posibles de un escenario.
-- Devuelve una lista con todos los resultados.
aplicarReglaP :: ReglaP -> Escenario -> [(Escenario, Cambio)]
aplicarReglaP regla (a1,n1,red,n2,a2) = [((a1,n1,red',n2,a2), cs) | (red', cs) <- loop red 0]
	where
	loop [] _ = []
	loop vieja@(v:vs) pos = case regla vieja of
		Just (nueva, cambio) -> (nueva, cambio `desplazar` pos) : cola
		Nothing -> cola
		where
		cola = map (\(r, c) -> (v:r, c)) $ loop vs (pos+1)


-- Expande un nodo del árbol, probando todas las reglas posibles.
expandirNodo :: [ReglaP] -> [ReglaC] -> Escenario -> [(Escenario, Cambio)]
expandirNodo rp rc escenario = ep ++ ec 
	where
	ep = concat [aplicarReglaP r escenario | r <- rp]
	ec = concat [aplicarReglaC r escenario | r <- rc]


-- Genera el árbol a partir de la red inicial.
generarArbol :: Escenario -> Arbol
generarArbol escenario = fase1 escenario []


-- Devuelve una lista con los nodos terminales del árbol.
terminales :: Ruta -> Arbol -> [Solucion]
terminales ids (Rama original cambios hijos) = if null hijos
	then [Solucion ids original original (reverse cambios) 0 0 0]
	else concatMap loop hijos
		where
		loop (Rama e c []) = [Solucion ids original e (reverse c) 0 0 0]
		loop (Rama _ _ zs) = concatMap loop zs


-- Genera todas las soluciones correspondientes a una ruta	
generarSoluciones :: Ruta -> Escenario -> [Solucion]
generarSoluciones ids escenario = terminales ids $ generarArbol escenario



-------------------------------------------------------------------------------
-- Fases de la generación del árbol.
-------------------------------------------------------------------------------

-- La primera fase de la resolución de escenarios consiste en la ejecución de
-- las etapas de creación de zonas, establecimiento de los operadores de
-- conexión, y canonización.
fase1 :: Escenario -> [Cambio] -> Arbol
fase1 escenario cambios = Rama escenario cambios [fase2 escenario2 total]
	where
	(escenario1, cambios1) = formalizar escenario
	(escenario2, cambios2) = canonizar escenario1
	total = cambios2 ++ cambios1


-- La segunda fase de la resolución es la creación de túneles ZZ y ZN.
fase2 :: Escenario -> [Cambio] -> Arbol
fase2 escenario cambios = if null hijos
	then fase3 escenario cambios
	else Rama escenario cambios [fase2 e (c:cambios) | (e,c) <- hijos]
	where
	hijos = expandirNodo tuneles_zz tuneles_zn escenario
		

-- La tercera fase es la creación de túneles NN.
fase3 :: Escenario -> [Cambio] -> Arbol
fase3 escenario cambios = Rama escenario cambios (cabeza:cola)
	where
	hijos1 = expandirNodo [] tuneles_nn escenario
	cabeza = fase4 escenario cambios
	cola   = [fase4 e (c:cambios) | (e,c) <- hijos1]


-- La cuarta fase es la aplicación de otro tipo de mecanismos.
fase4 :: Escenario -> [Cambio] -> Arbol
fase4 escenario cambios = if solucion escenario
	-- Si el escenario es solución, termina.
	then Rama escenario cambios []
	-- Si no lo es, sigue probando.
	else Rama escenario cambios [fase4 e (c:cambios) | (e,c) <- hijos]
		where
		hijos = expandirNodo [] mecanismos escenario



-------------------------------------------------------------------------------
-- Fases de la búsqueda de soluciones.
-------------------------------------------------------------------------------

-- Definición de la tabla de costes.
type Coste = Map.Map Mecanismo Double


-- Opciones de la búsqueda.
data Opciones = Opciones {
	nodo1, nodo2 :: Id,
	aplicacion1, aplicacion2 :: Aplicacion,
	protocolo1, protocolo2 :: NodoTerminal,
	longitud :: Maybe Int,
	elim_bucles :: Bool,
	forzar_ady :: Bool
	}
	deriving (Show, Eq)


-- Devuelve True si un escenario inicial es correcto, y False en caso contrario.
correcto :: Escenario -> Bool
correcto (_, _, red, _, _) = loop red
	where
	loop ((_,R4):(Directa,R6):_) = False
	loop ((_,R6):(Directa,R4):_) = False
	loop ((_,R6):(Directa,R4_td):_) = False
	loop (_:xs) = loop xs
	loop _ = True


-- Ejemplo de como quedaría la búsqueda completa
buscarSoluciones :: Opciones -> (Coste, Coste, Coste) -> Grafo -> [Solucion]
buscarSoluciones op costes grafo = paso4
	where
	-- Busca todas las rutas entre el origen y el destino
	paso1 :: [([Id],[Router])]
	paso1 = buscarRutas (longitud op, elim_bucles op, forzar_ady op) grafo (nodo1 op) (nodo2 op)
	
	-- Convierte las rutas en escenarios
	paso2 :: [(Ruta, Escenario)]
	paso2 = [(r1, e) | (r1, r2) <- paso1, let e = (aplicacion1 op, protocolo1 op, convertir r2, protocolo2 op, aplicacion2 op)]
	
	-- Busca todas las soluciones
	paso3 :: [Solucion]
	--paso3 = filter (\s -> solucion (final s)) $ concat [generarSoluciones r e | (r,e) <- paso2, correcto e]
	paso3 = concat [generarSoluciones r e | (r,e) <- paso2, correcto e]
	
	-- Calcula el coste de las soluciones
	paso4 :: [Solucion]
	paso4 = [calcularCoste grafo costes sol | sol <- paso3]


-- Convierte una secuencia de nodos en una red de distribución.
convertir :: [Router] -> [(Conexion, Router)]
convertir r = [(Directa, nodo) | nodo <- r]



-------------------------------------------------------------------------------
-- Evaluación del coste de una solución.
-------------------------------------------------------------------------------

-- Calcula el coste de una solución.
calcularCoste :: Grafo -> (Coste, Coste, Coste) -> Solucion -> Solucion
calcularCoste grafo (tabla_lat, tabla_bw, tabla_met) sol = sol {coste_lat = c1+e1, coste_bw = c2+e2, coste_met = c3+e3}
	where
	c1 = sum [Map.findWithDefault 0.0 (traducir $ tipo c) tabla_lat | c <- lcambios sol]
	c2 = sum [Map.findWithDefault 0.0 (traducir $ tipo c) tabla_bw  | c <- lcambios sol]
	c3 = sum [Map.findWithDefault 0.0 (traducir $ tipo c) tabla_met | c <- lcambios sol]
	
	e1 = sum (map latencia $ camino grafo (ruta sol))
	e2 = sum (map anchoBanda $ camino grafo (ruta sol))
	e3 = sum (map metrica $ camino grafo (ruta sol))
		

-- Compara dos soluciones por latencia.
comparar_lat :: Solucion -> Solucion -> Ordering
comparar_lat s1 s2 = (coste_lat s1) `compare` (coste_lat s2)


-- Compara dos soluciones por consumo de ancho de banda.
comparar_bw :: Solucion -> Solucion -> Ordering
comparar_bw s1 s2 = (coste_bw s1) `compare` (coste_bw s2)


-- Compara dos soluciones por métrica definida por el usuario.
comparar_met :: Solucion -> Solucion -> Ordering
comparar_met s1 s2 = (coste_met s1) `compare` (coste_met s2)


-- Compara dos soluciones según la ruta seguida.
comparar_ruta :: Solucion -> Solucion -> Ordering
comparar_ruta s1 s2 = (ruta s1) `compare` (ruta s2)


-- Traduce de tipos de cambios a mecanismos de transición.
traducir :: TipoCambio -> Mecanismo

-- Túneles automáticos
traducir ZZ_manual_4en6 = MT_auto
traducir ZZ_manual_6en4 = MT_auto
traducir ZZ_manual_udp  = MT_auto
traducir ZN_manual_4en6 = MT_auto
traducir ZN_manual_6en4 = MT_auto
traducir ZN_manual_udp  = MT_auto
traducir NN_manual_4en6 = MT_auto
traducir NN_manual_6en4 = MT_auto
traducir NN_manual_udp  = MT_auto

-- Broker de túneles
traducir ZZ_broker_4en6 = MT_broker
traducir ZZ_broker_6en4 = MT_broker
traducir ZZ_broker_udp  = MT_broker
traducir ZN_broker_4en6 = MT_broker
traducir ZN_broker_6en4 = MT_broker
traducir ZN_broker_udp  = MT_broker

-- Túnel 6to4
traducir ZZ_6to4        = MT_6to4
traducir ZN_6to4        = MT_6to4

-- Otros tipos de túneles
traducir ZN_dstm        = MT_dstm
traducir ZN_teredo      = MT_teredo
traducir ZN_isatap      = MT_isatap
traducir NN_isatap      = MT_isatap
traducir NN_teredo      = MT_teredo

-- Otros tipos de mecanismos
traducir Dual_stack     = MT_dual_stack
traducir Mapped         = MT_mapped
traducir NAT_PT         = MT_nat_pt
traducir SIIT           = MT_siit
traducir SOCKS          = MT_socks
traducir TRT            = MT_trt

-- Otros tipos de cambios
traducir _              = MT_nada
