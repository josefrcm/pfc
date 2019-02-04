-------------------------------------------------------------------------------
-- Autor: José Franco Campos
-- 
-- Generación, análisis y optimización de escenarios de migración
-- de redes Ipv4/IPv6 mediante programación funcional.
-- 
-- Diciembre 2008
--
--
-- Descripción: implementación de las reglas de formalización. Este módulo
--              engloba tanto la fase de creación de zonas como la de estable-
--              cimiento de los operadores de conexión.
-------------------------------------------------------------------------------

module Reglas.Formalizacion (
	formalizar
	) where

-- Módulos del programa
import Reglas.Tipos
import Topologia


-------------------------------------------------------------------------------
-- Implementación de las reglas
-------------------------------------------------------------------------------

-- Aplica las reglas a un escenario, devolviendo el escenario formalizado
-- y la lista de cambios.
formalizar :: Escenario -> (Escenario, [Cambio])
formalizar (a1,n1,[],n2,a2) = ((a1,n1,[(Directa,Zd)],n2,a2), [Cambio ZonaD 0 0 1])
formalizar (a1,n1,red,n2,a2) = ((a1,n1,formalizada,n2,a2), cambios)
	where
	(formalizada, cambios) = loop (head red) (tail red) 1 ([], [])



-- Recorre la lista de routers, agrupando routers contiguos según las reglas
-- de emparejamiento. Los argumentos son:
--	* Primer router de la red (cabeza).
--	* Resto de routers de la red.
--	* Posición del primer router en la red, para anotar el origen de los
--	  cambios. Avanza cada vez que se añaden un nuevo router a la lista
--	  de resultados.
--	* Acumulador con la lista de resultados temporales.
loop :: (Conexion, Router) -> Red -> Int -> (Red, [Cambio]) -> (Red, [Cambio])

-- Termina cuando ya no quedan más routers.
loop (op,R4_td)   [] _ (racc, cacc) = (reverse ((op,Z4):racc) , cacc)
loop (op,Rd_td)   [] _ (racc, cacc) = (reverse ((op,Zd):racc) , cacc)
loop (op,Rd_tp)   [] _ (racc, cacc) = (reverse ((op,Zd):racc) , cacc)
loop (op,Rd_tptd) [] _ (racc, cacc) = (reverse ((op,Zd):racc) , cacc)
loop (op,router)  [] _ (racc, cacc) = (reverse ((op,router):racc) , cacc)

-- Conexión de routers básicos al principio de la red.
loop (op,R4) rs pos (racc, cacc) = loop (op,Z4) rs pos (racc, cacc)
loop (op,R6) rs pos (racc, cacc) = loop (op,Z6) rs pos (racc, cacc)
loop (op,Rd) rs pos (racc, cacc) = loop (op,Zd) rs pos (racc, cacc)

-- Conexión de dos routers diferentes.
loop actual (r:rs) pos (racc, cacc) = loop actual' rs pos' (racc', cacc')
	where
	(actual', router, cambio) = emparejar pos actual r
	(pos', racc') = case router of
		Nothing -> (pos, racc)
		Just r -> (pos+1, r:racc)
	cacc' = cambio : cacc



-------------------------------------------------------------------------------
-- Esta función conecta dos routers contiguos que se encuentran en la posición
-- (i, i+1). Devuelve el nuevo router que irá en cabeza, el router que hay
-- que añadir a la lista de resultados, y el cambio procedente de la regla.
-------------------------------------------------------------------------------

emparejar :: Int -> (Conexion,Router) -> (Conexion,Router) -> ((Conexion,Router), Maybe (Conexion,Router), Cambio)

-- Conexión de una zona IPv4.
emparejar i (op,Z4) (_,R4)      = ((op,Z4), Nothing, Cambio Formalizar i 2 1)
emparejar i (op,Z4) (_,R6)      = ((Directa,Z6), Just (op,Z4), Cambio Formalizar i 2 2) -- error
emparejar i (op,Z4) (_,Rd)      = ((OpD,Zd), Just (op,Z4), Cambio Formalizar i 2 2)
emparejar i (op,Z4) (_,R4_td)   = ((Op4_td,Z4), Just (op,Z4), Cambio Formalizar i 2 2)
emparejar i (op,Z4) (_,Rd_td)   = ((OpD_td,Zd), Just (op,Z4), Cambio Formalizar i 2 2)
emparejar i (op,Z4) (_,Rd_tp)   = ((OpD_tp,Zd), Just (op,Z4), Cambio Formalizar i 2 2)
emparejar i (op,Z4) (_,Rd_tptd) = ((OpD_tptd,Zd), Just (op,Z4), Cambio Formalizar i 2 2)

-- Conexión de una zona IPv6.
emparejar i (op,Z6) (_,R4)      = ((Directa,Z4), Just (op,Z6), Cambio Formalizar i 2 2) -- error
emparejar i (op,Z6) (_,R6)      = ((op,Z6), Nothing, Cambio Formalizar i 2 1)
emparejar i (op,Z6) (_,Rd)      = ((OpD,Zd), Just (op,Z6), Cambio Formalizar i 2 2)
emparejar i (op,Z6) (_,R4_td)   = ((Directa,R4_td), Just (op,Z6), Cambio Formalizar i 2 2) -- error
emparejar i (op,Z6) (_,Rd_td)   = ((OpD_td,Zd), Just (op,Z6), Cambio Formalizar i 2 2)
emparejar i (op,Z6) (_,Rd_tp)   = ((OpD_tp,Zd), Just (op,Z6), Cambio Formalizar i 2 2)
emparejar i (op,Z6) (_,Rd_tptd) = ((OpD_tptd,Zd), Just (op,Z6), Cambio Formalizar i 2 2)

-- Conexión de una zona dual.
emparejar i (op,Zd) (_,R4)      = ((OpD,Z4), Just (op,Zd), Cambio Formalizar i 2 2)
emparejar i (op,Zd) (_,R6)      = ((OpD,Z6), Just (op,Zd), Cambio Formalizar i 2 2)
emparejar i (op,Zd) (_,Rd)      = ((op,Zd), Nothing, Cambio Formalizar i 2 1)
emparejar i (op,Zd) (_,R4_td)   = ((Op4_td,Z4), Just (op,Zd), Cambio Formalizar i 2 2)
emparejar i (op,Zd) (_,Rd_td)   = ((OpD_td,Zd), Just (op,Zd), Cambio Formalizar i 2 2)
emparejar i (op,Zd) (_,Rd_tp)   = ((OpD_tp,Zd), Just (op,Zd), Cambio Formalizar i 2 2)
emparejar i (op,Zd) (_,Rd_tptd) = ((OpD_tptd,Zd), Just (op,Zd), Cambio Formalizar i 2 2)
	
-- Conexión de un router IPv4 con traducción de direcciones.
emparejar i (op,R4_td) (_,R4)      = ((Op4_td,Z4), Just (op,Z4), Cambio Formalizar i 2 2)
emparejar i (op,R4_td) (_,R6)      = ((Directa,Z6), Just (op,R4_td), Cambio Formalizar i 2 2) -- error
emparejar i (op,R4_td) (_,Rd)      = ((OpD_td,Zd), Just (op,Z4), Cambio Formalizar i 2 2)
emparejar i (op,R4_td) (_,R4_td)   = ((Op4_td,R4_td), Just (op,Z4), Cambio Formalizar i 2 2)
emparejar i (op,R4_td) (_,Rd_td)   = ((OpD_td,Rd_td), Just (op,Z4), Cambio Formalizar i 2 2)
emparejar i (op,R4_td) (_,Rd_tp)   = ((OpD_tp,Rd_tp), Just (op,Z4), Cambio Formalizar i 2 2)
emparejar i (op,R4_td) (_,Rd_tptd) = ((OpD_tptd,Rd_tptd), Just (op,Z4), Cambio Formalizar i 2 2)

-- Conexión de un router dual con traducción de direcciones.
emparejar i (op,Rd_td) (_,R4)      = ((OpD_td,Z4), Just (op,Zd), Cambio Formalizar i 2 2)
emparejar i (op,Rd_td) (_,R6)      = ((OpD_td,Z6), Just (op,Zd), Cambio Formalizar i 2 2)
emparejar i (op,Rd_td) (_,Rd)      = ((OpD_td,Zd), Just (op,Zd), Cambio Formalizar i 2 2)
emparejar i (op,Rd_td) (_,R4_td)   = ((Op4_td,R4_td), Just (op,Zd), Cambio Formalizar i 2 2)
emparejar i (op,Rd_td) (_,Rd_td)   = ((OpD_td,Rd_td), Just (op,Zd), Cambio Formalizar i 2 2)
emparejar i (op,Rd_td) (_,Rd_tp)   = ((OpD_tp,Rd_tp), Just (op,Zd), Cambio Formalizar i 2 2)
emparejar i (op,Rd_td) (_,Rd_tptd) = ((OpD_tptd,Rd_tptd), Just (op,Zd), Cambio Formalizar i 2 2)

-- Conexión de un router dual con traducción de protocolos.
emparejar i (op,Rd_tp) (_,R4)      = ((OpD_tp,Z4), Just (op,Zd), Cambio Formalizar i 2 2)
emparejar i (op,Rd_tp) (_,R6)      = ((OpD_tp,Z6), Just (op,Zd), Cambio Formalizar i 2 2)
emparejar i (op,Rd_tp) (_,Rd)      = ((OpD_tp,Zd), Just (op,Zd), Cambio Formalizar i 2 2)
emparejar i (op,Rd_tp) (_,R4_td)   = ((Op4_td,R4_td), Just (op,Zd), Cambio Formalizar i 2 2)
emparejar i (op,Rd_tp) (_,Rd_td)   = ((OpD_td,Rd_td), Just (op,Zd), Cambio Formalizar i 2 2)
emparejar i (op,Rd_tp) (_,Rd_tp)   = ((OpD_tp,Rd_tp), Just (op,Zd), Cambio Formalizar i 2 2)
emparejar i (op,Rd_tp) (_,Rd_tptd) = ((OpD_tptd,Rd_tptd), Just (op,Zd), Cambio Formalizar i 2 2)

-- Conexión de un router dual con traducción de direcciones y protocolos.
emparejar i (op,Rd_tptd) (_,R4)      = ((OpD_tptd,Z4), Just (op,Zd), Cambio Formalizar i 2 2)
emparejar i (op,Rd_tptd) (_,R6)      = ((OpD_tptd,Z6),  Just (op,Zd), Cambio Formalizar i 2 2)
emparejar i (op,Rd_tptd) (_,Rd)      = ((OpD_tptd,Zd), Just (op,Zd), Cambio Formalizar i 2 2)
emparejar i (op,Rd_tptd) (_,R4_td)   = ((Op4_td,R4_td), Just (op,Zd), Cambio Formalizar i 2 2)
emparejar i (op,Rd_tptd) (_,Rd_td)   = ((OpD_td,Rd_td),  Just (op,Zd), Cambio Formalizar i 2 2)
emparejar i (op,Rd_tptd) (_,Rd_tp)   = ((OpD_tp,Rd_tp), Just (op,Zd), Cambio Formalizar i 2 2)
emparejar i (op,Rd_tptd) (_,Rd_tptd) = ((OpD_tptd,Rd_tptd), Just (op,Zd), Cambio Formalizar i 2 2)

-- Cualquier otro caso es un error.
emparejar _ _ _ = error "Situación incorrecta"
