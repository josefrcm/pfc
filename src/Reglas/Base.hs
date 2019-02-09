-------------------------------------------------------------------------------
-- Autor: José Franco Campos
--
-- Generación, análisis y optimización de escenarios de migración
-- de redes Ipv4/IPv6 mediante programación funcional.
--
-- Diciembre 2008
--
--
-- Descripción: funciones comunes a la implementación de varios tipos de reglas
--              de transición.
-------------------------------------------------------------------------------

module Reglas.Base (
	desplazar,
	aplicarR,
	aplicarRS,
	subset
	) where


-- Módulos del programa
import Reglas.Tipos


-------------------------------------------------------------------------------
-- Funciones.
-------------------------------------------------------------------------------

-- Mueve un cambio en un escenario a una nueva posición inicial.
desplazar :: Cambio -> Int -> Cambio
desplazar cambio n = cambio {inicio = inicio cambio + n}


-- Aplica una regla parcial a todas las posiciones posibles de un escenario,
-- devolviendo el nuevo escenario y la lista de cambios resultante.
aplicarR :: ReglaP -> Escenario -> (Escenario, [Cambio])
aplicarR regla (a1,n1,red,n2,a2) = ((a1,n1,red',n2,a2), cambios)
	where
	(red', cambios) = loop red 0 ([], [])
	
	-- Intenta aplicar una regla a un escenario. Si tiene exito, vuelve
	-- a intentarlo, y si no pasa a la siguiente posición.
	loop :: Red -> Int -> (Red, [Cambio]) -> (Red, [Cambio])
	loop [] _ (ns, cs) = (reverse ns, cs)
	loop vieja@(x:xs) pos (ns, cs) = case regla vieja of
		Just (nueva, cambio) -> loop nueva pos (ns, cambio `desplazar` pos : cs)
		Nothing -> loop xs (pos+1) (x:ns, cs)


-- Aplica un conjunto de reglas parciales a un escenario hasta que ya no puede
-- simplificarse más. Devuelve el nuevo escenario, y la lista de cambios en
-- orden inverso (primero el último cambio).
aplicarRS :: [ReglaP] -> Escenario -> (Escenario, [Cambio])
aplicarRS reglas e = loop reglas (e, [])
	where

	-- Aplica las reglas secuencialmente, acumulando los cambios.
	loop :: [ReglaP] -> (Escenario, [Cambio]) -> (Escenario, [Cambio])
	loop [] (viejo, acc) = (viejo, acc)
	loop (r:rs) (viejo, acc) = loop rs (nuevo, cambios ++ acc)
		where
		(nuevo, cambios) = aplicarR r viejo


-- Devuelve True si todos los elementos de la primera lista pertenecen también
-- a la segunda lista, y False en caso contrario.
subset :: Eq a => [a] -> [a] -> Bool
subset xs ys = all (\x -> elem x ys) xs
