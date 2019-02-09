{-# OPTIONS_GHC -F -pgmF trhsx  #-}
{-# LANGUAGE PatternGuards #-}

-------------------------------------------------------------------------------
-- Autor: José Franco Campos
-- 
-- Generación, análisis y optimización de escenarios de migración
-- de redes Ipv4/IPv6 mediante programación funcional.
-- 
-- Diciembre 2008
--
--
-- Descripción: implementación de las reglas de creación de zonas.
-- Notas: este módulo está obsoleto, es mejor usar el módulo Formalización.
-------------------------------------------------------------------------------

module Reglas.Zonas (crearZonas) where


-- Módulos del programa
import Reglas.Tipos
import Topologia



-------------------------------------------------------------------------------
-- Reglas de creación de zonas. Estas reglas se encargan de agrupar routers
-- del mismo tipo en una zona común, con el objetivo de simplificar la
-- topología y acelerar el procesamiento posterior. Por ejemplo, la red
-- [R4, R4, R4] se convertirá en [Z4].
-------------------------------------------------------------------------------

-- Aplica las reglas a un escenario, devolviendo el escenario modificado y
-- la lista de cambios correspondiente.
crearZonas :: Escenario -> (Escenario, [Cambio])
crearZonas (a1,n1,[],n2,a2) = ((a1,n1,[(Directa,Zd)],n2,a2) , [Cambio ZonaD 0 0 1])
crearZonas (a1,n1,red,n2,a2) = ((a1,n1,nueva,n2,a2) , cambios)
	where
	(nueva, cambios) = zonas red


-- Bucle interno de la aplicación de las reglas, compacta una secuencia de
-- nodos consecutivos del mismo tipo.
zonas :: Red -> (Red, [Cambio])
zonas red = loop red 1 [] [] 
	where
	loop :: Red -> Int -> Red -> [Cambio] -> (Red, [Cambio])
	-- Secuencia de nodos IPv4
	loop [/ r@(Directa,R4)+! , rs@_* /] i racc cacc =
		loop rs (i+1) ((Directa,Z4) : racc) ((Cambio Zona4 i (length r) 1) : cacc)

	-- Secuencia de nodos IPv6
	loop [/ r@(Directa,R6)+! , rs@_* /] i racc cacc =
		loop rs (i+1) ((Directa,Z6) : racc) ((Cambio Zona6 i (length r) 1) : cacc)
	
	-- Secuencia de nodos duales
	loop [/ r@(Directa,Rd)+! , rs@_* /] i racc cacc =
		loop rs (i+1) ((Directa,Zd) : racc) ((Cambio ZonaD i (length r) 1) : cacc)

	-- Otros casos
	loop (r:rs) i racc cacc = loop rs (i+1) (r:racc) cacc
	loop _ _ racc cacc = (reverse racc, cacc)
