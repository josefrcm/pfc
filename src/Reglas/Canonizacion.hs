{-# OPTIONS_GHC -F -pgmF trhsx #-}
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
-- Descripción: implementación de las reglas de canonización.
-- Notas: estas reglas están extendidas con respecto a las que aparecen en la
--        documentación, ya que permiten tratar las zonas duales como IPv4
--        o IPv6 según la necesidad.
-------------------------------------------------------------------------------

module Reglas.Canonizacion (
	canonizaciones,
	canonizar
	) where


-- Módulos del programa
import Reglas.Base
import Reglas.Tipos
import Topologia



-------------------------------------------------------------------------------
-- Reglas de canonización.
-------------------------------------------------------------------------------

-- Conexión de zonas IPv4 utilizando ⊗d.
can1 :: ReglaP
can1 [/ (conexion,Z4) , r@(OpD, Z4)+! , rs@_* /] = Just (
	(conexion,Z4) : rs,
	Cambio Canonizacion1 1 (1 + length r) 1)

can1 [/ (conexion,Z4) , r@(OpD, Zd)+! , rs@_* /] = Just (
	(conexion,Z4) : rs,
	Cambio Canonizacion1 1 (1 + length r) 1)

can1 [/ (conexion,Zd) , r@(OpD, Z4)+! , rs@_* /] = Just (
	(conexion,Z4) : rs,
	Cambio Canonizacion1 1 (1 + length r) 1)

can1 _ = Nothing


-- Conexión de zonas IPv4 utilizando ⊙d.
can2 :: ReglaP
can2 ((conexion,Z4) : (OpD_td,Z4) : rs) = Just (
	(conexion,Z4) : (Op4_td,Z4) : rs,
	Cambio Canonizacion2 1 2 2)

can2 ((conexion,Z4) : (OpD_td,Zd) : rs) = Just (
	(conexion,Z4) : (Op4_td,Z4) : rs,
	Cambio Canonizacion2 1 2 2)

can2 ((conexion,Zd) : (OpD_td,Z4) : rs) = Just (
	(conexion,Z4) : (Op4_td,Z4) : rs,
	Cambio Canonizacion2 1 2 2)

can2 _ = Nothing


-- Conexión de zonas IPv4 utilizando ⊙d.
can3 :: ReglaP
can3 ((conexion,Z4) : (OpD_tptd,Z4) : rs) = Just (
	(conexion,Z4) : (Op4_td,Z4) : rs,
	Cambio Canonizacion3 1 2 2)

can3 ((conexion,Z4) : (OpD_tptd,Zd) : rs) = Just (
	(conexion,Z4) : (Op4_td,Z4) : rs,
	Cambio Canonizacion3 1 2 2)

can3 ((conexion,Zd) : (OpD_tptd,Z4) : rs) = Just (
	(conexion,Z4) : (Op4_td,Z4) : rs,
	Cambio Canonizacion3 1 2 2)

can3 _ = Nothing


-- Conexión de zonas IPv6 utilizando ⊗d.
can4 :: ReglaP
can4 [/ (conexion,Z6), r@(OpD,Z6)+!, rs@_* /] = Just (
	(conexion,Z6) : rs,
	Cambio Canonizacion4 1 (1 + length r) 1)

can4 [/ (conexion,Z6), r@(OpD,Zd)+!, rs@_* /] = Just (
	(conexion,Z6) : rs,
	Cambio Canonizacion4 1 (1 + length r) 1)

can4 [/ (conexion,Zd), r@(OpD,Z6)+!, rs@_* /] = Just (
	(conexion,Z6) : rs,
	Cambio Canonizacion4 1 (1 + length r) 1)

can4 _ = Nothing


-- Conexión de zonas IPv6 utilizando ⊠d.
can5 :: ReglaP
can5 [/ (conexion,Z6), r@(OpD_tp,Z6)+!, rs@_* /] = Just (
	(conexion,Z6) : rs,
	Cambio Canonizacion5 1 (1 + length r) 1)

can5 [/ (conexion,Z6), r@(OpD_tp,Zd)+!, rs@_* /] = Just (
	(conexion,Z6) : rs,
	Cambio Canonizacion5 1 (1 + length r) 1)

can5 [/ (conexion,Zd), r@(OpD_tp,Z6)+!, rs@_* /] = Just (
	(conexion,Z6) : rs,
	Cambio Canonizacion5 1 (1 + length r) 1)

can5 _ = Nothing


-- Conexión de zonas IPv6 utilizando ⊙d ó ⊡d.
can6 :: ReglaP
can6 [/ (conexion,Z6), r@(OpD_td,Z6)+!, rs@_* /] = Just (
	(conexion,Z6) : rs,
	Cambio Canonizacion6 1 (1 + length r) 1)

can6 [/ (conexion,Z6), r@(OpD_td,Zd)+!, rs@_* /] = Just (
	(conexion,Z6) : rs,
	Cambio Canonizacion6 1 (1 + length r) 1)

can6 [/ (conexion,Zd), r@(OpD_td,Z6)+!, rs@_* /] = Just (
	(conexion,Z6) : rs,
	Cambio Canonizacion6 1 (1 + length r) 1)

can6 [/ (conexion,Z6), r@(OpD_tptd,Z6)+!, rs@_* /] = Just (
	(conexion,Z6) : rs,
	Cambio Canonizacion6 1 (1 + length r) 1)

can6 [/ (conexion,Z6), r@(OpD_tptd,Zd)+!, rs@_* /] = Just (
	(conexion,Z6) : rs,
	Cambio Canonizacion6 1 (1 + length r) 1)

can6 [/ (conexion,Zd), r@(OpD_tptd,Z6)+!, rs@_* /] = Just (
	(conexion,Z6) : rs,
	Cambio Canonizacion6 1 (1 + length r) 1)
	
can6 _ = Nothing


-- Conexión de zonas duales utilizando ⊠d.
can7 :: ReglaP
can7 [/ (conexion,Zd), r@(OpD,Zd)+! , rs@_* /] = Just (
	(conexion,Zd):rs,
	Cambio Canonizacion7 1 (1 + length r) 1)

can7 _ = Nothing


-- Conexión de una zona dual en una posición intermedia utilizando ⊠d.
can8 :: ReglaP
can8 [/ (conexion,zi) , routers@(OpD,Zd)+! , (OpD,zj) , rs@_* /] = if
	(subset [zi,zj] [Z4,Z6,Zd])
	then Just (
		(conexion,zi) : (OpD,zj) : rs,
		Cambio Canonizacion8 1 (2 + length routers) 2)
	else Nothing
can8 _ = Nothing



-------------------------------------------------------------------------------
-- Lista de todas las reglas de canonización.
-------------------------------------------------------------------------------

-- Lista de todas las reglas de canonización.
canonizaciones :: [ReglaP]
canonizaciones = [can1, can2, can3, can4, can5, can6, can7, can8]


-- Aplica todos los operadores de canonización al escenario.
canonizar :: Escenario -> (Escenario, [Cambio])
canonizar escenario = aplicarRS canonizaciones escenario
