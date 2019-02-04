{-# OPTIONS_GHC -F -pgmF trhsx -XPatternGuards #-}

-------------------------------------------------------------------------------
-- Autor: José Franco Campos
-- 
-- Generación, análisis y optimización de escenarios de migración
-- de redes Ipv4/IPv6 mediante programación funcional.
-- 
-- Diciembre 2008
--
--
-- Descripción: implementación de las reglas de creación de túneles.
-------------------------------------------------------------------------------

module Reglas.Tuneles (
	tuneles_zz,
	tuneles_zn,
	tuneles_nn
	) where


-- Módulos del programa
import Reglas.Base
import Reglas.Tipos
import Topologia



-------------------------------------------------------------------------------
-- Tuneles entre zonas intermedias.
-------------------------------------------------------------------------------

-- Túnel manual IPv4 dentro de IPv6.
zz_manual_4en6 :: ReglaP
zz_manual_4en6 [(Directa,Z4), (op1,Z6), (op2,Z4)]
	| subset [op1,op2] [OpD, OpD_tp] = Just (
		[(Directa,Z4)],
		Cambio ZZ_manual_4en6 1 3 1)
	| (elem op1 [OpD, OpD_tp]) && (elem op2 [OpD_td, OpD_tptd]) = Just (
		[(Directa,Z4), (Op4_td,Z4)],
		Cambio ZZ_manual_4en6 1 3 2)
	| subset [op1,op2] [OpD_td, OpD_tptd] = Just (
		[(Directa,Z4), (Op4_td,Z4), (Op4_td,Z4)],
		Cambio ZZ_manual_4en6 1 3 3)
	| otherwise = Nothing
zz_manual_4en6 _ = Nothing


-- Túnel manual IPv6 dentro de IPv4.
zz_manual_6en4 :: ReglaP
zz_manual_6en4 [(Directa,Z6), (op1,Z4), (op2,Z6)]
	| subset [op1,op2] [OpD, OpD_tp, OpD_td, OpD_tptd] = Just (
		[(Directa,Z6)],
		Cambio ZZ_manual_6en4 1 3 1)
	| otherwise = Nothing
zz_manual_6en4 _ = Nothing


-- Túnel manual IPv6 dentro de IPv4 UDP.
zz_manual_udp :: ReglaP
zz_manual_udp [/ (Directa,Z6), (op1,Z4), r@(Op4_td,Z4)*! , (op2,Z6) /]
	| subset [op1,op2] [OpD, OpD_tp, OpD_td, OpD_tptd] = Just (
		[(Directa,Z6)],
		Cambio ZZ_manual_udp 1 (3 + length r) 1)
	| otherwise = Nothing
zz_manual_udp _ = Nothing


-- Broker de túneles IPv4 dentro de IPv6.
zz_broker_4en6 :: ReglaP
zz_broker_4en6 [(Directa,Z4), (op1,Z6), (op2,Z4)]
	| subset [op1,op2] [OpD, OpD_tp] = Just (
		[(Directa,Z4)],
		Cambio ZZ_broker_4en6 1 3 1)
	| (elem op1 [OpD, OpD_tp]) && (elem op2 [OpD_td, OpD_tptd]) = Just (
		[(Directa,Z4), (Op4_td,Z4)],
		Cambio ZZ_broker_4en6 0 3 2)
	| subset [op1,op2] [OpD_td, OpD_tptd] = Just (
		[(Directa,Z4), (Op4_td,Z4), (Op4_td,Z4)],
		Cambio ZZ_broker_4en6 1 3 3)
	| otherwise = Nothing
zz_broker_4en6 _ = Nothing


-- Broker de túneles IPv6 dentro de IPv4.
zz_broker_6en4 :: ReglaP
zz_broker_6en4 [(Directa,Z6), (op1,Z4), (op2,Z6)]
	| subset [op1,op2] [OpD, OpD_tp, OpD_td, OpD_tptd] = Just (
		[(Directa,Z6)],
		Cambio ZZ_broker_6en4 1 3 1)
	| otherwise = Nothing
zz_broker_6en4 _ = Nothing


-- Broker de túneles IPv6 dentro de IPv4 UDP.
zz_broker_udp :: ReglaP
zz_broker_udp [/ (Directa,Z6), (op1,Z4), r@(Op4_td,Z4)*! , (op2,Z6) /]
	| subset [op1,op2] [OpD, OpD_tp, OpD_td, OpD_tptd] = Just (
		[(Directa,Z6)],
		Cambio ZZ_broker_udp 1 (3 + length r) 1)
	| otherwise = Nothing
zz_broker_udp _ = Nothing


-- Túnel 6to4.
zz_6to4 :: ReglaP
zz_6to4 [(Directa,Z6), (op1,Z4), (op2,Z6)]
	| subset [op1,op2] [OpD, OpD_tp, OpD_td, OpD_tptd] = Just (
		[(Directa,Z6)],
		Cambio ZZ_6to4 1 3 1)
	| otherwise = Nothing
zz_6to4 _ = Nothing



-------------------------------------------------------------------------------
-- Tuneles entre zonas y nodos terminales.
-------------------------------------------------------------------------------

-- Túnel manual IPv6 dentro de IPv4.
zn_manual_4en6 :: ReglaC
zn_manual_4en6 (a1, Nd, ((Directa,Z6):(op1,Z4):xs), n2, a2)
	| elem op1 [OpD, OpD_tp] = Just (
		(a1, Nd, ((Directa,Z4):xs), n2, a2),
		Cambio ZN_manual_4en6 0 3 2)
	| elem op1 [OpD_td, OpD_tptd] = Just (
		(a1, Nd, ((Directa,Z4):(Op4_td,Z4):xs), n2, a2),
		Cambio ZN_manual_4en6 0 2 1)
	| otherwise = Nothing
zn_manual_4en6 _ = Nothing


-- Túnel manual IPv6 dentro de IPv4.
zn_manual_6en4 :: ReglaC
zn_manual_6en4 (a1, Nd, ((Directa,Z4):(op1,Z6):xs), n2, a2)
	| elem op1 [OpD, OpD_tp, OpD_td, OpD_tptd] = Just (
		(a1, Nd, ((Directa,Z6):xs), n2, a2),
		Cambio ZN_manual_6en4 0 3 2)
	| otherwise = Nothing
zn_manual_6en4 _ = Nothing


-- Túnel manual IPv6 dentro de IPv4 UDP.
zn_manual_udp :: ReglaC
zn_manual_udp (a1, Nd, [/ (Directa,Z4), r@(Op4_td,Z4)*! , (op1,Z6) , xs@_* /], n2, a2)
	| elem op1 [OpD, OpD_tp, OpD_td, OpD_tptd] = Just (
		(a1, Nd, ((Directa,Z6):xs), n2, a2),
		Cambio ZN_manual_udp 0 (3 + length r) 2)
	| otherwise = Nothing
zn_manual_udp _ = Nothing


-- Broker de túneles IPv6 dentro de IPv4.
zn_broker_4en6 :: ReglaC
zn_broker_4en6 (a1, Nd, ((Directa,Z6):(op1,Z4):xs), n2, a2)
	| elem op1 [OpD, OpD_tp] = Just (
		(a1, Nd, ((Directa,Z4):xs), n2, a2),
		Cambio ZN_broker_4en6 0 3 2)
	| elem op1 [OpD_td, OpD_tptd] = Just (
		(a1, Nd, ((Directa,Z4):(Op4_td,Z4):xs), n2, a2),
		Cambio ZN_broker_4en6 0 2 1)
	| otherwise = Nothing
zn_broker_4en6 _ = Nothing


-- Broker de túneles IPv6 dentro de IPv4.
zn_broker_6en4 :: ReglaC
zn_broker_6en4 (a1, Nd, ((Directa,Z4):(op1,Z6):xs), n2, a2)
	| elem op1 [OpD, OpD_tp, OpD_td, OpD_tptd] = Just (
		(a1, Nd, ((Directa,Z6):xs), n2, a2),
		Cambio ZN_broker_6en4 0 3 2)
	| otherwise = Nothing
zn_broker_6en4 _ = Nothing


-- Broker de túneles IPv6 dentro de IPv4 UDP.
zn_broker_udp :: ReglaC
zn_broker_udp (a1, Nd, [/ (Directa,Z4), r@(Op4_td,Z4)*! , (op1,Z6) , xs@_* /], n2, a2)
	| elem op1 [OpD, OpD_tp, OpD_td, OpD_tptd] = Just (
		(a1, Nd, ((Directa,Z6):xs), n2, a2),
		Cambio ZN_broker_udp 0 (3 + length r) 2)
	| otherwise = Nothing
zn_broker_udp _ = Nothing


-- Túnel DSTM.
zn_dstm :: ReglaC
zn_dstm (a1, Nd, ((Directa,Z6):(op1,Z4):xs), n2, a2)
	| elem op1 [OpD, OpD_tp] = Just (
		(a1, Nd, ((Directa,Z4):xs), n2, a2),
		Cambio ZN_dstm 0 3 2)
	| elem op1 [OpD_td, OpD_tptd] = Just (
		(a1, Nd, ((Directa,Z4):(op1,Z4):xs), n2, a2),
		Cambio ZN_dstm 0 3 3)
	| otherwise = Nothing
zn_dstm _ = Nothing


-- Túnel Teredo.
zn_teredo :: ReglaC
zn_teredo (a1, Nd, [/ (Directa,Z4), r@(Op4_td,Z4)*! , (op1,Z6) , xs@_* /], n2, a2)
	| elem op1 [OpD, OpD_tp, OpD_td, OpD_tptd] = Just (
		(a1, Nd, ((Directa,Z6):xs), n2, a2),
		Cambio ZN_teredo 0 (3 + length r) 2)
	| otherwise = Nothing
zn_teredo _ = Nothing


-- Túnel ISATAP.
zn_isatap :: ReglaC
zn_isatap (a1, Nd, ((Directa,Z4):(op1,Z6):xs), n2, a2)
	| elem op1 [OpD, OpD_tp] = Just (
		(a1, Nd, ((Directa,Z6):xs), n2, a2),
		Cambio ZN_isatap 0 3 2)
	| otherwise = Nothing
zn_isatap _ = Nothing



-------------------------------------------------------------------------------
-- Tuneles entre nodos terminales.
-------------------------------------------------------------------------------

-- Túnel manual IPv4 dentro de IPv6.
nn_manual_4en6 :: ReglaC
nn_manual_4en6 (a1, Nd, [(Directa,Z6)], Nd, a2) = Just (
	(a1, Nd, [(Directa,Z4)], Nd, a2),
	Cambio NN_manual_4en6 0 3 3)
nn_manual_4en6 _ = Nothing


-- Túnel manual IPv6 dentro de IPv4.
nn_manual_6en4 :: ReglaC
nn_manual_6en4 (a1, Nd, [(Directa,Z4)], Nd, a2) = Just (
	(a1, Nd, [(Directa,Z6)], Nd, a2),
	Cambio NN_manual_6en4 0 3 3)
nn_manual_6en4 _ = Nothing


-- Túnel manual IPv6 dentro de IPv4 UDP.
nn_manual_udp :: ReglaC
nn_manual_udp (a1, Nd, [/ (Directa,Z4), r@(Op4_td,Z4)+! /], Nd, a2) = Just (
	(a1, Nd, [(Directa,Z6)], Nd, a2),
	Cambio NN_manual_udp 0 (3 + length r) 3)
nn_manual_udp _ = Nothing


-- Túnel ISATAP.
nn_isatap :: ReglaC
nn_isatap (a1, Nd, [(Directa,Z4)], Nd, a2) = Just (
	(a1, Nd, [(Directa,Z6)], Nd, a2),
	Cambio NN_isatap 0 3 3)
nn_isatap _ = Nothing



-------------------------------------------------------------------------------
-- Lista con todas las reglas.
-------------------------------------------------------------------------------

-- Reglas entre zonas intermedias.
tuneles_zz :: [ReglaP]
tuneles_zz = [
	zz_manual_6en4,
	zz_manual_4en6,
	zz_manual_udp,
	zz_broker_6en4,
	zz_broker_4en6,
	zz_broker_udp,
	zz_6to4]


-- Reglas entre zonas intermedias y nodos terminales.
tuneles_zn :: [ReglaC]
tuneles_zn = [
	zn_manual_4en6,
	zn_manual_6en4,
	zn_manual_udp,
	zn_broker_4en6,
	zn_broker_6en4,
	zn_broker_udp,
	zn_dstm,
	zn_teredo,
	zn_isatap]


-- Reglas entre nodos terminales.
tuneles_nn :: [ReglaC]
tuneles_nn = [
	nn_manual_6en4,
	nn_manual_4en6,
	nn_manual_udp,
	nn_isatap]
