-------------------------------------------------------------------------------
-- Autor: José Franco Campos
-- 
-- Generación, análisis y optimización de escenarios de migración
-- de redes Ipv4/IPv6 mediante programación funcional.
-- 
-- Diciembre 2008
--
--
-- Descripción: implementación de las reglas para redes finales.
-------------------------------------------------------------------------------

module Reglas.Mecanismos (
	solucion,
	mecanismos
	) where

	
-- Módulos del programa
import Reglas.Base
import Reglas.Tipos
import Topologia



-------------------------------------------------------------------------------
-- Escenarios solución.
--
-- La función "solucion" devuelve True si el escenario es solución (es decir,
-- hay conectividad de manera directa), y False si es necesario aplicar algún
-- mecanismo de transición adicional.
-------------------------------------------------------------------------------

solucion :: Escenario -> Bool

-- Zonas IPv4 con aplicaciones IPv4.
solucion (A4, N4, [(_,Z4)], N4, A4) = True
solucion (A4, N4, [(_,Z4)], Nd, A4) = True
solucion (A4, Nd, [(_,Z4)], N4, A4) = True
solucion (A4, Nd, [(_,Z4)], Nd, A4) = True


-- Zonas IPv4 con aplicaciones IPv6.
solucion (A6, Nmap, [(_,Z4)], Nmap, A6) = True


-- Zonas IPv4 con aplicaciones heterogéneas.
solucion (A4, N4, [(_,Z4)], Nmap, A6) = True
solucion (A4, Nd, [(_,Z4)], Nmap, A6) = True
solucion (A6, Nmap, [(_,Z4)], N4, A4) = True
solucion (A6, Nmap, [(_,Z4)], Nd, A4) = True


-- Zonas IPv6 con aplicaciones IPv4.
solucion (A4, Nd, [(_,Z6)], Nd, A4) = True


-- Zonas IPv6 con aplicaciones IPv6.
solucion (A6, N6, [(_,Z6)], N6, A6) = True
solucion (A6, N6, [(_,Z6)], Nd, A6) = True
solucion (A6, Nd, [(_,Z6)], N6, A6) = True
solucion (A6, Nd, [(_,Z6)], Nd, A6) = True


-- Zonas IPv6 con aplicaciones heterogéneas (transformar A4 en Ad).
solucion (A6, Nd, [(_,Z6)], Nd, A4) = True
solucion (A4, Nd, [(_,Z6)], Nd, A6) = True


-- Zonas IPv4 con traducción de direcciones. Si una de las dos aplicaciones
-- es IPv4, la red se trata como si tuviera un único elemento.
solucion (A4, n1, [(_,Z4),(Op4_td,Z4)], n2, a2) =
	solucion (A4, n1, [(Directa,Z4)], n2, a2)
solucion (a1, n1, [(_,Z4),(Op4_td,Z4)], n2, A4) =
	solucion (a1, n1, [(Directa,Z4)], n2, A4)
solucion (A6, Nmap, [(_,Z4),(Op4_td,Z4)], Nmap, A6) = True


-- Composición de dos zonas con aplicaciones IPv6.
solucion (A6, Nmap, [(_,Z4),(op,Z6)], N6, A6) = elem op [OpD_tp, OpD_tptd]
solucion (A6, Nmap, [(_,Z4),(op,Z6)], Nd, A6) = elem op [OpD_tp, OpD_tptd]


-- Composición de dos zonas con aplicaciones heterogéneas.
solucion (A4, N4, [(_,Z4),(op,Z6)], N6, A6) = elem op [OpD_tp, OpD_tptd]
solucion (A4, N4, [(_,Z4),(op,Z6)], Nd, A6) = elem op [OpD_tp, OpD_tptd]
solucion (A4, Nd, [(_,Z4),(op,Z6)], N6, A6) = elem op [OpD_tp, OpD_tptd]
solucion (A4, Nd, [(_,Z4),(op,Z6)], Nd, A6) = elem op [OpD_tp, OpD_tptd]
solucion (A6, N6, [(_,Z6),(op,Z4)], N4, A4) = elem op [OpD_tp, OpD_tptd]
solucion (A6, N6, [(_,Z6),(op,Z4)], Nd, A4) = elem op [OpD_tp, OpD_tptd]
solucion (A6, Nd, [(_,Z6),(op,Z4)], N4, A4) = elem op [OpD_tp, OpD_tptd]
solucion (A6, Nd, [(_,Z6),(op,Z4)], Nd, A4) = elem op [OpD_tp, OpD_tptd]


-- Las aplicaciones duales pueden comportarse como A4 o A6, según interese.
solucion (Ad, n1, red, n2, a2) = s4 || s6
	where
	s4 = solucion (A4, n1, red, n2, a2)
	s6 = solucion (A6, n1, red, n2, a2)
solucion (a1, n1, red, n2, Ad) = s4 || s6
	where
	s4 = solucion (a1, n1, red, n2, Ad)
	s6 = solucion (a1, n1, red, n2, Ad)
	
-- El resto de escenarios no son solución.
solucion _ = False



-------------------------------------------------------------------------------
-- Redes básicas IPv4 con aplicaciones IPv4.
-------------------------------------------------------------------------------

-- Caso 1.
base_Z4_A4 :: ReglaC

base_Z4_A4 (A4, N6, red@[(_,Z4)], n2, A4) = Just (
	(A4, Nd, red, n2, A4),
	Cambio Dual_stack 0 1 1)

base_Z4_A4 (A4, n1, red@[(_,Z4)], N6, A4) = Just (
	(A4, n1, red, Nd, A4),
	Cambio Dual_stack 2 1 1)

base_Z4_A4 _ = Nothing



-------------------------------------------------------------------------------
-- Redes básicas IPv4 con aplicaciones IPv6.
-------------------------------------------------------------------------------

-- Caso 1.
base_Z4_A6_1 :: ReglaC
base_Z4_A6_1 (A6, n1, red@[(_,Z4)], n2, A6) = if
	(elem n1 [N4, N6]) &&
	(elem n2 [N4, N6, Nd])
	then Just (
		(A6, Nd, red, n2, A6),
		Cambio Dual_stack 0 1 1)
	else Nothing
base_Z4_A6_1 _ = Nothing


-- Caso 2.
base_Z4_A6_2 :: ReglaC
base_Z4_A6_2 (A6, Nd, red@[(_,Z4)], Nd, A6) = Just (
	(A6, Nmap, red, Nmap, A6),
	Cambio Mapped 0 3 3)
base_Z4_A6_2 _ = Nothing


-- Caso 3.
base_Z4_A6_3 :: ReglaC
base_Z4_A6_3 (A6, Nd, [(_,Z4)], Nd, A6) = Just (
	(A6, Nd, [(Directa,Z6)], Nd, A6),
	Cambio NN_manual_6en4 0 3 3)
base_Z4_A6_3 _ = Nothing



-------------------------------------------------------------------------------
-- Redes básicas IPv4 con aplicaciones heterogéneas.
-------------------------------------------------------------------------------

-- Caso 1.
base_Z4_Ah_1 :: ReglaC
base_Z4_Ah_1 (A4, N6, red@[(_,Z4)], n2, A6) = if
	elem n2 [N4, N6, Nd]
	then Just (
		(A4, Nd, red, n2, A6),
		Cambio Dual_stack 0 1 1)
	else Nothing
base_Z4_Ah_1 _ = Nothing


-- Caso 2.
base_Z4_Ah_2 :: ReglaC
base_Z4_Ah_2 (A4, n1, red@[(_,Z4)], n2, A6) = if
	(elem n1 [N4, N6, Nd]) &&
	(elem n2 [N4, N6])
	then Just (
		(A4, n1, red, Nd, A6),
		Cambio Dual_stack 2 1 1)
	else Nothing
base_Z4_Ah_2 _ = Nothing


-- Caso 3.
base_Z4_Ah_3 :: ReglaC
base_Z4_Ah_3 (A4, n1, red@[(_,Z4)], Nd, A6) = if
	elem n1 [N4, Nd]
	then Just (
		(A4, n1, red, Nmap, A6),
		Cambio Mapped 2 1 1)
	else Nothing
base_Z4_Ah_3 _ = Nothing



-------------------------------------------------------------------------------
-- Redes básicas IPv6 con aplicaciones IPv4.
-------------------------------------------------------------------------------

-- Caso 1.
base_Z6_A4_1 :: ReglaC
base_Z6_A4_1 (A4, n1, red@[(_,Z6)], n2, A4) = if
	(elem n1 [N4, N6]) &&
	(elem n2 [N4, N6, Nd])
	then Just (
		(A4, Nd, red, n2, A4),
		Cambio Dual_stack 0 1 1)
	else Nothing
base_Z6_A4_1 _ = Nothing


-- Caso 2.
base_Z6_A4_2 :: ReglaC
base_Z6_A4_2 (A4, Nd, [(_,Z6)], Nd, A4) = Just (
	(A4, Nd, [(Directa,Z4)], Nd, A4),
	Cambio NN_manual_4en6 0 3 3)
base_Z6_A4_2 _ = Nothing



-------------------------------------------------------------------------------
-- Redes básicas IPv6 con aplicaciones IPv6.
-------------------------------------------------------------------------------

-- Caso 1.
base_Z6_A6 :: ReglaC
base_Z6_A6 (A6, N4, red@[(_,Z6)], n2, A6) = if
	elem n2 [N4, N6, Nd]
	then Just (
		(A6, Nd, red, n2, A6),
		Cambio Dual_stack 0 1 1)
	else Nothing
base_Z6_A6 _ = Nothing



-------------------------------------------------------------------------------
-- Redes básicas IPv6 con aplicaciones heterogéneas.
-------------------------------------------------------------------------------

-- Caso 1.
base_Z6_Ah_1 :: ReglaC
base_Z6_Ah_1 (A6, N4, red@[(_,Z6)], n2, A4) = if
	elem n2 [N4, N6, Nd]
	then Just (
		(A6, Nd, red, n2, A4),
		Cambio Dual_stack 0 1 1)
	else Nothing
base_Z6_Ah_1 _ = Nothing


-- Caso 2.
base_Z6_Ah_2 :: ReglaC
base_Z6_Ah_2 (A6, n1, red@[(_,Z6)], n2, A4) = if
	(elem n1 [N4, N6, Nd]) &&
	(elem n2 [N4, N6])
	then Just (
		(A6, n1, red, Nd, A4),
		Cambio Dual_stack 2 1 1)
	else Nothing
base_Z6_Ah_2 _ = Nothing


-- Caso 3.
base_Z6_Ah_3 :: ReglaC
base_Z6_Ah_3 (A6, Nd, [(_,Z6)], Nd, A4) = Just (
	(A6, Nd, [(Directa,Z4)], Nd, A4),
	Cambio NN_manual_4en6 0 3 3)
base_Z6_Ah_3 _ = Nothing



-------------------------------------------------------------------------------
-- Redes básicas duales con aplicaciones IPv4.
-------------------------------------------------------------------------------

-- Caso 1.
base_Zd_A4 :: ReglaC
base_Zd_A4 (A4, n1, [(_,Zd)], n2, A4) = Just (
	(A4, n1, [(Directa,Z4)], n2, A4),
	Cambio Nada 1 1 1)
base_Zd_A4 _ = Nothing



-------------------------------------------------------------------------------
-- Redes básicas duales con aplicaciones IPv6.
-------------------------------------------------------------------------------

-- Caso 1.
base_Zd_A6 :: ReglaC
base_Zd_A6 (A6, n1, [(_,Zd)], n2, A6) = Just (
	(A6, n1, [(Directa,Z6)], n2, A6),
	Cambio Nada 1 1 1)
base_Zd_A6 _ = Nothing



-------------------------------------------------------------------------------
-- Redes básicas duales con aplicaciones heterogéneas.
-------------------------------------------------------------------------------

-- Caso 1.
base_Zd_Ah_1 :: ReglaC
base_Zd_Ah_1 (A4, n1, [(_,Zd)], n2, A6) = Just (
	(A4, n1, [(Directa,Z4)], n2, A6),
	Cambio Nada 1 1 1)
base_Zd_Ah_1 _ = Nothing


-- Caso 2.
base_Zd_Ah_2 :: ReglaC
base_Zd_Ah_2 (A4, n1, [(_,Zd)], n2, A6) = Just (
	(A4, n1, [(Directa,Z6)], n2, A6),
	Cambio Nada 1 1 1)
base_Zd_Ah_2 _ = Nothing



-------------------------------------------------------------------------------
-- Redes básicas IPv4 con traducción de direcciones.
-------------------------------------------------------------------------------

-- Caso 1.
base_NAT_1 :: ReglaC
base_NAT_1 (A6, n1, [(_,Z4), (Op4_td,Z4)], n2, A6) = if
	(elem n1 [N4, N6]) &&
	(elem n2 [N4, N6, Nd])
	then Just (
		(A6, Nd, [(Directa,Z4), (Op4_td,Z4)], n2, A6),
		Cambio Dual_stack 0 1 1)
	else Nothing 
base_NAT_1 _ = Nothing


-- Caso 2.
base_NAT_2 :: ReglaC
base_NAT_2 (A6, Nd, [(_,Z4), (Op4_td,Z4)], Nd, A6) = Just (
	(A6, Nmap, [(Directa,Z4)], Nmap, A6),
	Cambio Mapped 0 4 3)
base_NAT_2 _ = Nothing


-- Caso 3.
base_NAT_3 :: ReglaC
base_NAT_3 (A6, Nd, [(_,Z4), (Op4_td,Z4)], Nd, A6) = Just (
	(A6, Nd, [(Directa,Z6)], Nd, A6),
	Cambio NN_teredo 0 3 3)
base_NAT_3 _ = Nothing



-------------------------------------------------------------------------------
-- Composición de dos zonas del mismo tipo.
-------------------------------------------------------------------------------

-- Caso 1.
dos_iguales :: ReglaC
dos_iguales (a1, n1, [(_,Z4),(OpD,     Z4)], n2, a2) = Just (
	(a1, n1, [(Directa,Z4)], n2, a2),
	Cambio Nada 1 2 1)
	
dos_iguales (a1, n1, [(_,Z4),(OpD_td,  Z4)], n2, a2) = Just (
	(a1, n1, [(Directa,Z4), (Op4_td,Z4)], n2, a2),
	Cambio Nada 1 2 2)

dos_iguales (a1, n1, [(_,Z4),(OpD_tp,  Z4)], n2, a2) = Just (
	(a1, n1, [(Directa,Z4)], n2, a2),
	Cambio Nada 1 2 1)

dos_iguales (a1, n1, [(_,Z4),(OpD_tptd,Z4)], n2, a2) = Just (
	(a1, n1, [(Directa,Z4), (Op4_td,Z4)], n2, a2),
	Cambio Nada 1 2 2)

dos_iguales (a1, n1, [(_,Z6),(OpD,     Z6)], n2, a2) = Just (
	(a1, n1, [(Directa,Z6)], n2, a2),
	Cambio Nada 1 2 1)

dos_iguales (a1, n1, [(_,Z6),(OpD_td,  Z6)], n2, a2) = Just (
	(a1, n1, [(Directa,Z6)], n2, a2),
	Cambio Nada 1 2 1)

dos_iguales (a1, n1, [(_,Z6),(OpD_tp,  Z6)], n2, a2) = Just (
	(a1, n1, [(Directa,Z6)], n2, a2),
	Cambio Nada 1 2 1)

dos_iguales (a1, n1, [(_,Z6),(OpD_tptd,Z6)], n2, a2) = Just (
	(a1, n1, [(Directa,Z6)], n2, a2),
	Cambio Nada 1 2 1)

dos_iguales _ = Nothing



-------------------------------------------------------------------------------
-- Composición de dos zonas diferentes con aplicaciones IPv4.
-------------------------------------------------------------------------------

-- Caso 1
dos_A4_1 :: ReglaC
dos_A4_1 (A4, n1, red@[(_,Z4),(op,Z6)], n2, A4) = if
	(elem n1 [N4, N6, Nd]) &&
	(elem n2 [N4, N6]) &&
	(elem op [OpD, OpD_td, OpD_tp, OpD_tptd])
	then Just (
		(A4, n1, red, Nd, A4),
		Cambio Dual_stack 3 1 1)
	else Nothing
dos_A4_1 _ = Nothing


-- Caso 2.
dos_A4_2 :: ReglaC
dos_A4_2 (A4, N6, red@[(_,Z4),(op,Z6)], Nd, A4) = if
	elem op [OpD, OpD_td, OpD_tp, OpD_tptd]
	then Just (
		(A4, Nd, red, Nd, A4),
		Cambio Dual_stack 0 1 1)
	else Nothing
dos_A4_2 _ = Nothing


-- Caso 3.
dos_A4_3 :: ReglaC
dos_A4_3 (A4, n1, [(_,Z4),(op,Z6)], Nd, A4) = if
	(elem n1 [N4, Nd]) &&
	(elem op [OpD, OpD_tp])
	then Just (
		(A4, n1, [(Directa,Z4)], Nd, A4),
		Cambio ZN_manual_4en6 1 3 2) 
	else Nothing
dos_A4_3 _ = Nothing


-- Caso 4.
dos_A4_4 :: ReglaC
dos_A4_4 (A4, n1, [(_,Z4),(op,Z6)], Nd, A4) = if
	(elem n1 [N4, Nd]) &&
	(elem op [OpD_td, OpD_tptd])
	then Just (
		(A4, n1, [(Directa,Z4),(Op4_td,Z4)], Nd, A4),
		Cambio ZN_manual_4en6 1 3 3)
	else Nothing
dos_A4_4 _ = Nothing



-------------------------------------------------------------------------------
-- Composición de dos zonas diferentes con aplicaciones IPv6.
-------------------------------------------------------------------------------

-- Caso 1.
dos_A6_1 :: ReglaC
dos_A6_1 (A6, n1, red@[(_,Z4),(op,Z6)], n2, A6) = if
	(elem n1 [N4, N6]) &&
	(elem n2 [N4, N6, Nd]) &&
	(elem op [OpD, OpD_td, OpD_tp, OpD_tptd])
	then Just (
		(A6, Nd, red, n2, A6),
		Cambio Dual_stack 3 1 1)
	else Nothing
dos_A6_1 _ = Nothing


-- Caso 2.
dos_A6_2 :: ReglaC
dos_A6_2 (A6, n1, red@[(_,Z4),(op,Z6)], N4, A6) = if
	(elem n1 [N4, N6, Nd]) &&
	(elem op [OpD, OpD_td, OpD_tp, OpD_tptd])
	then Just (
		(A6, n1, red, Nd, A6),
		Cambio Dual_stack 3 1 1)
	else Nothing
dos_A6_2 _ = Nothing


-- Caso 3.
dos_A6_3 :: ReglaC
dos_A6_3 (A6, Nd, [(_,Z4),(op,Z6)], n2, A6) = if
	(elem n2 [N6, Nd]) &&
	(elem op [OpD, OpD_td, OpD_tp, OpD_tptd])
	then Just (
		(A6, Nd, [(Directa,Z6)], n2, A6),
		Cambio ZN_manual_6en4 0 3 2) 
	else Nothing
dos_A6_3 _ = Nothing


-- Caso 4.
dos_A6_4 :: ReglaC
dos_A6_4 (A6, Nd, [(_,Z4),(op,Z6)], n2, A6) = if
	(elem n2 [N6, Nd]) &&
	(elem op [OpD, OpD_td])
	then Just (
		(A6, Nmap, [(Directa,Z4),(op',Z6)], n2, A6),
		Cambio Mapped 0 1 1)
	else Nothing
	where
		op' = if op == OpD then OpD_tp else OpD_tptd
dos_A6_4 _ = Nothing



-------------------------------------------------------------------------------
-- Composición de dos zonas diferentes con aplicaciones heterogéneas.
-------------------------------------------------------------------------------

-- Caso 1.
dos_Ah_1 :: ReglaC
dos_Ah_1 (A4, N6, red@[(_,Z4),(op,Z6)], n2, A6) = if
	(elem op [OpD, OpD_td, OpD_tp, OpD_tptd]) &&
	(elem n2 [N4, N6, Nd])
	then Just (
		(A4, Nd, red, n2, A6),
		Cambio Dual_stack 0 1 1)
	else Nothing
dos_Ah_1 _ = Nothing


-- Caso 2.
dos_Ah_2 :: ReglaC
dos_Ah_2 (A4, n1, red@[(_,Z4),(op,Z6)], N4, A6) = if
	(elem op [OpD, OpD_td, OpD_tp, OpD_tptd]) &&
	(elem n1 [N4, Nd])
	then Just (
		(A4, n1, red, Nd, A6),
		Cambio Dual_stack 3 1 1)
	else Nothing
dos_Ah_2 _ = Nothing


-- Caso 3.
dos_Ah_3 :: ReglaC
dos_Ah_3 (A4, n1, red@[(_,Z4),(op,Z6)], N6, A6) = if
	(elem op [OpD, OpD_td]) &&
	(elem n1 [N4, N6, Nd])
	then Just (
		(A4, n1, red, Nd, A6),
		Cambio Dual_stack 3 1 1)
	else Nothing
dos_Ah_3 _ = Nothing


-- Caso 4.
dos_Ah_4 :: ReglaC
dos_Ah_4 (A4, n1, [(_,Z4),(op,Z6)], n2, A6) = if
	(elem op [OpD, OpD_td]) &&
	(elem n2 [N6, Nd]) &&
	(elem n1 [N4, Nd])
	then Just (
		(A4, n1, [(Directa,Z4),(op',Z6)], n2, A6),
		Cambio Traduccion 1 2 2)
	else Nothing
	where
		op' = if op == OpD then OpD_tp else OpD_tptd
dos_Ah_4 _ = Nothing


-- Caso 5.
dos_Ah_5 :: ReglaC
dos_Ah_5 (A4, n1, [(_,Z4),(op,Z6)], Nd, A6) = if
	(elem op [OpD, OpD_tp]) &&
	(elem n1 [N6, Nd])
	then Just (
		(A4, n1, [(Directa,Z4)], Nd, A6),
		Cambio ZN_manual_4en6 1 3 2)
	else Nothing
dos_Ah_5 _ = Nothing


-- Caso 6.
dos_Ah_6 :: ReglaC
dos_Ah_6 (A4, n1, [(_,Z4),(op,Z6)], Nd, A6) = if
	(elem op [OpD_td, OpD_tptd]) &&
	(elem n1 [N6, Nd])
	then Just (
		(A4, n1, [(Directa,Z4),(Op4_td,Z4)], Nd, A6),
		Cambio ZN_manual_4en6 1 3 3)
	else Nothing
dos_Ah_6 _ = Nothing


-- Caso 7.
dos_Ah_7 :: ReglaC
dos_Ah_7 (A4, n1, red@[(_,Z6),(op,Z4)], n2, A6) = if
	(elem op [OpD, OpD_td, OpD_tp, OpD_tptd]) &&
	(elem n1 [N4, N6]) &&
	(elem n2 [N4, N6, Nd])
	then Just (
		(A4, Nd, red, n2, A6),
		Cambio Dual_stack 0 1 1)
	else Nothing
dos_Ah_7 _ = Nothing


-- Caso 8.
dos_Ah_8 :: ReglaC
dos_Ah_8 (A4, Nd, red@[(_,Z6),(op,Z4)], n2, A6) = if
	(elem op [OpD, OpD_td, OpD_tp, OpD_tptd]) &&
	(elem n2 [N4, N6])
	then Just (
		(A4, Nd, red, Nd, A6),
		Cambio Dual_stack 3 1 1)
	else Nothing
dos_Ah_8 _ = Nothing


-- Caso 9.
dos_Ah_9 :: ReglaC
dos_Ah_9 (A4, Nd, [(_,Z6),(op,Z4)], Nd, A6) = if
	(elem op [OpD, OpD_tp])
	then Just (
		(A4, Nd, [(Directa,Z4)], Nd, A6),
		Cambio ZN_manual_4en6 0 3 2)
	else Nothing
dos_Ah_9 _ = Nothing


-- Caso 10.
dos_Ah_10 :: ReglaC
dos_Ah_10 (A4, Nd, [(_,Z6),(op,Z4)], Nd, A6) = if
	(elem op [OpD_td, OpD_tptd])
	then Just (
		(A4, Nd, [(Directa,Z4),(Op4_td,Z4)], Nd, A6),
		Cambio ZN_manual_4en6 0 3 3)
	else Nothing
dos_Ah_10 _ = Nothing



-------------------------------------------------------------------------------
-- Composición de tres zonas v4-v6-v4 con aplicaciones IPv4.
-------------------------------------------------------------------------------

-- Caso 1.
tres_464_A4_1 :: ReglaC
tres_464_A4_1 (A4, n1, [(_,Z4),(op1,Z6),(op2,Z4)], n2, A4) = if
	(subset [n1, n2] [N4, N6, Nd]) &&
	(subset [op1, op2] [OpD, OpD_td, OpD_tp, OpD_tptd])
	then Just (
		(A4, n1, [(Directa,Z4)], n2, A4),
		Cambio ZZ_manual_4en6 1 3 1)
	else Nothing
tres_464_A4_1 _ = Nothing

-- Caso 2.
tres_464_A4_2 :: ReglaC
tres_464_A4_2 (A4, n1, [(_,Z4),(op1,Z6),(op2,Z4)], Nd, A4) = if
	(elem n1 [N4, N6, Nd]) &&
	(subset [op1, op2] [OpD, OpD_td, OpD_tp, OpD_tptd])
	then Just (
		(A4, n1, [(Directa,Z4),(op1,Z6)], Nd, A4),
		Cambio ZZ_manual_6en4 2 3 2)
	else Nothing
tres_464_A4_2 _ = Nothing



-------------------------------------------------------------------------------
-- Composición de tres zonas v4-v6-v4 con aplicaciones IPv6.
-------------------------------------------------------------------------------

-- Caso 1.
tres_464_A6_1 :: ReglaC
tres_464_A6_1 (A6, n1, [(_,Z4),(op1,Z6),(op2,Z4)], n2, A6) = if
	(subset [n1, n2] [N4, N6, Nd]) &&
	(subset [op1, op2] [OpD, OpD_td, OpD_tp, OpD_tptd]) 
	then Just (
		(A6, n1, [(Directa,Z4)], n2, A6),
		Cambio ZZ_manual_4en6 1 3 1)
	else Nothing
tres_464_A6_1 _ = Nothing


-- Caso 2.
tres_464_A6_2 :: ReglaC
tres_464_A6_2 (A6, n1, [(_,Z4),(op1,Z6),(op2,Z4)], Nd, A6) = if
	(elem n1 [N4, N6, Nd]) &&
	(subset [op1, op2] [OpD, OpD_td, OpD_tp, OpD_tptd])
	then Just (
		(A6, n1, [(Directa,Z4),(op1,Z6)], Nd, A6),
		Cambio ZZ_manual_6en4 2 3 2)
	else Nothing
tres_464_A6_2 _ = Nothing



-------------------------------------------------------------------------------
-- Composición de tres zonas v4-v6-v4 con aplicaciones heterogéneas.
-------------------------------------------------------------------------------

-- Caso 1.
tres_464_Ah_1 :: ReglaC
tres_464_Ah_1 (A4, n1, [(_,Z4),(op1,Z6),(op2,Z4)], n2, A6) = if
	(subset [n1, n2] [N4, N6, Nd]) &&
	(subset [op1, op2] [OpD, OpD_td, OpD_tp, OpD_tptd])
	then Just (
		(A4, n1, [(Directa,Z4)], n2, A6),
		Cambio ZZ_manual_4en6 1 3 1)
	else Nothing
tres_464_Ah_1 _ = Nothing


-- Caso 2.
tres_464_Ah_2 :: ReglaC
tres_464_Ah_2 (A4, n1, [(_,Z4),(op1,Z6),(op2,Z4)], Nd, A6) = if
	(elem n1 [N4, N6, Nd]) &&
	(subset [op1, op2] [OpD, OpD_td, OpD_tp, OpD_tptd])
	then Just (
		(A4, n1, [(Directa,Z4),(op1,Z6)], Nd, A6),
		Cambio ZZ_manual_6en4 2 3 2)
	else Nothing
tres_464_Ah_2 _ = Nothing


-- Caso 3.
tres_464_Ah_3 :: ReglaC
tres_464_Ah_3 (A4, Nd, [(_,Z4),(op1,Z6),(op2,Z4)], n2, A6) = if
	(elem n2 [N4, N6, Nd]) &&
	(subset [op1, op2] [OpD, OpD_td, OpD_tp, OpD_tptd])
	then Just (
		(A4, Nd, [(Directa,Z6),(op2,Z4)], n2, A6),
		Cambio ZZ_manual_6en4 0 3 2)
	else Nothing
tres_464_Ah_3 _ = Nothing



-------------------------------------------------------------------------------
-- Composición de tres zonas v6-v4-v6 con aplicaciones IPv4.
-------------------------------------------------------------------------------

-- Caso 1.
tres_646_A4_1 :: ReglaC
tres_646_A4_1 (A4, n1, [(_,Z6),(op1,Z4),(op2,Z6)], n2, A4) = if
	(subset [n1, n2] [N4, N6, Nd]) &&
	(subset [op1, op2] [OpD, OpD_td, OpD_tp, OpD_tptd])
	then Just (
		(A4, n1, [(Directa,Z6)], n2, A4),
		Cambio ZZ_manual_6en4 1 3 1)
	else Nothing
tres_646_A4_1 _ = Nothing


-- Caso 2.
tres_646_A4_2 :: ReglaC
tres_646_A4_2 (A4, n1, [(_,Z6),(op1,Z4),(op2,Z6)], Nd, A4) = if
	(elem n1 [N4, N6, Nd]) &&
	(elem op1 [OpD, OpD_td, OpD_tp, OpD_tptd]) &&
	(elem op2 [OpD, OpD_tp])
	then Just (
		(A4, n1, [(Directa,Z6),(op1,Z4)], Nd, A4),
		Cambio ZZ_manual_4en6 2 3 2)
	else Nothing
tres_646_A4_2 _ = Nothing


-- Caso 3.
tres_646_A4_3 :: ReglaC
tres_646_A4_3 (A4, n1, [(_,Z6),(op1,Z4),(op2,Z6)], Nd, A4) = if
	(elem n1 [N4, N6, Nd]) &&
	(elem op1 [OpD, OpD_td, OpD_tp, OpD_tptd]) &&
	(elem op2 [OpD_td, OpD_tptd])
	then Just (
		(A4, n1, [(Directa,Z6),(op1,Z4),(Op4_td,Z4)], Nd, A4),
		Cambio ZZ_manual_4en6 2 3 3)
	else Nothing
tres_646_A4_3 _ = Nothing



-------------------------------------------------------------------------------
-- Composición de tres zonas v6-v4-v6 con aplicaciones IPv6.
-------------------------------------------------------------------------------

-- Caso 1.
tres_646_A6_1 :: ReglaC
tres_646_A6_1 (A6, n1, [(_,Z6),(op1,Z4),(op2,Z6)], n2, A6) = if
	(subset [n1, n2] [N4, N6, Nd]) &&
	(subset [op1, op2] [OpD, OpD_td, OpD_tp, OpD_tptd])
	then Just (
		(A6, n1, [(Directa,Z6)], n2, A6),
		Cambio ZZ_manual_6en4 1 3 1)
	else Nothing
tres_646_A6_1 _ = Nothing


-- Caso 2.
tres_646_A6_2 :: ReglaC
tres_646_A6_2 (A6, n1, [(_,Z6),(op1,Z4),(op2,Z6)], Nd, A6) = if
	(elem n1 [N4, N6, Nd]) &&
	(elem op1 [OpD, OpD_td, OpD_tp, OpD_tptd]) &&
	(elem op2 [OpD, OpD_tp])
	then Just (
		(A6, n1, [(Directa,Z6),(op1,Z4)], Nd, A6),
		Cambio ZZ_manual_4en6 2 3 2)
	else Nothing
tres_646_A6_2 _ = Nothing


-- Caso 3.
tres_646_A6_3 :: ReglaC
tres_646_A6_3 (A6, n1, [(_,Z6),(op1,Z4),(op2,Z6)], Nd, A6) = if
	(elem n1 [N4, N6, Nd]) &&
	(elem op1 [OpD, OpD_td, OpD_tp, OpD_tptd]) &&
	(elem op2 [OpD_td, OpD_tptd])
	then Just (
		(A6, n1, [(Directa,Z6),(op1,Z4),(Op4_td,Z4)], Nd, A6),
		Cambio ZZ_manual_4en6 2 3 3)
	else Nothing
tres_646_A6_3 _ = Nothing



-------------------------------------------------------------------------------
-- Composición de tres zonas v6-v4-v6 con aplicaciones v4-v6-v4.
-------------------------------------------------------------------------------

-- Caso 1.
tres_646_Ah_1 :: ReglaC
tres_646_Ah_1 (A4, n1, [(_,Z6),(op1,Z4),(op2,Z6)], n2, A6) = if
	(subset [n1, n2] [N4, N6, Nd]) &&
	(subset [op1, op2] [OpD, OpD_td, OpD_tp, OpD_tptd])
	then Just (
		(A4, n1, [(Directa,Z6)], n2, A6),
		Cambio ZZ_manual_6en4 1 3 1)
	else Nothing
tres_646_Ah_1 _ = Nothing


-- Caso 2.
tres_646_Ah_2 :: ReglaC
tres_646_Ah_2 (A4, n1, [(_,Z6),(op1,Z4),(op2,Z6)], Nd, A6) = if
	(elem n1 [N4, N6, Nd]) &&
	(elem op1 [OpD, OpD_td, OpD_tp, OpD_tptd]) &&
	(elem op2 [OpD, OpD_tp])
	then Just (
		(A4, n1, [(Directa,Z6),(op1,Z4)], Nd, A6),
		Cambio ZZ_manual_4en6 2 3 2)
	else Nothing
tres_646_Ah_2 _ = Nothing


-- Caso 3.
tres_646_Ah_3 :: ReglaC
tres_646_Ah_3 (A4, n1, [(_,Z6),(op1,Z4),(op2,Z6)], Nd, A6) = if
	(elem n1 [N4, N6, Nd]) &&
	(elem op1 [OpD, OpD_td, OpD_tp, OpD_tptd]) &&
	(elem op2 [OpD_td, OpD_tptd])
	then Just (
		(A4, n1, [(Directa,Z6),(op1,Z4),(Op4_td,Z4)], Nd, A6),
		Cambio ZZ_manual_4en6 2 3 3)
	else Nothing
tres_646_Ah_3 _ = Nothing


-- Caso 4.
tres_646_Ah_4 :: ReglaC
tres_646_Ah_4 (A4, Nd, [(_,Z6),(op1,Z4),(op2,Z6)], n2, A6) = if
	(elem n2 [N4, N6, Nd]) &&
	(elem op1 [OpD, OpD_tp]) &&
	(elem op2 [OpD, OpD_td, OpD_tp, OpD_tptd])
	then Just (
		(A4, Nd, [(Directa,Z4),(op2,Z6)], n2, A6),
		Cambio ZZ_manual_4en6 0 3 2)
	else Nothing
tres_646_Ah_4 _ = Nothing


-- Caso 5.
tres_646_Ah_5 :: ReglaC
tres_646_Ah_5 (A4, Nd, [(_,Z6),(op1,Z4),(op2,Z6)], n2, A6) = if
	(elem n2 [N4, N6, Nd]) &&
	(elem op1 [OpD_td, OpD_tptd]) &&
	(elem op2 [OpD, OpD_td, OpD_tp, OpD_tptd])
	then Just (
		(A4, Nd, [(Directa,Z4),(Op4_td,Z4),(op2,Z6)], n2, A6),
		Cambio ZZ_manual_4en6 0 3 3)
	else Nothing
tres_646_Ah_5 _ = Nothing



-------------------------------------------------------------------------------
-- Mecanismos para partes de escenario.
-------------------------------------------------------------------------------

-- Estas reglas ya se cubren con los túneles normales



-------------------------------------------------------------------------------
-- Lista de todos los mecanismos de transición.
-------------------------------------------------------------------------------

mecanismos :: [ReglaC]
mecanismos = [
	-- Reglas básicas.
	base_Z4_A4,
	base_Z4_A6_1, base_Z4_A6_2, base_Z4_A6_3,
	base_Z4_Ah_1, base_Z4_Ah_2, base_Z4_Ah_3,
	base_Z6_A4_1, base_Z6_A4_2,
	base_Z6_A6,
	base_Z6_Ah_1, base_Z6_Ah_2, base_Z6_Ah_3,
	base_Zd_A4,
	base_Zd_A6,
	base_Zd_Ah_1, base_Zd_Ah_2,
	base_NAT_1, base_NAT_2, base_NAT_3,

	-- Composición de dos zonas.
	dos_iguales,
	dos_A4_1, dos_A4_2, dos_A4_3, dos_A4_4,
	dos_A6_1, dos_A6_2, dos_A6_3, dos_A6_4,
	dos_Ah_1, dos_Ah_2, dos_Ah_3, dos_Ah_4, dos_Ah_5,
	dos_Ah_6, dos_Ah_7, dos_Ah_8, dos_Ah_9, dos_Ah_10,

	-- Composición de tres zonas.
	tres_464_A4_1, tres_464_A4_2,
	tres_464_A6_1, tres_464_A6_2,
	tres_464_Ah_1, tres_464_Ah_2, tres_464_Ah_3,
	tres_646_A4_1, tres_646_A4_2, tres_646_A4_3,
	tres_646_A6_1, tres_646_A6_2, tres_646_A6_3,
	tres_646_Ah_1, tres_646_Ah_2, tres_646_Ah_3, tres_646_Ah_4, tres_646_Ah_5 ]
