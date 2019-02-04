-- Operador de conexion dual
op1 :: ReglaP
op1 [/ (conexion,zi) , routers@(Directa,Zd)+! , (Directa,zj) , xs@_* /] = if
	subset [zi,zj] [Z4,Z6]
	then Just (
		(conexion,zi) : (OpD,zj) : xs,
		Cambio Operador1 1 (2 + length routers) 2)
	else Nothing
op1 _ = Nothing

-- Operador de conexion IPv4 con traducción de direcciones IPv4
op2 :: ReglaP
op2 [/ (conexion,zi) , (Directa,R4_td) , (Directa,zj) , xs@_* /] = if
	(subset [zi,zj] [Z4,Z6,Zd])
	then Just (
		(conexion,zi) : (Op4_td,zj) : xs,
		Cambio Operador2 1 3 2)
	else Nothing
op2 _ = Nothing

-- Operador de conexion dual con traducción de direcciones IPv4
op3 :: ReglaP
op3 [/ (conexion,zi) , r1@(Directa,Zd)*! , (Directa,Rd_td) , r2@(Directa,Zd)*! , (Directa,zj) , xs@_* /] = if
	--(subset [zi,zj] [Z4,Z6])
	(subset [zi,zj] [Z4,Z6,Zd])
	then Just (
		(conexion,zi) : (OpD_td,zj) : xs,
		Cambio Operador3 1 (3 + length r1 + length r2) 2)
	else Nothing
op3 _ = Nothing

-- Operador de conexion dual con traducción de protocolos IPv4 e IPv6
op4 :: ReglaP
op4 [/ (conexion,zi) , r1@(Directa,Zd)*! , (Directa,Rd_tp) , r2@(Directa,Zd)*! , (Directa,zj) , xs@_* /] = if
	--(subset [zi,zj] [Z4,Z6,Zd]) &&
	(subset [zi,zj] [Z4,Z6]) &&
	(zi /= zj)
	then Just (
		(conexion,zi) : (OpD_tp,zj) : xs,
		Cambio Operador4 1 (3 + length r1 + length r2) 2)
	else Nothing
op4 _ = Nothing

-- Operador de conexion dual con traducción de direcciones IPv4 y traducción de protocolos IPv4 e IPv6
op5 :: ReglaP
op5 [/ (conexion,zi) , r1@(Directa,Zd)*! , (Directa,Rd_tptd) , r2@(Directa,Zd)*! , (Directa,zj) , xs@_* /] = if
	--(subset [zi,zj] [Z4,Z6,Zd]) &&
	(subset [zi,zj] [Z4,Z6]) &&
	(zi /= zj)
	then Just (
		(conexion,zi) : (OpD_tptd,zj) : xs,
		Cambio Operador5 1 (3 + length r1 + length r2) 2)
	else Nothing
op5 _ = Nothing
