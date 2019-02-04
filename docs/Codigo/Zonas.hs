-------------------------------------------------------------------------------
-- Reglas de creacion de zonas. Estas reglas se encargan de agrupar routers
-- del mismo tipo en una zona comun, con el objetivo de simplificar la
-- topologia y acelerar el procesamiento posterior. Por ejemplo, la red
-- [R4, R4, R4] se convertira en [Z4].
-------------------------------------------------------------------------------

crear_zonas :: Escenario -> (Escenario, [Cambio])
crear_zonas (a1,n1,[],n2,a2) = ((a1,n1,[(Directa,Zd)],n2,a2) , [Cambio ZonaD 0 0 1])
crear_zonas (a1,n1,red,n2,a2) = ((a1,n1,nueva,n2,a2) , cambios)
	where
	(nueva, cambios) = zonas red



zonas :: Red -> (Red, [Cambio])
zonas red = loop red 1 [] [] 
	where
	loop :: Red -> Int -> Red -> [Cambio] -> (Red, [Cambio])
	loop [/ r@(Directa,R4)+! , rs@_* /] i racc cacc = loop rs (i+1) ((Directa,Z4) : racc) ((Cambio Zona4 i (length r) 1) : cacc)
	loop [/ r@(Directa,R6)+! , rs@_* /] i racc cacc = loop rs (i+1) ((Directa,Z6) : racc) ((Cambio Zona6 i (length r) 1) : cacc)
	loop [/ r@(Directa,Rd)+! , rs@_* /] i racc cacc = loop rs (i+1) ((Directa,Zd) : racc) ((Cambio ZonaD i (length r) 1) : cacc)
	loop (r:rs) i racc cacc = loop rs (i+1) (r:racc) cacc 
	loop _ _ racc cacc = (reverse racc, cacc)
