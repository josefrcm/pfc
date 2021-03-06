{-# LANGUAGE DuplicateRecordFields, RankNTypes #-}

-------------------------------------------------------------------------------
-- Autor: José Franco Campos
-- 
-- Generación, análisis y optimización de escenarios de migración
-- de redes Ipv4/IPv6 mediante programación funcional.
--
-- Diciembre 2008
--
--
-- Descripción: implementación del renderizado y y la edición de los grafos.
-------------------------------------------------------------------------------

module Interfaz.Canvas where


-- Módulos externos
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Gdk.Events as GtkEvents
import qualified Graphics.Rendering.Cairo as Cairo

import Control.Monad
import Data.IORef
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import Text.Printf

-- Módulos del programa
import Reglas
import Topologia
import Interfaz.Tipos



-------------------------------------------------------------------------------
-- Constantes.
-------------------------------------------------------------------------------

-- Borde que se deja alrededor del grafo.
borde :: Double
borde = 20.0


-- Radio de los nodos.
radio :: Double
radio = 15.0



---------------------------------------------------------------------------------------------------
-- Funciones para dibujar el grafo.
---------------------------------------------------------------------------------------------------

-- Devuelve True si dos elementos son adyacentes en una lista.
adyacentes :: Eq a => a -> a -> [a] -> Bool
adyacentes e1 e2 (x1:x2:xs)
	| (e1 == x1) && (e2 == x2) = True
	| (e1 == x2) && (e2 == x1) = True
	| otherwise = adyacentes e1 e2 (x2:xs)
adyacentes _ _ _ = False


-- Dibuja un nodo.
dibujarNodo :: Vector -> Bool -> Gtk.Color -> Nodo -> Cairo.Render ()
dibujarNodo pos sel (Gtk.Color r g b) datos = do
	-- Dibuja el relleno del círculo.
	let r' = fromIntegral r / 65535.0
	let g' = fromIntegral g / 65535.0
	let b' = fromIntegral b / 65535.0
	Cairo.setSourceRGB r' g' b'
	Cairo.arc (x pos) (y pos) radio 0 (2*pi)
	Cairo.fillPreserve

	-- Si el nodo está seleccionado, añade un borde adicional.
	when sel $ do
		Cairo.arc (x pos) (y pos) (radio + 4) 0 (2*pi)
	
	-- Dibuja el borde del círculo.
	Cairo.setLineWidth 2
	Cairo.setSourceRGB 0 0 0
	
	-- Escribe el identificador del nodo.
	let txt1 = show $ ident datos
	ext1 <- Cairo.textExtents txt1
	let x1 = (x pos) - (Cairo.textExtentsWidth  ext1 / 2.0)
	let y1 = (y pos) + (Cairo.textExtentsHeight ext1 / 2.0)
	Cairo.moveTo x1 y1
	Cairo.showText txt1
	
	-- Escribe el nombre del nodo, o si no existe, la etiqueta.
	let txt2 = if null (nombre datos) then etiqueta datos else nombre datos
	ext2 <- Cairo.textExtents txt2
	let x2 = (x pos) - (Cairo.textExtentsWidth  ext2 / 2.0)
	let y2 = (y pos) + (Cairo.textExtentsHeight ext2) + radio + 5.0
	Cairo.moveTo x2 y2
	Cairo.showText txt2
	Cairo.stroke


-- Dibuja un arco.
dibujarArco :: Vector -> Vector -> Bool -> Gtk.Color -> Arco -> Cairo.Render ()
dibujarArco p1 p2 sel (Gtk.Color r g b) datos = do
	-- Establece el color del arco.
	let r' = fromIntegral r / 65535.0
	let g' = fromIntegral g / 65535.0
	let b' = fromIntegral b / 65535.0
	Cairo.setSourceRGB r' g' b'
	
	-- Dibuja el arco, ajustando el grosor si está seleccionado.
	if sel
		then Cairo.setLineWidth 3
		else Cairo.setLineWidth 2
	Cairo.moveTo (x p1) (y p1)
	Cairo.lineTo (x p2) (y p2)
	
	-- Genera las etiquetas
	let txt1 = printf "Lat: %f" (latencia datos) :: String
	let txt2 = printf "BW: %f"  (anchoBanda datos) :: String
	let txt3 = printf "Met: %f" (metrica datos) :: String
	
	-- Calcula la normal al arco (método ineficiente).
	let centro = (p1 .+ p2) ./ 2.0
	let angulo = (pi / 2.0) + atan ((y p2 - y p1) / (x p2 - x p1))
	let normal = Vector {
		x = cos angulo,
		y = sin angulo,
		z = 0.0 }

	-- Calcula el tamaño de las etiquetas, y el centro del texto.
	exts <- mapM Cairo.textExtents [txt1, txt2, txt3]
	let ancho = maximum (map Cairo.textExtentsWidth exts)
	let alto = maximum (map Cairo.textExtentsHeight exts)
	let dims = Vector {
		x = 0.5 * ancho,
		y = 1.5 * alto,
		z = 0.0 }
	let dist = sqrt (dot dims dims)
	let pos = centro .- dims .+ (normal .* dist)
	
	-- Muestra el texto.
	Cairo.moveTo (x pos) (y pos + 1*alto)
	Cairo.showText txt1
	Cairo.moveTo (x pos) (y pos + 2*alto)
	Cairo.showText txt2	
	Cairo.moveTo (x pos) (y pos + 3*alto)
	Cairo.showText txt3
	Cairo.stroke


-- Actualiza el contenido del área de dibujo.
dibujarGrafo :: forall widget. (Gtk.WidgetClass widget) => IORef Configuracion -> widget -> IO Bool
dibujarGrafo conf canvas = do
	-- Calcula el centro del grafo en función del tamaño del área de dibujo.
	win <- Gtk.widgetGetDrawWindow canvas
	(xs, ys) <- Gtk.widgetGetSize canvas
	let (xc, yc) = (fromIntegral xs / 2.0, fromIntegral ys / 2.0)

	-- Lee la configuración
	conf' <- readIORef conf
	let delta = pos_nueva (sel_nodos conf') .- pos_vieja (sel_nodos conf')
	let ruta' = case sol_sel conf' of
		Nothing -> []
		Just s -> ruta s
	
	-- Dibuja los elementos
	Gtk.renderWithDrawable win $ do
		-- Guarda la matriz de transformación y centra el grafo.
		Cairo.save
		Cairo.translate xc yc
		Cairo.scale (zoom conf') (zoom conf')
	
		-- Pinta el fondo de amarillo claro.
		Cairo.setSourceRGB 1.0 1.0 0.92
		Cairo.paint
	
		-- Dibuja todos los arcos.
		forM_ (listaArcos (topologia conf')) $ \((src,dst),arco) -> when (src < dst) $ do
			-- Coordenadas de origen.
			let p1 = coordenadasNodo (topologia conf') src .+
				if IntSet.member src (lista_ids (sel_nodos conf'))
				then delta
				else Vector 0.0 0.0 0.0
			
			-- Coordenadas de destino.
			let p2 = coordenadasNodo (topologia conf') dst .+
				if IntSet.member dst (lista_ids (sel_nodos conf'))
				then delta
				else Vector 0.0 0.0 0.0
			
			-- Ajusta el color dependiendo de si está seleccionado o es parte de la solución.
			let (sel, color) = if Set.member (src,dst) (sel_arcos conf') 
				then (True, Gtk.Color 65535 0 0)
				else if adyacentes src dst ruta'
					then (True, Gtk.Color 0 0 65535)
					else (False, Gtk.Color 0 0 0)
			
			-- Dibuja el arco.
			dibujarArco p1 p2 sel color arco
	
		-- Dibuja todos los nodos
		Cairo.setFontSize radio
		forM_ (listaNodos (topologia conf')) $ \(k, nodo) -> do
			-- Posición del nodo.
			let pos = center (graficos nodo) .+
				if IntSet.member k (lista_ids (sel_nodos conf'))
				then delta
				else Vector 0.0 0.0 0.0
			
			-- Consulta si está seleccionado, y su color en función del tipo de router.
			let sel = IntSet.member k (lista_ids (sel_nodos conf'))
			let color = Map.findWithDefault (Gtk.Color 0 0 0) (protocolo nodo) (colores conf')
			
			-- Dibuja el nodo.
			dibujarNodo pos sel color nodo
		
		-- Restaura la matriz de transformación y termina.
		Cairo.restore
	return True



-------------------------------------------------------------------------------
-- Manejadores de evento
-------------------------------------------------------------------------------

-- Devuelve la posición del cursor en el espacio de coordenadas del grafo.
posicionCursor :: Gtk.DrawingArea -> IORef Configuracion -> GtkEvents.Event -> IO Vector
posicionCursor canvas conf e = do
	conf' <- readIORef conf
	(xs, ys) <- Gtk.widgetGetSize canvas
	let cursor1 = Vector (GtkEvents.eventX e) (GtkEvents.eventY e) 0.0
	let cursor2 = cursor1 .- Vector (fromIntegral xs / 2.0) (fromIntegral ys / 2.0) 0.0
	let cursor3 = cursor2 ./ (zoom conf')
	return cursor3


-- Operaciones cuando se pulsa el ratón.
pulsarRaton :: Gtk.DrawingArea -> IORef Configuracion -> GtkEvents.Event -> IO Bool
pulsarRaton canvas conf event = do
	conf' <- readIORef conf
	case (modo conf', GtkEvents.eventButton event) of
		-- Edición de un nodo ya existente
		(EditarNodos, GtkEvents.LeftButton) -> do
			pos <- posicionCursor canvas conf event
			case buscarNodoCoord (topologia conf') pos radio of
				-- Se ha pinchado en un espacio vacío.
				Nothing -> if Gtk.Shift `elem` (GtkEvents.eventModifier event)
					then return ()
					else modifyIORef conf $ \c -> c {
						sel_nodos = Sel {
							lista_ids = IntSet.empty,
							pos_vieja = Vector 0.0 0.0 0.0,
							pos_nueva = Vector 0.0 0.0 0.0 }}
				-- Se ha pinchado en un nodo existente.
				Just n  -> do
					let sel_nodos' = if Gtk.Shift `elem` (GtkEvents.eventModifier event)
						then IntSet.insert n (lista_ids $ sel_nodos conf')
						else IntSet.singleton n
					modifyIORef conf $ \c -> c {
						sel_nodos = Sel {
							lista_ids = sel_nodos',
							pos_vieja = pos,
							pos_nueva = pos },
						pulsado = True}
			return True
			
		-- Edición de un arco ya existente
		(EditarEnlaces, GtkEvents.LeftButton) -> do
			pos <- posicionCursor canvas conf event
			case buscarArcoCoord (topologia conf') pos 5.0 of
				-- Se ha pinchado en un espacio vacío.
				Nothing -> if Gtk.Shift `elem` (GtkEvents.eventModifier event)
					then return ()
					else modifyIORef conf $ \c -> c {sel_arcos = Set.empty }
				-- Se ha pinchado en un arco existente.
				Just a -> do
					let sel_arcos' = if Gtk.Shift `elem` (GtkEvents.eventModifier event)
						then Set.insert a (sel_arcos conf')
						else Set.singleton a
					modifyIORef conf $ \c -> c {sel_arcos = sel_arcos'}
			return True
			
		-- Creación de un nuevo nodo
		(CrearNodos, GtkEvents.LeftButton) -> do
			-- Obtiene la posición actual del cursor.
			pos <- posicionCursor canvas conf event
			-- Crea el nuevo nodo.
			let nuevo = Nodo {
				ident = nuevoId (topologia conf'),
				protocolo = (prot conf'),
				nombre = "",
				etiqueta = "",
				graficos = Graficos {
					center = pos,
					atr_graficos = Map.empty },
				atr_nodo = Map.empty}
			-- Añade el nodo al grafo, y lo marca como modificado.
			modifyIORef conf $ \c -> c {
				topologia = insertarNodo (topologia c) nuevo,
				modificado = True }
			return True
			
		-- Conexión de dos nodos ya existentes
		(CrearEnlaces, GtkEvents.LeftButton) -> do
			pos <- posicionCursor canvas conf event
			case (IntSet.elems $ lista_ids $ sel_nodos conf', buscarNodoCoord (topologia conf') pos radio) of
				-- Se selecciona el primer nodo.
				([], Just n)  -> do
					modifyIORef conf $ \c -> c {
						sel_nodos = Sel {
							lista_ids = IntSet.singleton n,
							pos_vieja = Vector 0.0 0.0 0.0,
							pos_nueva = Vector 0.0 0.0 0.0 }}
				
				-- Se selecciona el segundo nodo.
				([n1], Just n2) -> do
					modifyIORef conf $ \c -> c {
						topologia = insertarArco (topologia c) (Arco n1 n2 0.0 1.0 0.0 Map.empty),
						sel_nodos = Sel {
							lista_ids = IntSet.empty,
							pos_vieja = Vector 0.0 0.0 0.0,
							pos_nueva = Vector 0.0 0.0 0.0 },
						modificado = True}

				-- No se ha seleccionado nada.
				_ -> return ()
			
			return True
		
		-- Nada
		_ -> return (GtkEvents.eventSent event)


-- Al soltar el botón izquierdo, si había algún nodo seleccionado, se deja en
-- la nueva posición.
soltarRaton :: Gtk.DrawingArea -> IORef Configuracion -> GtkEvents.Event -> IO Bool
soltarRaton _ conf event = do
	conf' <- readIORef conf
	-- Si había un nodo selecionado...
	if (modo conf' == EditarNodos) &&
	   (GtkEvents.eventButton event == GtkEvents.LeftButton) &&
	   (not $ IntSet.null (lista_ids $ sel_nodos conf'))
		then do
			-- Desplaza los nodos, y marca el grafo como modificado.
			let delta = pos_nueva (sel_nodos conf') .- pos_vieja (sel_nodos conf')
			modifyIORef conf $ \c -> c {
				topologia = Topologia.moverNodos (topologia c) (IntSet.elems $ lista_ids $ sel_nodos c) delta,
				pulsado = False,
				sel_nodos = (sel_nodos c) {
					pos_vieja = Vector 0.0 0.0 0.0,
					pos_nueva = Vector 0.0 0.0 0.0 },
				modificado = True}
			return True

		-- Si no, no hace nada
		else return (GtkEvents.eventSent event)


-- Al mover el ratón, si hay un nodo seleccionado, se arrastra a la nueva posición.
moverRaton :: Gtk.DrawingArea -> IORef Configuracion -> GtkEvents.Event -> IO Bool
moverRaton canvas conf event = do
		conf' <- readIORef conf
		-- Si había un nodo selecionado...
		if (modo conf' == EditarNodos) &&
		   (not $ IntSet.null (lista_ids $ sel_nodos conf')) &&
		   (pulsado conf')
			then do
				-- Mueve el nodo a la nueva posición.
				pos <- posicionCursor canvas conf event
				modifyIORef conf $ \c -> c {
					sel_nodos = (sel_nodos c) {pos_nueva = pos},
					modificado = True}
				return True
			
			-- Si no, no hace nada.
			else return (GtkEvents.eventSent event)
