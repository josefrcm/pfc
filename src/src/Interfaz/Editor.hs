{-# OPTIONS_GHC -fglasgow-exts #-}

-------------------------------------------------------------------------------
-- Autor: José Franco Campos
-- 
-- Generación, análisis y optimización de escenarios de migración
-- de redes Ipv4/IPv6 mediante programación funcional.
-- 
-- Diciembre 2008
--
--
-- Descripción: implementación de la ventana de edición de topologías.
--              Este módulo es el más importante del programa, porque se
--              encarga de llamar al resto de funciones.
-------------------------------------------------------------------------------

module Interfaz.Editor where


-- Módulos externos
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import qualified Graphics.UI.Gtk.ModelView as MV
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.IORef
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import System.FilePath.Posix
import Text.Printf

-- Módulos del programa
import Reglas
import Topologia
import Interfaz.Canvas
import Interfaz.DlgAbrir
import Interfaz.DlgBusqueda
import Interfaz.DlgConfirmar
import Interfaz.DlgGuardar
import Interfaz.DlgMostrar
import Interfaz.DlgOpciones
import Interfaz.DlgSoluciones
import Interfaz.Tipos


-------------------------------------------------------------------------------
-- Funciones auxiliares.
-------------------------------------------------------------------------------

-- Tabla de costes predeterminada.
costes_iniciales :: Coste
costes_iniciales = Map.fromList [
	(MT_dual_stack, 20.0),
	(MT_mapped, 10.0),
	(MT_broker, 15.0),
	(MT_dstm, 20.0),
	(MT_isatap, 25.0),
	(MT_6to4, 17.0),
	(MT_teredo, 30.0),
	(MT_auto, 15.0),
	(MT_nat_pt, 60.0),
	(MT_siit, 70.0),
	(MT_socks, 80.0),
	(MT_trt, 90.0)]


-- Inicializa la configuración del programa a partir de un grafo.
inicializar :: Grafo -> Configuracion
inicializar grafo = Conf {
		sel_nodos = Sel {
			lista_ids = IntSet.empty,
			pos_vieja = Vector 0.0 0.0 0.0,
			pos_nueva = Vector 0.0 0.0 0.0},
		sel_arcos = Set.empty,
		zoom = 1.0,
		modo = EditarNodos,
		prot = R4,
		fichero = Nothing,
		modificado = False,
		opciones = Nothing,
		soluciones = [],
		sol_sel = Nothing,
		pulsado = False,
		costes = (costes_iniciales, costes_iniciales, costes_iniciales),
		colores = Map.fromList [
			(R4, Color 0 0 0),
			(R6, Color 0 0 0),
			(Rd, Color 0 0 0),
			(R4_td, Color 0 0 0),
			(Rd_td, Color 0 0 0),
			(Rd_tp, Color 0 0 0),
			(Rd_tptd, Color 0 0 0)],
		topologia = grafo
		}


-- Lee la configuración de los botones de selección de color.
leerColores :: Map.Map Router ColorButton -> IO Colores
leerColores botones = do
	rgb_ipv4      <- colorButtonGetColor (botones Map.! R4)
	rgb_ipv6      <- colorButtonGetColor (botones Map.! R6)
	rgb_dual      <- colorButtonGetColor (botones Map.! Rd)
	rgb_ipv4_td   <- colorButtonGetColor (botones Map.! R4_td)
	rgb_dual_td   <- colorButtonGetColor (botones Map.! Rd_td)
	rgb_dual_tp   <- colorButtonGetColor (botones Map.! Rd_tp)
	rgb_dual_tptd <- colorButtonGetColor (botones Map.! Rd_tptd)
	return $ Map.fromList [
		(R4, rgb_ipv4),
		(R6, rgb_ipv6),
		(Rd, rgb_dual),
		(R4_td, rgb_ipv4_td),
		(Rd_td, rgb_dual_td),
		(Rd_tp, rgb_dual_tp),
		(Rd_tptd, rgb_dual_tptd)]



-------------------------------------------------------------------------------
-- Funciones de actualización
-------------------------------------------------------------------------------

-- Actualiza el contenido del área de dibujo	
actualizarVentana :: forall self. (WindowClass self) => self -> DrawingArea -> IORef Configuracion -> IO ()
actualizarVentana ventana canvas conf = do
	conf' <- readIORef conf
	let titulo = case fichero conf' of
		Nothing -> printf "Migración -> sin título"
		Just f  -> printf "Migración -> %s%s" (takeFileName f) (if modificado conf' then " (modificado)" else "")
		
	windowSetTitle ventana titulo
	widgetQueueDraw canvas


-- Añade una columna a la vista de resultados
crearColumna :: forall a self. (Show a, TreeViewClass self, Ord a)
             => self
             -> MV.ListStore Solucion
             -> String
             -> (Solucion -> a)
             -> IORef Configuracion
             -> IO ()
crearColumna vista lista titulo campo conf = do
	-- Crea la columna
	col <- MV.treeViewColumnNew
	MV.treeViewColumnSetTitle  col titulo
	
	-- Añade el renderizador para que muestre el contenido
	renderer <- MV.cellRendererTextNew
	MV.cellLayoutPackStart     col renderer True
	MV.cellLayoutSetAttributes col renderer lista $ \fila -> [ MV.cellText := show (campo fila) ]
	MV.treeViewAppendColumn    vista col
		
	-- Añade el manejador de señal para la ordenación
	treeViewColumnSetClickable col True
	col `onColClicked` do
		conf' <- readIORef conf
		let resultados = sortBy (\s1 s2 -> (campo s1) `compare` (campo s2)) (soluciones conf')
		MV.listStoreClear lista
		forM_ resultados (\i -> MV.listStoreAppend lista i)
		MV.treeViewColumnsAutosize vista
	return ()


-- Centra el desplazamiento del área de edición en el origen del grafo
centrarGrafo :: ScrolledWindow -> IORef Configuracion -> IO ()
centrarGrafo scroll conf = do
	conf' <- readIORef conf
	let (Vector xc yc _) = centroGrafo (topologia conf')
	hadj <- scrolledWindowGetHAdjustment scroll
	vadj <- scrolledWindowGetVAdjustment scroll
	hpage <- adjustmentGetPageSize hadj
	vpage <- adjustmentGetPageSize vadj
	adjustmentSetValue hadj (5000 + xc - (hpage/2))
	adjustmentSetValue vadj (5000 + yc - (vpage/2))


-- Borra la lista de resultados
vaciarResultados :: IORef Configuracion -> MV.ListStore a -> IO ()
vaciarResultados conf lista = do
	modifyIORef conf $ \c -> c {
		--sel_nodos = Sel {lista_ids = IntSet.empty, pos_vieja = Vector 0 0 0, pos_nueva = Vector 0 0 0},
		--sel_arcos = Set.empty,
		soluciones = [],
		sol_sel = Nothing }
	MV.listStoreClear lista



-------------------------------------------------------------------------------
-- Manejadores de eventos para las opciones de los menús
-------------------------------------------------------------------------------

-- Creación de una nueva topología. Devuelve True si realmente se ha creado,
-- y False en caso contrario.
opcionNuevo :: IORef Configuracion -> IO Bool
opcionNuevo conf = do
	conf' <- readIORef conf
	crear <- if (modificado conf') then do
		-- Si el fichero actual ha sido modificado, se pide
		-- confirmación antes de cerrarlo.
		ok <- crearConfirmar
		case ok of
			Guardar -> do
				case fichero conf' of
					Nothing -> do
						fichero' <- crearGuardar
						case fichero' of
							Nothing -> return False
							Just f -> do
								guardarGrafo f (topologia conf')
								return True
					Just f -> do
						guardarGrafo f (topologia conf')
						return True
			Cerrar -> return True
			Cancelar -> return False
		-- Si no, se cierra sin preguntar.
		else return True

	-- Crea la nueva topología.
	when crear $ do
		modifyIORef conf $ \c -> c {
			topologia = vacio,
			fichero = Nothing,
			modificado = False,
			soluciones = [] }
	return crear


-- Apertura de un fichero GML.
opcionAbrir :: IORef Configuracion -> IO Bool
opcionAbrir conf = Control.Exception.catch
	(do
		-- Intenta cargar el fichero.
		fichero' <- crearAbrir
		case fichero' of
			Nothing -> return False
			Just f -> do
				datos <- cargarGrafo f
				datos' <- evaluate datos
				modifyIORef conf $ \c -> c {
					topologia = datos',
					fichero = fichero',
					soluciones = [] }
				return True)
	(\e -> do
		-- Si se produce algún error, muestra un mensaje al usuario.
		dlg_error <- messageDialogNew
			Nothing
			[]
			MessageError
			ButtonsClose
			("Error al cargar el fichero:\n" ++ show e)
		widgetShow dlg_error
		_ <- dialogRun dlg_error
		widgetDestroy dlg_error
		return False)


-- Escritura de una topología.
opcionGuardar :: IORef Configuracion -> IO ()
opcionGuardar conf = do
	conf' <- readIORef conf
	case fichero conf' of
		-- Si la topología no tenía nombre, pide al usuario que elija un fichero.
		Nothing -> do
			fichero' <- crearGuardar
			case fichero' of
				Nothing -> return ()
				Just f -> do
					guardarGrafo f (topologia conf')
					modifyIORef conf $ \c -> c {fichero = fichero', modificado = False}
		-- Si ya tenía nombre, la guarda.
		Just f -> do
			guardarGrafo f (topologia conf')
			modifyIORef conf $ \c -> c {modificado = False}


-- Escritura de una topología con un nuevo nombre.
opcionGuardarComo :: IORef Configuracion -> IO ()
opcionGuardarComo conf = do
	conf' <- readIORef conf
	fichero' <- crearGuardar
	case fichero' of
		Nothing -> return ()
		Just f -> do
			guardarGrafo f (topologia conf')
			modifyIORef conf $ \c -> c {fichero = fichero', modificado = False}


-- Sale del programa.
opcionSalir :: IORef Configuracion -> IO Bool
opcionSalir conf = do
	conf' <- readIORef conf
	-- Si el grafo ha sido modificado y aún no se ha guardado, pide confirmación al usuario.
	if (modificado conf') then do
		ok <- crearConfirmar
		case ok of
			Guardar -> do
				case fichero conf' of
					Nothing -> do
						fichero' <- crearGuardar
						case fichero' of
							Nothing -> return ()
							Just f -> guardarGrafo f (topologia conf')
					Just f -> guardarGrafo f (topologia conf')
				return True
			Cerrar -> return True
			Cancelar -> return False

		else return True


-- Configuración de los costes de los mecanismos de transición.
opcionConfigurar :: IORef Configuracion -> IO ()
opcionConfigurar conf = do
	conf' <- readIORef conf
	costes' <- crearOpciones (costes conf')
	case costes' of
		Nothing -> return ()
		Just cs -> modifyIORef conf $ \c -> c {costes = cs}
	return ()


-- Búsqueda de soluciones.
opcionBuscar :: IORef Configuracion -> IO [Solucion]
opcionBuscar conf = do
	-- Obtiene los identificadores de los nodos.
	conf' <- readIORef conf
	let keys = [k | (k,_) <- listaNodos (topologia conf')]
	if null keys
		-- No se pueden buscar soluciones en grafos vacíos.
		then do
			dlg_info <- messageDialogNew
				Nothing
				[]
				MessageInfo
				ButtonsClose
				("El grafo debe tener al menos un nodo.")
			widgetShow dlg_info
			_ <- dialogRun dlg_info
			widgetDestroy dlg_info
			return []
		-- Lanza el proceso de búsqueda de rutas.
		else do
			op <- crearBusqueda (minimum keys) (maximum keys) (opciones conf')
			case op of
				Nothing -> return []
				Just op_val -> do
					sols <- crearSoluciones op_val conf
					
					-- Guarda los parámetros para la siguiente vez.
					modifyIORef conf $ \c -> c {
						opciones = Just op_val,
						soluciones = sols }
					return sols



-------------------------------------------------------------------------------
-- Crea la ventana de edición.
-------------------------------------------------------------------------------

-- Ruta al fichero con la definición de la interfaz.
interfaz :: FilePath
interfaz = "res/editor.glade"


-- Crea la ventana de edición y entra en el bucle principal.
crearEditor :: FilePath -> IO ()
crearEditor path = do
	-- Inicializa GTK.
	initGUI
	timeoutAddFull (yield >> return True) priorityDefaultIdle 50
	
	-- Carga la interfaz gráfica.
	ui <- xmlNew interfaz
	let xml = case ui of
		Just i -> i
		Nothing -> error $ "No se puede cargar el fichero \"" ++ interfaz ++ "\""

	-- Crea la configuración inicial.
	conf <- if null path
		then newIORef (inicializar vacio)
		else do
			g <- cargarGrafo path
			newIORef (inicializar g)
	
	---------------------------------------------------------------------------
	-- Ventana principal.
	---------------------------------------------------------------------------

	window <- xmlGetWidget xml castToWindow "window1"
	scroll <- xmlGetWidget xml castToScrolledWindow "scrolledwindow1"
	window `onDestroy` mainQuit


	---------------------------------------------------------------------------
	-- Canvas.
	---------------------------------------------------------------------------

	canvas <- xmlGetWidget xml castToDrawingArea "drawingarea1"

	canvas `onExpose` \e -> case e of
		(Expose _ _ _ _) -> dibujarGrafo conf canvas
		_ -> return False

	canvas `onButtonPress` \e -> do
		ok <- pulsarRaton canvas conf e
		actualizarVentana window canvas conf
		return ok

	canvas `onButtonRelease` \e -> do
		ok <- soltarRaton canvas conf e
		actualizarVentana window canvas conf
		return ok
	
	onMotionNotify canvas True $ \e -> do
		ok <- moverRaton canvas conf e
		actualizarVentana window canvas conf
		return ok


	---------------------------------------------------------------------------
	-- Barra de zoom.
	---------------------------------------------------------------------------

	-- Nivel de zoom.
	barra_zoom <- xmlGetWidget xml castToHScale "barra_zoom"

	-- Manejador de señal para la barra de zoom.
	barra_zoom `onRangeValueChanged` do
		val <- rangeGetValue barra_zoom
		modifyIORef conf $ \c -> c {zoom = val}
		actualizarVentana window canvas conf

	-- Cambia el zoom cuando se mueve la rueda del ratón.
	canvas `onScroll` \e -> do
		conf' <- readIORef conf
		case eventDirection e of
			ScrollUp -> when (zoom conf' < 10.0) $ do
				let zoom' = zoom conf' * 1.1
				modifyIORef conf $ \c -> c {zoom = zoom'}
				barra_zoom `rangeSetValue` zoom'
			ScrollDown -> when (zoom conf' > 0.1) $ do
				let zoom' = zoom conf' / 1.1
				modifyIORef conf $ \c -> c {zoom = zoom'}
				barra_zoom `rangeSetValue` zoom'
			_ -> return ()
		actualizarVentana window canvas conf
		return True

	-- Borrado de elementos.
	window `onKeyPress` \e -> do
		conf' <- readIORef conf
		when (eventKeyName e == "Delete") $ do
			let grafo' = borrarNodos (topologia conf') (IntSet.elems $ lista_ids $ sel_nodos conf')
			let grafo'' = borrarArcos grafo' (Set.elems $ sel_arcos conf')
			modifyIORef conf $ \c -> c {
				topologia = grafo'',
				sel_nodos = Sel {
					lista_ids = IntSet.empty,
					pos_vieja = Vector 0.0 0.0 0.0,
					pos_nueva = Vector 0.0 0.0 0.0 },
				pulsado = False,
				modificado = True}
			actualizarVentana window canvas conf
		return True


	---------------------------------------------------------------------------
	-- Lista de resultados
	---------------------------------------------------------------------------

	-- Crea la vista de resultados y la lista de soluciones.
	lista <- MV.listStoreNew [] 
	vista_resultados <- xmlGetWidget xml castToTreeView "vista_resultados"
	vista_resultados `MV.treeViewSetModel` lista
	vista_resultados `treeViewSetHeadersVisible` True

	-- Crea las columnas.
	crearColumna vista_resultados lista "Latencia" coste_lat conf
	crearColumna vista_resultados lista "Ancho de banda" coste_bw conf
	crearColumna vista_resultados lista "Métrica" coste_met conf
	crearColumna vista_resultados lista "Ruta" ruta conf

	-- Añade la señal para que cuando el usuario seleccione una solución
	-- se resalte la ruta en la ventana de edición.
	treeViewColumnsAutosize vista_resultados
	vista_resultados `onCursorChanged` do
		([indice], _) <- treeViewGetCursor vista_resultados
		valor <- MV.listStoreGetValue lista indice
		modifyIORef conf $ \c -> c {sol_sel = Just valor}
		actualizarVentana window canvas conf
	
	-- Añade la señal para que cuando el usuario haga doble click sobre
	-- una solución, esta se muestre con más detalle en una nueva ventana.
	vista_resultados `onRowActivated` \fila _ -> do
		let indice = head fila
		valor <- MV.listStoreGetValue lista indice
		crearMostrar valor


	---------------------------------------------------------------------------
	-- Botones de modo.
	---------------------------------------------------------------------------

	-- Extrae los widgets.
	btn_editar_nodos   <- xmlGetWidget xml castToRadioButton "btn_editar_nodos"
	btn_editar_enlaces <- xmlGetWidget xml castToRadioButton "btn_editar_enlaces"
	btn_crear_nodos    <- xmlGetWidget xml castToRadioButton "btn_crear_nodos"
	btn_crear_enlaces  <- xmlGetWidget xml castToRadioButton "btn_crear_enlaces"

	-- Manejadores de señal para los botones de modo.
	btn_editar_nodos `onToggled` do
		modifyIORef conf $ \c -> c {
			modo = EditarNodos,
			sel_nodos = Sel {
				lista_ids = IntSet.empty,
				pos_vieja = Vector 0.0 0.0 0.0,
				pos_nueva = Vector 0.0 0.0 0.0 },
			sel_arcos = Set.empty }
		actualizarVentana window canvas conf
		
	btn_editar_enlaces `onToggled` do
		modifyIORef conf $ \c -> c {
			modo = EditarEnlaces,
			sel_nodos = Sel {
				lista_ids = IntSet.empty,
				pos_vieja = Vector 0.0 0.0 0.0,
				pos_nueva = Vector 0.0 0.0 0.0 },
			sel_arcos = Set.empty }
		actualizarVentana window canvas conf
	
	btn_crear_nodos `onToggled` do
		modifyIORef conf $ \c -> c {
			modo = CrearNodos,
			sel_nodos = Sel {
				lista_ids = IntSet.empty,
				pos_vieja = Vector 0.0 0.0 0.0,
				pos_nueva = Vector 0.0 0.0 0.0 },
			sel_arcos = Set.empty }
		actualizarVentana window canvas conf
		
	btn_crear_enlaces `onToggled` do
		modifyIORef conf $ \c -> c {
			modo = CrearEnlaces,
			sel_nodos = Sel {
				lista_ids = IntSet.empty,
				pos_vieja = Vector 0.0 0.0 0.0,
				pos_nueva = Vector 0.0 0.0 0.0 },
			sel_arcos = Set.empty }
		actualizarVentana window canvas conf


	---------------------------------------------------------------------------
	-- Botones de selección.
	---------------------------------------------------------------------------

	-- Botones de selección de tipo de router.
	sel_ipv4      <- xmlGetWidget xml castToRadioButton "sel_ipv4"
	sel_ipv6      <- xmlGetWidget xml castToRadioButton "sel_ipv6"
	sel_dual      <- xmlGetWidget xml castToRadioButton "sel_dual"
	sel_ipv4_td   <- xmlGetWidget xml castToRadioButton "sel_ipv4_td"
	sel_dual_td   <- xmlGetWidget xml castToRadioButton "sel_dual_td"
	sel_dual_tp   <- xmlGetWidget xml castToRadioButton "sel_dual_tp"
	sel_dual_tptd <- xmlGetWidget xml castToRadioButton "sel_dual_tptd"

	-- Manejadores de señal para los botones de router.
	sel_ipv4      `onToggled` (modifyIORef conf $ \c -> c {prot = R4})
	sel_ipv6      `onToggled` (modifyIORef conf $ \c -> c {prot = R6})
	sel_dual      `onToggled` (modifyIORef conf $ \c -> c {prot = Rd})
	sel_ipv4_td   `onToggled` (modifyIORef conf $ \c -> c {prot = R4_td})
	sel_dual_td   `onToggled` (modifyIORef conf $ \c -> c {prot = Rd_td})
	sel_dual_tp   `onToggled` (modifyIORef conf $ \c -> c {prot = Rd_tp})
	sel_dual_tptd `onToggled` (modifyIORef conf $ \c -> c {prot = Rd_tptd})
	toggleButtonToggled sel_ipv4


	---------------------------------------------------------------------------
	-- Botones de colores.
	---------------------------------------------------------------------------

	-- Botones para configurar los colores de los routers.
	color_ipv4       <- xmlGetWidget xml castToColorButton "color_ipv4"
	color_ipv6       <- xmlGetWidget xml castToColorButton "color_ipv6"
	color_dual       <- xmlGetWidget xml castToColorButton "color_dual"
	color_ipv4_td    <- xmlGetWidget xml castToColorButton "color_ipv4_td"
	color_dual_td    <- xmlGetWidget xml castToColorButton "color_dual_td"
	color_dual_tp    <- xmlGetWidget xml castToColorButton "color_dual_tp"
	color_dual_tptd  <- xmlGetWidget xml castToColorButton "color_dual_tptd"

	-- Establece los manejadores de señal.
	let colores' = Map.fromList [
		(R4, color_ipv4),
		(R6, color_ipv6),
		(Rd, color_dual),
		(R4_td, color_ipv4_td),
		(Rd_td, color_dual_td),
		(Rd_tp, color_dual_tp),
		(Rd_tptd, color_dual_tptd)]
	forM_ [color_ipv4, color_ipv6, color_dual, color_ipv4_td,
	      color_dual_td, color_dual_tp, color_dual_tptd] $ \i -> do
		i `onColorSet` do
			col <- leerColores colores'
			modifyIORef conf $ \c -> c {colores = col}

	-- Lee el valor inicial de los colores.
	col <- leerColores colores'
	modifyIORef conf $ \c -> c {colores = col}


	---------------------------------------------------------------------------
	-- Barra de menús.
	---------------------------------------------------------------------------

	-- Opciones de los menús.
	menu_nuevo <- xmlGetWidget xml castToImageMenuItem "menu_nuevo"
	menu_nuevo `onActivateLeaf` do
		ok <- opcionNuevo conf
		when ok (vaciarResultados conf lista)
		actualizarVentana window canvas conf
		centrarGrafo scroll conf

	menu_abrir <- xmlGetWidget xml castToImageMenuItem "menu_abrir"
	menu_abrir `onActivateLeaf` do
		ok <- opcionAbrir conf
		when ok (vaciarResultados conf lista)
		actualizarVentana window canvas conf
		centrarGrafo scroll conf

	menu_guardar <- xmlGetWidget xml castToImageMenuItem "menu_guardar"
	menu_guardar `onActivateLeaf` do
		opcionGuardar conf
		actualizarVentana window canvas conf

	menu_guardar_como <- xmlGetWidget xml castToImageMenuItem "menu_guardar_como"
	menu_guardar_como `onActivateLeaf` do
		opcionGuardarComo conf
		actualizarVentana window canvas conf

	menu_salir <- xmlGetWidget xml castToImageMenuItem "menu_salir"
	menu_salir `onActivateLeaf` do
		ok <- opcionSalir conf
		when ok (widgetDestroy window)

	menu_configurar <- xmlGetWidget xml castToMenuItem "menu_configurar"
	menu_configurar `onActivateLeaf` do
		opcionConfigurar conf
		actualizarVentana window canvas conf
		
	menu_buscar <- xmlGetWidget xml castToMenuItem "menu_buscar"
	menu_buscar `onActivateLeaf` do
		resultados <- opcionBuscar conf
		MV.listStoreClear lista
		forM_ resultados (\i -> MV.listStoreAppend lista i)
		MV.treeViewColumnsAutosize vista_resultados
		actualizarVentana window canvas conf


	---------------------------------------------------------------------------
	-- Selección de protocolo.
	---------------------------------------------------------------------------

	sel_protocolo <- xmlGetWidget xml castToComboBox "sel_protocolo"
	comboBoxSetActive sel_protocolo 0 
	sel_protocolo `onChanged` do
		conf' <- readIORef conf
		sel'  <- comboBoxGetActiveText sel_protocolo
		case sel' of
			Nothing -> return ()
			Just p -> do
				let nuevo_prot = case p of
					"IPv4" -> R4
					"IPv6" -> R6
					"Dual" -> Rd
					"IPv4 con TD" -> R4_td
					"Dual con TD" -> Rd_td
					"Dual con TP" -> Rd_tp
					"Dual con TP-TD" -> Rd_tptd
					_ -> error "Selección incorrecta"
				
				let nueva_topo = actualizarNodos
					(topologia conf')
					(IntSet.elems $ lista_ids $ sel_nodos conf')
					(\n -> n {protocolo = nuevo_prot})  
				modifyIORef conf $ \c -> c {
					modificado = True,
					topologia = nueva_topo}
				actualizarVentana window canvas conf

	---------------------------------------------------------------------------
	-- Selección de latencia.
	---------------------------------------------------------------------------
	
	sel_latencia <- xmlGetWidget xml castToSpinButton "sel_latencia"
	sel_latencia `onValueSpinned` do
		conf' <- readIORef conf
		val <- spinButtonGetValue sel_latencia
		let nueva_topo = actualizarArcos
			(topologia conf')
			(Set.elems $ sel_arcos conf')
			(\a -> a {latencia = val})
		modifyIORef conf $ \c -> c {
			modificado = True,
			topologia = nueva_topo }
		actualizarVentana window canvas conf
		
	sel_bandwidth <- xmlGetWidget xml castToSpinButton "sel_bandwidth"
	sel_bandwidth `onValueSpinned` do
		conf' <- readIORef conf
		val <- spinButtonGetValue sel_bandwidth
		let nueva_topo = actualizarArcos
			(topologia conf')
			(Set.elems $ sel_arcos conf')
			(\a -> a {anchoBanda = val})
		modifyIORef conf $ \c -> c {
			modificado = True,
			topologia = nueva_topo }
		actualizarVentana window canvas conf
		
	sel_metrica <- xmlGetWidget xml castToSpinButton "sel_metrica"
	sel_metrica `onValueSpinned` do
		conf' <- readIORef conf
		val <- spinButtonGetValue sel_metrica
		let nueva_topo = actualizarArcos
			(topologia conf')
			(Set.elems $ sel_arcos conf')
			(\a -> a {metrica = val})
		modifyIORef conf $ \c -> c {
			modificado = True,
			topologia = nueva_topo }
		actualizarVentana window canvas conf
	
	
	---------------------------------------------------------------------------
	-- Ejecución del programa.
	---------------------------------------------------------------------------
	
	-- Muestra la interfaz.
	widgetShowAll window
	
	-- Redimensiona el área de dibujo para que quepa todo el grafo.
	widgetSetSizeRequest canvas 10000 10000
	actualizarVentana window canvas conf
	centrarGrafo scroll conf

	-- Ejecuta la interfaz.
	mainGUI
