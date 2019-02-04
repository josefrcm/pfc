-------------------------------------------------------------------------------
-- Autor: José Franco Campos
-- 
-- Generación, análisis y optimización de escenarios de migración
-- de redes Ipv4/IPv6 mediante programación funcional.
-- 
-- Diciembre 2008
--
--
-- Descripción: implementación del diálogo de búsqueda de soluciones.
-------------------------------------------------------------------------------

module Interfaz.DlgBusqueda (crearBusqueda) where


-- Módulos externos
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Control.Monad

-- Módulos del programa
import Reglas



------------------------------------------------------------------------------
-- Implementación del diálogo.
------------------------------------------------------------------------------

-- Ruta al fichero con la definición de la interfaz.
interfaz :: FilePath
interfaz = "res/busqueda.glade"


-- Crea el diálogo de búsqueda. Los argumentos son:
--	* Identificador de nodos mínimo.
--	* Identificador de nodos máximo.
--	* Opciones de búsqueda (opcional). Si no se pasa nada, se inicializan
--	  a los valores predeterminados.
-- Devuelve Nothing si el usuario canceló la operación, y Just x si pulsó el
-- botón "Aceptar", siendo 'x' las nuevas opciones.
crearBusqueda :: Int -> Int -> Maybe Opciones -> IO (Maybe Opciones)
crearBusqueda rmin rmax params = do
	-- Carga la interfaz gráfica.
	ui <- xmlNew interfaz
	let xml = case ui of
		Just i -> i
		Nothing -> error $ "No se puede cargar el fichero \"" ++ interfaz ++ "\""
	
	-- Widgets que vamos a usar.
	dialogo  <- xmlGetWidget xml castToDialog "dialog1"
	aceptar  <- xmlGetWidget xml castToButton "btn_aceptar"
	cancelar <- xmlGetWidget xml castToButton "btn_cancelar"

	btn_nodo1       <- xmlGetWidget xml castToSpinButton "nodo1"
	btn_aplicacion1 <- xmlGetWidget xml castToComboBox "aplicacion1"
	btn_protocolo1  <- xmlGetWidget xml castToComboBox "protocolo1"
	btn_nodo2       <- xmlGetWidget xml castToSpinButton "nodo2"
	btn_aplicacion2 <- xmlGetWidget xml castToComboBox "aplicacion2"
	btn_protocolo2  <- xmlGetWidget xml castToComboBox "protocolo2"
	lim_longitud    <- xmlGetWidget xml castToSpinButton "lim_longitud"
	opt_longitud    <- xmlGetWidget xml castToCheckButton "opt_longitud"
	opt_adyacentes  <- xmlGetWidget xml castToCheckButton "opt_adyacentes"
	opt_bucles      <- xmlGetWidget xml castToCheckButton "opt_bucles"

	-- Manejadores de eventos para los botones
	aceptar `onClicked` do
		dialogResponse dialogo ResponseAccept
	
	cancelar `onClicked` do
		dialogResponse dialogo ResponseCancel

	-- Establece el rango de la selección de nodo, para evitar que el
	-- usuario elija un nodo inexistente.
	spinButtonSetRange btn_nodo1 (fromIntegral rmin) (fromIntegral rmax)
	spinButtonSetRange btn_nodo2 (fromIntegral rmin) (fromIntegral rmax)
	spinButtonSetRange lim_longitud 0.0 1000.0
	
	-- Establece los parámetros iniciales.
	case params of
		Nothing -> do
			-- Parámetros predeterminados.
			comboBoxSetActive btn_aplicacion1 0
			comboBoxSetActive btn_aplicacion2 0
			comboBoxSetActive btn_protocolo1 0
			comboBoxSetActive btn_protocolo2 0
			
		Just op -> do
			-- Parámetros del nodo de origen.
			spinButtonSetValue btn_nodo1 $ fromIntegral (nodo1 op)
			comboBoxSetActive btn_aplicacion1 $ fromEnum (aplicacion1 op)
			comboBoxSetActive btn_protocolo1 $ fromEnum (protocolo1 op)
		
			-- Parámetros del nodo de destino.
			spinButtonSetValue btn_nodo2 $ fromIntegral (nodo2 op)
			comboBoxSetActive btn_aplicacion2 $ fromEnum (aplicacion2 op)
			comboBoxSetActive btn_protocolo2 $ fromEnum (protocolo2 op)
			
			-- Criterios de poda.
			case longitud op of
				Nothing -> do
					toggleButtonSetActive opt_longitud False
					spinButtonSetValue lim_longitud 0.0
				Just lng -> do
					toggleButtonSetActive opt_longitud True
					spinButtonSetValue lim_longitud $ fromIntegral lng
			toggleButtonSetActive opt_adyacentes (forzar_ady op)
			toggleButtonSetActive opt_bucles (elim_bucles op)
	
	-- Espera a que el diálogo devuelva la respuesta
	widgetShow dialogo
	response <- dialogRun dialogo
	
	-- Procesa la respuesta
	val <- case response of
		ResponseAccept -> do
			-- Lee los parámetros del nodo de origen.
			src' <- spinButtonGetValueAsInt btn_nodo1
			Just a1'  <- comboBoxGetActive btn_aplicacion1
			Just p1'  <- comboBoxGetActive btn_protocolo1
			
			-- Lee los parámetros del nodo de destino.
			dst' <- spinButtonGetValueAsInt btn_nodo2
			Just a2'  <- comboBoxGetActive btn_aplicacion2
			Just p2'  <- comboBoxGetActive btn_protocolo2
			
			-- Lee los criterios de poda.
			tmp <- toggleButtonGetActive opt_longitud
			lng <- case tmp of
				True -> return Just `ap` spinButtonGetValueAsInt lim_longitud
				False -> return Nothing
			ady <- toggleButtonGetActive opt_adyacentes
			bcl <- toggleButtonGetActive opt_bucles
			
			-- Crea la nueva estructura de opciones.
			let op' = Opciones {
				nodo1 = src',
				nodo2 = dst',
				aplicacion1 = toEnum a1',
				aplicacion2 = toEnum a2',
				protocolo1 = toEnum p1',
				protocolo2 = toEnum p2',
				longitud = lng,
				forzar_ady = ady,
				elim_bucles = bcl }
			return $ Just op'
		_ -> return Nothing
	
	-- Devuelve el resultado
	widgetDestroy dialogo
	return val
