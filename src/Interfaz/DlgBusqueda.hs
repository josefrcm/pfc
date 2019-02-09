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
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Builder as GtkBuilder
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
	ui <- GtkBuilder.builderNew
	GtkBuilder.builderAddFromFile ui interfaz
-- 	ui <- xmlNew interfaz
-- 	let xml = case ui of
-- 		Just i -> i
-- 		Nothing -> error $ "No se puede cargar el fichero \"" ++ interfaz ++ "\""
	
	-- Widgets que vamos a usar.
	dialogo  <- GtkBuilder.builderGetObject ui Gtk.castToDialog "dialog1"
	aceptar  <- GtkBuilder.builderGetObject ui Gtk.castToButton "btn_aceptar"
	cancelar <- GtkBuilder.builderGetObject ui Gtk.castToButton "btn_cancelar"

	btn_nodo1       <- GtkBuilder.builderGetObject ui Gtk.castToSpinButton "nodo1"
	btn_aplicacion1 <- GtkBuilder.builderGetObject ui Gtk.castToComboBox "aplicacion1"
	btn_protocolo1  <- GtkBuilder.builderGetObject ui Gtk.castToComboBox "protocolo1"
	btn_nodo2       <- GtkBuilder.builderGetObject ui Gtk.castToSpinButton "nodo2"
	btn_aplicacion2 <- GtkBuilder.builderGetObject ui Gtk.castToComboBox "aplicacion2"
	btn_protocolo2  <- GtkBuilder.builderGetObject ui Gtk.castToComboBox "protocolo2"
	lim_longitud    <- GtkBuilder.builderGetObject ui Gtk.castToSpinButton "lim_longitud"
	opt_longitud    <- GtkBuilder.builderGetObject ui Gtk.castToCheckButton "opt_longitud"
	opt_adyacentes  <- GtkBuilder.builderGetObject ui Gtk.castToCheckButton "opt_adyacentes"
	opt_bucles      <- GtkBuilder.builderGetObject ui Gtk.castToCheckButton "opt_bucles"

	-- Manejadores de eventos para los botones
	Gtk.onClicked aceptar $ do
		Gtk.dialogResponse dialogo Gtk.ResponseAccept
	
	Gtk.onClicked cancelar $ do
		Gtk.dialogResponse dialogo Gtk.ResponseCancel

	-- Establece el rango de la selección de nodo, para evitar que el
	-- usuario elija un nodo inexistente.
	Gtk.spinButtonSetRange btn_nodo1 (fromIntegral rmin) (fromIntegral rmax)
	Gtk.spinButtonSetRange btn_nodo2 (fromIntegral rmin) (fromIntegral rmax)
	Gtk.spinButtonSetRange lim_longitud 0.0 1000.0
	
	-- Establece los parámetros iniciales.
	case params of
		Nothing -> do
			-- Parámetros predeterminados.
			Gtk.comboBoxSetActive btn_aplicacion1 0
			Gtk.comboBoxSetActive btn_aplicacion2 0
			Gtk.comboBoxSetActive btn_protocolo1 0
			Gtk.comboBoxSetActive btn_protocolo2 0
			
		Just op -> do
			-- Parámetros del nodo de origen.
			Gtk.spinButtonSetValue btn_nodo1 $ fromIntegral (nodo1 op)
			Gtk.comboBoxSetActive btn_aplicacion1 $ fromEnum (aplicacion1 op)
			Gtk.comboBoxSetActive btn_protocolo1 $ fromEnum (protocolo1 op)
		
			-- Parámetros del nodo de destino.
			Gtk.spinButtonSetValue btn_nodo2 $ fromIntegral (nodo2 op)
			Gtk.comboBoxSetActive btn_aplicacion2 $ fromEnum (aplicacion2 op)
			Gtk.comboBoxSetActive btn_protocolo2 $ fromEnum (protocolo2 op)
			
			-- Criterios de poda.
			case longitud op of
				Nothing -> do
					Gtk.toggleButtonSetActive opt_longitud False
					Gtk.spinButtonSetValue lim_longitud 0.0
				Just lng -> do
					Gtk.toggleButtonSetActive opt_longitud True
					Gtk.spinButtonSetValue lim_longitud $ fromIntegral lng
			Gtk.toggleButtonSetActive opt_adyacentes (forzar_ady op)
			Gtk.toggleButtonSetActive opt_bucles (elim_bucles op)
	
	-- Espera a que el diálogo devuelva la respuesta
	Gtk.widgetShow dialogo
	response <- Gtk.dialogRun dialogo
	
	-- Procesa la respuesta
	val <- case response of
		Gtk.ResponseAccept -> do
			-- Lee los parámetros del nodo de origen.
			src' <- Gtk.spinButtonGetValueAsInt btn_nodo1
			a1'  <- Gtk.comboBoxGetActive btn_aplicacion1
			p1'  <- Gtk.comboBoxGetActive btn_protocolo1
			
			-- Lee los parámetros del nodo de destino.
			dst' <- Gtk.spinButtonGetValueAsInt btn_nodo2
			a2'  <- Gtk.comboBoxGetActive btn_aplicacion2
			p2'  <- Gtk.comboBoxGetActive btn_protocolo2
			
			-- Lee los criterios de poda.
			tmp <- Gtk.toggleButtonGetActive opt_longitud
			lng <- case tmp of
				True -> return Just `ap` Gtk.spinButtonGetValueAsInt lim_longitud
				False -> return Nothing
			ady <- Gtk.toggleButtonGetActive opt_adyacentes
			bcl <- Gtk.toggleButtonGetActive opt_bucles
			
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
	Gtk.widgetDestroy dialogo
	return val
