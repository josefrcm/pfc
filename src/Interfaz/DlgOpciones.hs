-------------------------------------------------------------------------------
-- Autor: José Franco Campos
-- 
-- Generación, análisis y optimización de escenarios de migración
-- de redes Ipv4/IPv6 mediante programación funcional.
-- 
-- Diciembre 2008
--
--
-- Descripción: implementación del diálogo de configuración de costes.
-------------------------------------------------------------------------------

module Interfaz.DlgOpciones (crearOpciones) where


-- Módulos externos
import Control.Monad
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Builder as GtkBuilder
import Data.Map


-- Módulos del programa
import Reglas


------------------------------------------------------------------------------
-- Implementación del diálogo.
------------------------------------------------------------------------------

-- Ruta al fichero con la definición de la interfaz.
interfaz :: FilePath
interfaz = "res/configuracion.glade"


-- Ejecuta el diálogo. Toma como entrada los costes actuales. Devuelve Nothing
-- si el usuario canceló la operación, y Just x si pulsó el botón "Aceptar",
-- donde x son los nuevos costes.
crearOpciones :: (Coste, Coste, Coste) ->  IO (Maybe (Coste, Coste, Coste))
crearOpciones (c1, c2, c3) = do
	-- Carga la interfaz gráfica.
	ui <- GtkBuilder.builderNew
	GtkBuilder.builderAddFromFile ui interfaz
-- 	let xml = case ui of
-- 		Just i -> i
-- 		Nothing -> error $ "No se puede cargar el fichero \"" ++ interfaz ++ "\""
	
	-- Widgets que vamos a usar.
	dialogo  <- GtkBuilder.builderGetObject ui Gtk.castToDialog "dialog1"
	aceptar  <- GtkBuilder.builderGetObject ui Gtk.castToButton "btn_aceptar"
	cancelar <- GtkBuilder.builderGetObject ui Gtk.castToButton "btn_cancelar"

	-- Widgets de costes por latencia.
	latencia_dual   <- GtkBuilder.builderGetObject ui Gtk.castToSpinButton "latencia_dual"
	latencia_mapped <- GtkBuilder.builderGetObject ui Gtk.castToSpinButton "latencia_mapped"
	latencia_broker <- GtkBuilder.builderGetObject ui Gtk.castToSpinButton "latencia_broker"
	latencia_dstm   <- GtkBuilder.builderGetObject ui Gtk.castToSpinButton "latencia_dstm"
	latencia_isatap <- GtkBuilder.builderGetObject ui Gtk.castToSpinButton "latencia_isatap"
	latencia_6to4   <- GtkBuilder.builderGetObject ui Gtk.castToSpinButton "latencia_6to4"
	latencia_teredo <- GtkBuilder.builderGetObject ui Gtk.castToSpinButton "latencia_teredo"
	latencia_auto   <- GtkBuilder.builderGetObject ui Gtk.castToSpinButton "latencia_auto"
	latencia_nat    <- GtkBuilder.builderGetObject ui Gtk.castToSpinButton "latencia_nat"
	latencia_siit   <- GtkBuilder.builderGetObject ui Gtk.castToSpinButton "latencia_siit"
	latencia_socks  <- GtkBuilder.builderGetObject ui Gtk.castToSpinButton "latencia_socks"
	latencia_trt    <- GtkBuilder.builderGetObject ui Gtk.castToSpinButton "latencia_trt"
	
	-- Widgets de costes por ancho de banda.
	bandwidth_dual   <- GtkBuilder.builderGetObject ui Gtk.castToSpinButton "bandwidth_dual"
	bandwidth_mapped <- GtkBuilder.builderGetObject ui Gtk.castToSpinButton "bandwidth_mapped"
	bandwidth_broker <- GtkBuilder.builderGetObject ui Gtk.castToSpinButton "bandwidth_broker"
	bandwidth_dstm   <- GtkBuilder.builderGetObject ui Gtk.castToSpinButton "bandwidth_dstm"
	bandwidth_isatap <- GtkBuilder.builderGetObject ui Gtk.castToSpinButton "bandwidth_isatap"
	bandwidth_6to4   <- GtkBuilder.builderGetObject ui Gtk.castToSpinButton "bandwidth_6to4"
	bandwidth_teredo <- GtkBuilder.builderGetObject ui Gtk.castToSpinButton "bandwidth_teredo"
	bandwidth_auto   <- GtkBuilder.builderGetObject ui Gtk.castToSpinButton "bandwidth_auto"
	bandwidth_nat    <- GtkBuilder.builderGetObject ui Gtk.castToSpinButton "bandwidth_nat"
	bandwidth_siit   <- GtkBuilder.builderGetObject ui Gtk.castToSpinButton "bandwidth_siit"
	bandwidth_socks  <- GtkBuilder.builderGetObject ui Gtk.castToSpinButton "bandwidth_socks"
	bandwidth_trt    <- GtkBuilder.builderGetObject ui Gtk.castToSpinButton "bandwidth_trt"
	
	-- Widgets de costes por métrica.
	metrica_dual   <- GtkBuilder.builderGetObject ui Gtk.castToSpinButton "metrica_dual"
	metrica_mapped <- GtkBuilder.builderGetObject ui Gtk.castToSpinButton "metrica_mapped"
	metrica_broker <- GtkBuilder.builderGetObject ui Gtk.castToSpinButton "metrica_broker"
	metrica_dstm   <- GtkBuilder.builderGetObject ui Gtk.castToSpinButton "metrica_dstm"
	metrica_isatap <- GtkBuilder.builderGetObject ui Gtk.castToSpinButton "metrica_isatap"
	metrica_6to4   <- GtkBuilder.builderGetObject ui Gtk.castToSpinButton "metrica_6to4"
	metrica_teredo <- GtkBuilder.builderGetObject ui Gtk.castToSpinButton "metrica_teredo"
	metrica_auto   <- GtkBuilder.builderGetObject ui Gtk.castToSpinButton "metrica_auto"
	metrica_nat    <- GtkBuilder.builderGetObject ui Gtk.castToSpinButton "metrica_nat"
	metrica_siit   <- GtkBuilder.builderGetObject ui Gtk.castToSpinButton "metrica_siit"
	metrica_socks  <- GtkBuilder.builderGetObject ui Gtk.castToSpinButton "metrica_socks"
	metrica_trt    <- GtkBuilder.builderGetObject ui Gtk.castToSpinButton "metrica_trt"

	-- Manejadores de eventos para los botones.
	Gtk.onClicked aceptar $ do
		Gtk.dialogResponse dialogo Gtk.ResponseAccept
	
	Gtk.onClicked cancelar $ do
		Gtk.dialogResponse dialogo Gtk.ResponseCancel

	-- Correspondencias entre mecanismos y botones de latencia.
	let btn_latencia = [
		(MT_dual_stack, latencia_dual),   
		(MT_mapped,     latencia_mapped), 
		(MT_broker,     latencia_broker ),
		(MT_dstm,       latencia_dstm),
		(MT_isatap,     latencia_isatap),
		(MT_6to4,       latencia_6to4),
		(MT_teredo,     latencia_teredo),
		(MT_auto,       latencia_auto),
		(MT_nat_pt,     latencia_nat), 
		(MT_siit,       latencia_siit),  
		(MT_socks,      latencia_socks), 
		(MT_trt,        latencia_trt)]
	
	-- Correspondencias entre mecanismos y botones de ancho de banda.
	let btn_bandwidth = [
		(MT_dual_stack, bandwidth_dual),   
		(MT_mapped,     bandwidth_mapped), 
		(MT_broker,     bandwidth_broker ),
		(MT_dstm,       bandwidth_dstm),
		(MT_isatap,     bandwidth_isatap),
		(MT_6to4,       bandwidth_6to4),
		(MT_teredo,     bandwidth_teredo),
		(MT_auto,       bandwidth_auto),
		(MT_nat_pt,     bandwidth_nat), 
		(MT_siit,       bandwidth_siit),  
		(MT_socks,      bandwidth_socks), 
		(MT_trt,        bandwidth_trt)]
	
	-- Correspondencias entre mecanismos y botones de métrica.
	let btn_metrica = [
		(MT_dual_stack, metrica_dual),   
		(MT_mapped,     metrica_mapped), 
		(MT_broker,     metrica_broker ),
		(MT_dstm,       metrica_dstm),
		(MT_isatap,     metrica_isatap),
		(MT_6to4,       metrica_6to4),
		(MT_teredo,     metrica_teredo),
		(MT_auto,       metrica_auto),
		(MT_nat_pt,     metrica_nat), 
		(MT_siit,       metrica_siit),  
		(MT_socks,      metrica_socks), 
		(MT_trt,        metrica_trt)]
	
	-- Establece los costes iniciales.
	escribirValores c1 btn_latencia
	escribirValores c2 btn_bandwidth
	escribirValores c3 btn_metrica
	
	-- Espera a que el diálogo devuelva la respuesta.
	Gtk.widgetShow dialogo
	response <- Gtk.dialogRun dialogo
	
	-- Procesa la respuesta.
	val <- case response of
		Gtk.ResponseAccept -> do
			c1' <- leerValores btn_latencia
			c2' <- leerValores btn_bandwidth
			c3' <- leerValores btn_metrica
			return (Just (c1', c2', c3'))
			
		_ -> return Nothing
	
	-- Devuelve el resultado.
	Gtk.widgetDestroy dialogo
	return val



-------------------------------------------------------------------------------
-- Funciones auxiliares.
-------------------------------------------------------------------------------

-- Escribe el contenido de los botones de una pestaña.
escribirValores :: Coste -> [(Mecanismo, Gtk.SpinButton)] -> IO ()
escribirValores coste botones = forM_ botones $ \(m,b) -> do
	Gtk.spinButtonSetValue b (findWithDefault 0.0 m coste)


-- Lee el contenido de los botones de una pestaña.
leerValores :: [(Mecanismo, Gtk.SpinButton)] -> IO Coste
leerValores botones = do
	valores <- forM botones $ \(_,b) -> Gtk.spinButtonGetValue b
	let mecanismos = Prelude.map (\(m,_) -> m) botones
	let costes = fromList (zip mecanismos valores)
	return costes
