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
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
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
	ui <- xmlNew interfaz
	let xml = case ui of
		Just i -> i
		Nothing -> error $ "No se puede cargar el fichero \"" ++ interfaz ++ "\""
	
	-- Widgets que vamos a usar.
	dialogo  <- xmlGetWidget xml castToDialog "dialog1"
	aceptar  <- xmlGetWidget xml castToButton "btn_aceptar"
	cancelar <- xmlGetWidget xml castToButton "btn_cancelar"

	-- Widgets de costes por latencia.
	latencia_dual   <- xmlGetWidget xml castToSpinButton "latencia_dual"
	latencia_mapped <- xmlGetWidget xml castToSpinButton "latencia_mapped"
	latencia_broker <- xmlGetWidget xml castToSpinButton "latencia_broker"
	latencia_dstm   <- xmlGetWidget xml castToSpinButton "latencia_dstm"
	latencia_isatap <- xmlGetWidget xml castToSpinButton "latencia_isatap"
	latencia_6to4   <- xmlGetWidget xml castToSpinButton "latencia_6to4"
	latencia_teredo <- xmlGetWidget xml castToSpinButton "latencia_teredo"
	latencia_auto   <- xmlGetWidget xml castToSpinButton "latencia_auto"
	latencia_nat    <- xmlGetWidget xml castToSpinButton "latencia_nat"
	latencia_siit   <- xmlGetWidget xml castToSpinButton "latencia_siit"
	latencia_socks  <- xmlGetWidget xml castToSpinButton "latencia_socks"
	latencia_trt    <- xmlGetWidget xml castToSpinButton "latencia_trt"
	
	-- Widgets de costes por ancho de banda.
	bandwidth_dual   <- xmlGetWidget xml castToSpinButton "bandwidth_dual"
	bandwidth_mapped <- xmlGetWidget xml castToSpinButton "bandwidth_mapped"
	bandwidth_broker <- xmlGetWidget xml castToSpinButton "bandwidth_broker"
	bandwidth_dstm   <- xmlGetWidget xml castToSpinButton "bandwidth_dstm"
	bandwidth_isatap <- xmlGetWidget xml castToSpinButton "bandwidth_isatap"
	bandwidth_6to4   <- xmlGetWidget xml castToSpinButton "bandwidth_6to4"
	bandwidth_teredo <- xmlGetWidget xml castToSpinButton "bandwidth_teredo"
	bandwidth_auto   <- xmlGetWidget xml castToSpinButton "bandwidth_auto"
	bandwidth_nat    <- xmlGetWidget xml castToSpinButton "bandwidth_nat"
	bandwidth_siit   <- xmlGetWidget xml castToSpinButton "bandwidth_siit"
	bandwidth_socks  <- xmlGetWidget xml castToSpinButton "bandwidth_socks"
	bandwidth_trt    <- xmlGetWidget xml castToSpinButton "bandwidth_trt"
	
	-- Widgets de costes por métrica.
	metrica_dual   <- xmlGetWidget xml castToSpinButton "metrica_dual"
	metrica_mapped <- xmlGetWidget xml castToSpinButton "metrica_mapped"
	metrica_broker <- xmlGetWidget xml castToSpinButton "metrica_broker"
	metrica_dstm   <- xmlGetWidget xml castToSpinButton "metrica_dstm"
	metrica_isatap <- xmlGetWidget xml castToSpinButton "metrica_isatap"
	metrica_6to4   <- xmlGetWidget xml castToSpinButton "metrica_6to4"
	metrica_teredo <- xmlGetWidget xml castToSpinButton "metrica_teredo"
	metrica_auto   <- xmlGetWidget xml castToSpinButton "metrica_auto"
	metrica_nat    <- xmlGetWidget xml castToSpinButton "metrica_nat"
	metrica_siit   <- xmlGetWidget xml castToSpinButton "metrica_siit"
	metrica_socks  <- xmlGetWidget xml castToSpinButton "metrica_socks"
	metrica_trt    <- xmlGetWidget xml castToSpinButton "metrica_trt"

	-- Manejadores de eventos para los botones.
	aceptar `onClicked` do
		dialogResponse dialogo ResponseAccept
	
	cancelar `onClicked` do
		dialogResponse dialogo ResponseCancel

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
	widgetShow dialogo
	response <- dialogRun dialogo
	
	-- Procesa la respuesta.
	val <- case response of
		ResponseAccept -> do
			c1' <- leerValores btn_latencia
			c2' <- leerValores btn_bandwidth
			c3' <- leerValores btn_metrica
			return (Just (c1', c2', c3'))
			
		_ -> return Nothing
	
	-- Devuelve el resultado.
	widgetDestroy dialogo
	return val



-------------------------------------------------------------------------------
-- Funciones auxiliares.
-------------------------------------------------------------------------------

-- Escribe el contenido de los botones de una pestaña.
escribirValores :: Coste -> [(Mecanismo, SpinButton)] -> IO ()
escribirValores coste botones = forM_ botones $ \(m,b) -> do
	spinButtonSetValue b (findWithDefault 0.0 m coste)


-- Lee el contenido de los botones de una pestaña.
leerValores :: [(Mecanismo, SpinButton)] -> IO Coste
leerValores botones = do
	valores <- forM botones $ \(_,b) -> spinButtonGetValue b
	let mecanismos = Prelude.map (\(m,_) -> m) botones
	let costes = fromList (zip mecanismos valores)
	return costes
