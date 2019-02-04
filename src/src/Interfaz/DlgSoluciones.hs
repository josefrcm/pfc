-------------------------------------------------------------------------------
-- Autor: José Franco Campos
-- 
-- Generación, análisis y optimización de escenarios de migración
-- de redes Ipv4/IPv6 mediante programación funcional.
-- 
-- Diciembre 2008
--
--
-- Descripción: implementación del diálogo de progreso durante el proceso de
--              búsqueda de soluciones.
-------------------------------------------------------------------------------

module Interfaz.DlgSoluciones where

-- Módulos externos
import Graphics.UI.Gtk
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Data.IORef
import Data.List

-- Módulos del programa
import Reglas
import Interfaz.Tipos


------------------------------------------------------------------------------
-- Implementación del diálogo.
------------------------------------------------------------------------------

-- Ejecuta el proceso de búsqueda de soluciones. Toma como entrada las opciones
-- de búsqueda y la configuración actual del programa, de la que extrae la
-- tabla de costes y la topología. Devuelve una lista de soluciones, que puede
-- ser vacía si el usuario cancela la operación o no existen soluciones.
crearSoluciones :: Opciones -> IORef Configuracion -> IO [Solucion]
crearSoluciones op conf = do
	-- Lee la configuración.
	conf' <- readIORef conf
	
	-- Crea el diálogo.
	dlg_progreso <- messageDialogNew
		Nothing
		[]
		MessageInfo
		ButtonsCancel
		"Calculando rutas..."
	
	-- Lanza un hilo que genera todas las soluciones.
	sols_var <- newEmptyMVar
	thread <- forkIO $ (do
		-- Busca las soluciones, y al terminar cierra el diálogo.
		let sols = sortBy comparar_lat $ buscarSoluciones op (costes conf') (topologia conf')
		putMVar sols_var sols
		dialogResponse dlg_progreso ResponseAccept
		)
		-- Si el usuario cancela antes de que termine, devuelve una lista vacía.
		`Control.Exception.catch` (\_ -> putMVar sols_var [])
	
	-- Ejecuta el diálogo, esperando a que el usuario pulse el botón de
	-- cancelar, o termine el proceso de búsqueda, lo que primero suceda.
	widgetShow dlg_progreso
	response <- dialogRun dlg_progreso
	sols <- case response of
		ResponseCancel -> do
			throwTo thread undefined
			return []
		ResponseAccept -> do
			sols <- takeMVar sols_var
			return sols
		_ -> return []
	
	-- Devuelve la solución.
	widgetDestroy dlg_progreso
	return sols
