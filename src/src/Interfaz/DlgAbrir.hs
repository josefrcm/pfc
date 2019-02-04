-------------------------------------------------------------------------------
-- Autor: José Franco Campos
-- 
-- Generación, análisis y optimización de escenarios de migración
-- de redes Ipv4/IPv6 mediante programación funcional.
-- 
-- Diciembre 2008
--
--
-- Descripción: implementación del diálogo de apertura de ficheros.
-------------------------------------------------------------------------------

module Interfaz.DlgAbrir where


-- Módulos externos
import Graphics.UI.Gtk



------------------------------------------------------------------------------
-- Implementación del diálogo.
------------------------------------------------------------------------------

-- Ejecuta el diálogo, devolviendo Nothing si el usuario canceló la operación,
-- y Just f si seleccionó el fichero 'f'.
crearAbrir :: IO (Maybe FilePath)
crearAbrir = do
	-- Crea el diálogo de selección de fichero
	dlg_abrir <- fileChooserDialogNew
		(Just "Abrir fichero")
		Nothing
		FileChooserActionOpen
		[("Abrir", ResponseAccept), ("Cancelar", ResponseCancel)]
	
	-- Ejecuta el diálogo
	widgetShow dlg_abrir
	response <- dialogRun dlg_abrir
	fichero <- case response of
		ResponseAccept -> do
			n <- fileChooserGetFilename dlg_abrir
			return n
		_ -> return Nothing

	-- Destruye el diálogo
	widgetDestroy dlg_abrir
	return fichero
