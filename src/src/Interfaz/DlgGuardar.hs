-------------------------------------------------------------------------------
-- Autor: José Franco Campos
-- 
-- Generación, análisis y optimización de escenarios de migración
-- de redes Ipv4/IPv6 mediante programación funcional.
-- 
-- Diciembre 2008
--
--
-- Descripción: implementación del diálogo para guardar ficheros.
-------------------------------------------------------------------------------

module Interfaz.DlgGuardar where

-- Módulos externos
import Graphics.UI.Gtk



------------------------------------------------------------------------------
-- Implementación del diálogo.
------------------------------------------------------------------------------

-- Ejecuta el diálogo, devolviendo Nothing si el usuario canceló la operación,
-- y Just f si seleccionó el fichero 'f'.
crearGuardar :: IO (Maybe FilePath)
crearGuardar = do
	-- Crea el diálogo de selección de fichero.
	dlg_abrir <- fileChooserDialogNew
		(Just "Guardar fichero")
		Nothing
		FileChooserActionSave
		[("Guardar", ResponseAccept), ("Cancelar", ResponseCancel)]
	fileChooserSetDoOverwriteConfirmation dlg_abrir True
	
	-- Ejecuta el diálogo.
	widgetShow dlg_abrir
	response <- dialogRun dlg_abrir
	fichero <- case response of
		ResponseAccept -> do
			n <- fileChooserGetFilename dlg_abrir
			return n
		_ -> return Nothing

	-- Destruye el diálogo.
	widgetDestroy dlg_abrir
	return fichero
