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
import qualified Graphics.UI.Gtk as Gtk



------------------------------------------------------------------------------
-- Implementación del diálogo.
------------------------------------------------------------------------------

-- Ejecuta el diálogo, devolviendo Nothing si el usuario canceló la operación,
-- y Just f si seleccionó el fichero 'f'.
crearGuardar :: IO (Maybe FilePath)
crearGuardar = do
	-- Crea el diálogo de selección de fichero.
	dlg_abrir <- Gtk.fileChooserDialogNew
		(Just "Guardar fichero")
		Nothing
		Gtk.FileChooserActionSave
		[("Guardar", Gtk.ResponseAccept), ("Cancelar", Gtk.ResponseCancel)]
	Gtk.fileChooserSetDoOverwriteConfirmation dlg_abrir True
	
	-- Ejecuta el diálogo.
	Gtk.widgetShow dlg_abrir
	response <- Gtk.dialogRun dlg_abrir
	fichero <- case response of
		Gtk.ResponseAccept -> do
			n <- Gtk.fileChooserGetFilename dlg_abrir
			return n
		_ -> return Nothing

	-- Destruye el diálogo.
	Gtk.widgetDestroy dlg_abrir
	return fichero
