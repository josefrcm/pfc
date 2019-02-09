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
import qualified Graphics.UI.Gtk as Gtk



------------------------------------------------------------------------------
-- Implementación del diálogo.
------------------------------------------------------------------------------

-- Ejecuta el diálogo, devolviendo Nothing si el usuario canceló la operación,
-- y Just f si seleccionó el fichero 'f'.
crearAbrir :: IO (Maybe FilePath)
crearAbrir = do
	-- Crea el diálogo de selección de fichero
	dlg_abrir <- Gtk.fileChooserDialogNew
		(Just "Abrir fichero")
		Nothing
		Gtk.FileChooserActionOpen
		[("Abrir", Gtk.ResponseAccept), ("Cancelar", Gtk.ResponseCancel)]
	
	-- Ejecuta el diálogo
	Gtk.widgetShow dlg_abrir
	response <- Gtk.dialogRun dlg_abrir
	fichero <- case response of
		Gtk.ResponseAccept -> do
			n <- Gtk.fileChooserGetFilename dlg_abrir
			return n
		_ -> return Nothing

	-- Destruye el diálogo
	Gtk.widgetDestroy dlg_abrir
	return fichero
