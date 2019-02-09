-------------------------------------------------------------------------------
-- Autor: José Franco Campos
-- 
-- Generación, análisis y optimización de escenarios de migración
-- de redes Ipv4/IPv6 mediante programación funcional.
-- 
-- Diciembre 2008
--
--
-- Descripción: implementación del diálogo de confirmación de cierre.
-------------------------------------------------------------------------------

module Interfaz.DlgConfirmar where


-- Módulos externos
import qualified Graphics.UI.Gtk as Gtk



------------------------------------------------------------------------------
-- Implementación del diálogo.
------------------------------------------------------------------------------

-- Posibles respuestas del usuario.
data Confirmacion = Guardar | Cerrar | Cancelar


-- Este diálogo se ejecuta cuando el usuario quiere cerrar un fichero que ha
-- sido modificado sin haberse guardado aún. Pide confirmación al usuario,
-- y devuelve el resultado.
crearConfirmar :: IO Confirmacion
crearConfirmar = do
	-- Crea el diálogo.
	dlg_guardar <- Gtk.messageDialogNew
		Nothing
		[]
		Gtk.MessageQuestion
		Gtk.ButtonsNone
		"El fichero ha sido modificado. ¿Desea guardarlo antes de cerrar?"
	
	-- Añade los botones.
	Gtk.dialogAddButton dlg_guardar "Guardar" Gtk.ResponseYes
	Gtk.dialogAddButton dlg_guardar "No guardar" Gtk.ResponseNo
	Gtk.dialogAddButton dlg_guardar "Cancelar" Gtk.ResponseCancel
 
	-- Ejecuta el diálogo.
	Gtk.widgetShow dlg_guardar
	response <- Gtk.dialogRun dlg_guardar
	let ok = case response of
		Gtk.ResponseYes -> Guardar
		Gtk.ResponseNo -> Cerrar
		_ -> Cancelar
	
	-- Destruye el diálogo.
	Gtk.widgetDestroy dlg_guardar
	return ok
