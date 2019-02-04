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
import Graphics.UI.Gtk



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
	dlg_guardar <- messageDialogNew
		Nothing
		[]
		MessageQuestion
		ButtonsNone
		"El fichero ha sido modificado. ¿Desea guardarlo antes de cerrar?"
	
	-- Añade los botones.
	dialogAddButton dlg_guardar "Guardar" ResponseYes
	dialogAddButton dlg_guardar "No guardar" ResponseNo
	dialogAddButton dlg_guardar "Cancelar" ResponseCancel
 
	-- Ejecuta el diálogo.
	widgetShow dlg_guardar
	response <- dialogRun dlg_guardar
	let ok = case response of
		ResponseYes -> Guardar
		ResponseNo -> Cerrar
		_ -> Cancelar
	
	-- Destruye el diálogo.
	widgetDestroy dlg_guardar
	return ok
