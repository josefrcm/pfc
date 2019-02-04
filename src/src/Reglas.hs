-------------------------------------------------------------------------------
-- Autor: José Franco Campos
-- 
-- Generación, análisis y optimización de escenarios de migración
-- de redes Ipv4/IPv6 mediante programación funcional.
-- 
-- Diciembre 2008
--
--
-- Descripción: reexporta los submódulos más importantes del módulo Reglas,
--              para poder ser usados desde otras partes del programa.
-------------------------------------------------------------------------------

module Reglas (
	module Reglas.Solucion,
	module Reglas.Test,
	module Reglas.Tipos
	) where


-- Módulos exportados
import Reglas.Solucion
import Reglas.Test
import Reglas.Tipos
