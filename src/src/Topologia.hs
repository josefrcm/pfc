-------------------------------------------------------------------------------
-- Autor: José Franco Campos
-- 
-- Generación, análisis y optimización de escenarios de migración
-- de redes Ipv4/IPv6 mediante programación funcional.
-- 
-- Diciembre 2008
--
--
-- Descripción: reexporta los submódulos más importantes del módulo Topologia,
--              para poder ser usados desde otras partes del programa.
-------------------------------------------------------------------------------

module Topologia (
	module Topologia.Grafo,
	module Topologia.GML,
	module Topologia.Parser,
	module Topologia.Rutas,
	module Topologia.Estadisticas,
	module Topologia.Test,
	module Topologia.Tipos
	) where


-- Módulos exportados
import Topologia.Grafo
import Topologia.GML
import Topologia.Parser
import Topologia.Rutas
import Topologia.Estadisticas
import Topologia.Test
import Topologia.Tipos
