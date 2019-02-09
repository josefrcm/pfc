-------------------------------------------------------------------------------
-- Autor: José Franco Campos
--
-- Generación, análisis y optimización de escenarios de migración
-- de redes Ipv4/IPv6 mediante programación funcional.
--
-- Diciembre 2008
--
--
-- Descripción: ejecución de la batería de pruebas.
-------------------------------------------------------------------------------

-- Módulos del programa
import           Lib

-- Ejecución de la batería de pruebas.
main :: IO ()
main = do
    probarTopologia
    probarReglas
