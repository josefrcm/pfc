-------------------------------------------------------------------------------
-- Autor: José Franco Campos
--
-- Generación, análisis y optimización de escenarios de migración
-- de redes Ipv4/IPv6 mediante programación funcional.
--
-- Diciembre 2008
--
--
-- Descripción: reexporta los submódulos más importantes del.
-------------------------------------------------------------------------------
module Lib
    ( module Interfaz
    , module Reglas
    , module Topologia
    ) where

-- Módulos exportados
import           Interfaz
import           Reglas
import           Topologia
