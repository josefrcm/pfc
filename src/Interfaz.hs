-------------------------------------------------------------------------------
-- Autor: José Franco Campos
--
-- Generación, análisis y optimización de escenarios de migración
-- de redes Ipv4/IPv6 mediante programación funcional.
--
-- Diciembre 2008
--
--
-- Descripción: reexporta los submódulos más importantes del módulo Interfaz,
--              para poder ser usados desde otras partes del programa.
-------------------------------------------------------------------------------
module Interfaz
    ( module Interfaz.Editor
    , module Interfaz.Tipos
    ) where

-- Módulos exportados
import           Interfaz.Editor
import           Interfaz.Tipos
