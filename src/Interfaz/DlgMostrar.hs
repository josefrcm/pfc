-------------------------------------------------------------------------------
-- Autor: José Franco Campos
-- 
-- Generación, análisis y optimización de escenarios de migración
-- de redes Ipv4/IPv6 mediante programación funcional.
-- 
-- Diciembre 2008
--
--
-- Descripción: implementación del diálogo de detalles de una solución.
-------------------------------------------------------------------------------

module Interfaz.DlgMostrar (
	crearMostrar
	) where

-- Módulos externos
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Builder as GtkBuilder
import Text.Printf

-- Módulos del programa
import Topologia
import Reglas


------------------------------------------------------------------------------
-- Implementación del diálogo.
------------------------------------------------------------------------------

-- Ruta al fichero con la definición de la interfaz.
interfaz :: FilePath
interfaz = "res/solucion.glade"


-- Crea el diálogo que mostrará la solución pasada como entrada.
-- No devuelve nada.
crearMostrar :: Solucion -> IO ()
crearMostrar sol = do
	-- Carga el fichero de interfaz
	ui <- GtkBuilder.builderNew
	GtkBuilder.builderAddFromFile ui interfaz
-- 	ui <- xmlNew interfaz
-- 	let xml = case ui of
-- 		Just i -> i
-- 		Nothing -> error $ "No se puede cargar el fichero \"" ++ interfaz ++ "\""
	
	-- Extrae los widgets.
	dialogo  <- GtkBuilder.builderGetObject ui Gtk.castToDialog "dialog1"
	entry1   <- GtkBuilder.builderGetObject ui Gtk.castToEntry  "entry1"
	entry2   <- GtkBuilder.builderGetObject ui Gtk.castToEntry  "entry2"
	entry3   <- GtkBuilder.builderGetObject ui Gtk.castToEntry  "entry3"
	entry4   <- GtkBuilder.builderGetObject ui Gtk.castToEntry  "entry4"
	textview <- GtkBuilder.builderGetObject ui Gtk.castToTextView "textview1"
	
	-- Añade los parámetros de la solución.
	Gtk.entrySetText entry1 (show (ruta sol))
	Gtk.entrySetText entry2 (show (coste_lat sol))
	Gtk.entrySetText entry3 (show (coste_bw sol))
	Gtk.entrySetText entry4 (show (coste_met sol))
	
	-- Crea el buffer de texto, y le asigna un tipo de letra monoespacio.
	buffer <- Gtk.textBufferNew Nothing
	Gtk.textViewSetBuffer textview buffer
	f <- Gtk.fontDescriptionNew
	Gtk.fontDescriptionSetFamily f "Monospace"
	Gtk.fontDescriptionSetSize f 20
	Gtk.widgetModifyFont textview (Just f)

	-- Añade el texto de la solución.
	Gtk.textBufferInsertAtCursor buffer $ showE (inicial sol) ++ "\n"
	Gtk.textBufferInsertAtCursor buffer $ showS (inicial sol) (lcambios sol) ++ "\n"
	Gtk.textBufferInsertAtCursor buffer $ showE (final sol)
	
	-- Ejecuta el diálogo
	Gtk.widgetShow dialogo
	_ <- Gtk.dialogRun dialogo
	Gtk.widgetDestroy dialogo



-------------------------------------------------------------------------------
-- Caracteres especiales
-------------------------------------------------------------------------------

-- Representación Unicode de los operadores de conexión
unicode :: Conexion -> String
unicode Directa  = "\x2194"    -- ↔
unicode OpD      = "\x2297\&d" -- ⊗
unicode Op4_td   = "\x2299\&4" -- ⊙
unicode OpD_td   = "\x2299\&d" -- ⊙
unicode OpD_tp   = "\x22A0\&d" -- ⊠
unicode OpD_tptd = "\x22A1\&d" -- ⊡

-- Representación Unicode del operador diamante
diamante :: String
diamante = "\x22c4" -- <>



-------------------------------------------------------------------------------
-- Funciones para mostrar los diversos campos de una solución
-------------------------------------------------------------------------------

-- Sólo interesa mostrar algunos de los tipos de cambios, en concreto, los
-- relativos a la creación de túneles y los mecanismos de transición finales.
-- Todo los cambios que aparezcan en esta lista no serán mostrados.
noMostrar :: [TipoCambio]
noMostrar = [
	Nada,
	Formalizar,
	Zona4,
	Zona6,
	ZonaD,
	Operador1,
	Operador2,
	Operador3,
	Operador4,
	Operador5,
	Canonizacion1,
	Canonizacion2,
	Canonizacion3,
	Canonizacion4,
	Canonizacion5,
	Canonizacion6,
	Canonizacion7,
	Canonizacion8 ]
	

-- Traduce los nombres de los mecanismos a algo más inteligible por el usuario.
traducir :: TipoCambio -> String
traducir Zona4          = "Zona IPv4"
traducir Zona6          = "Zona IPv6"
traducir ZonaD          = "Zona dual"
traducir Formalizar     = "Formalizar"
traducir Operador1      = "Operador 1"
traducir Operador2      = "Operador 2"
traducir Operador3      = "Operador 3"
traducir Operador4      = "Operador 4"
traducir Operador5      = "Operador 5"
traducir Canonizacion1  = "Canonización 1"
traducir Canonizacion2  = "Canonización 2"
traducir Canonizacion3  = "Canonización 3"
traducir Canonizacion4  = "Canonización 4"
traducir Canonizacion5  = "Canonización 5"
traducir Canonizacion6  = "Canonización 6"
traducir Canonizacion7  = "Canonización 7"
traducir Canonizacion8  = "Canonización 8"
traducir ZZ_manual_4en6 = "Túnel manual IPv4 en IPv6"
traducir ZZ_manual_6en4 = "Túnel manual IPv6 en IPv4"
traducir ZZ_manual_udp  = "Túnel manual IPv6 en IPv4 UDP"
traducir ZZ_broker_4en6 = "Broker de túneles IPv4 en IPv6"
traducir ZZ_broker_6en4 = "Broker de túneles IPv6 en IPv4"
traducir ZZ_broker_udp  = "Broker de túneles IPv6 en IPv4 UDP"
traducir ZZ_6to4        = "Túnel 6to4"
traducir ZN_manual_4en6 = "Túnel manual IPv4 en IPv6"
traducir ZN_manual_6en4 = "Túnel manual IPv6 en IPv4"
traducir ZN_manual_udp  = "Túnel manual IPv6 en IPv4 UDP"
traducir ZN_broker_4en6 = "Broker de túneles IPv4 en IPv6"
traducir ZN_broker_6en4 = "Broker de túneles IPv6 en IPv4"
traducir ZN_broker_udp  = "Broker de túneles IPv6 en IPv4 UDP"
traducir ZN_6to4        = "Túnel 6to4"
traducir ZN_dstm        = "Túnel DSTM"
traducir ZN_teredo      = "Túnel Teredo"
traducir ZN_isatap      = "Tunel ISATAP"
traducir NN_manual_4en6 = "Túnel manual IPv4 en IPv6"
traducir NN_manual_6en4 = "Túnel manual IPv6 en IPv4"
traducir NN_manual_udp  = "Túnel manual IPv6 en IPv4 UDP"
traducir NN_isatap      = "Túnel ISATAP"
traducir NN_teredo      = "Túnel Teredo"
traducir Dual_stack     = "Instalación de Dual Stack"
traducir Mapped         = "Instalación de Mapped"
traducir NAT_PT         = "Traducción NAT-PT"
traducir SIIT           = "Traducción SIIT"
traducir SOCKS          = "Traducción SOCKS"
traducir TRT            = "Traducción TRT"
traducir _              = "Nada"


-- Muestra la solución.
showS :: Escenario -> [Cambio] -> String
showS escenario cambios = loop cambios (crearExt escenario)
	where
	pos = posiciones escenario
	loop :: [Cambio] -> Extension -> String
	loop [] _ = ""
	loop (c:cs) ext = (showC ext pos c) ++ loop cs (aplicarCambio c ext)


-- Muestra el escenario.
-- Nota: por alguna razón que desconozco, Gtk muestra el espacio detrás
-- del diamante con la mitad de anchura, por lo que hay que usar dos espacios.
showE :: Escenario -> String
showE (a1, n1, red, n2, a2) =
	printf "%s %s  %s " (show a1) diamante (show n1) ++
	(concatMap showP red) ++
	printf "%s %s %s  %s" (unicode Directa) (show n2) diamante (show a2)


-- Muestra una pareja (conexión, router).
showP :: (Conexion, Router) -> String
showP (c, r) = unicode c ++ " " ++ show r ++ " "


-- Muestra un cambio.
showC :: Extension -> ([Int], [Int]) -> Cambio -> String
showC ext (pos_principio, pos_final) cambio
	| elem (tipo cambio) noMostrar = ""
	| otherwise = linea1 ++ linea2
	where
	-- Posición de texto en la que empieza y acaba el cambio.
	(p, f) = expandir ext (inicio cambio, inicio cambio + antes cambio)
	principio = pos_principio !! p
	fin       = pos_final !! f
	
	-- Texto del cambio.
	texto1 = "\x2514" ++ (replicate (fin-principio-2) '\x2500') ++ "\x2518"
	texto2 = traducir (tipo cambio)
	
	-- Espacio en blanco antes de cada línea, para centrarlas.
	espacio1 = principio
	espacio2 = principio + ((length texto1 - length texto2) `div` 2)
	
	-- Contenido de cada línea.
	linea1 = (replicate espacio1 ' ') ++ texto1 ++ "\n"
	linea2 = (replicate espacio2 ' ') ++ texto2 ++ "\n"
 


-------------------------------------------------------------------------------
-- Para poder mostrar los resultados en pantalla, es necesario saber donde
-- empieza y donde acaba cada router. Las siguientes funciones calculan el
-- tamaño de cada uno de ellos.
-------------------------------------------------------------------------------

-- Calcula donde empieza y donde acaba la representación en texto de cada
-- router.
posiciones :: Escenario -> ([Int], [Int])
posiciones (a1,n1,red,n2,_) = loop (map lengthP red) final_n1 [principio_n1] [final_n1]
	where
	principio_n1 = length (printf "%s %s " (show a1) diamante :: String)
	final_n1 = length (printf "%s %s %s" (show a1) diamante (show n1) :: String)
	
	loop [] pos acc1 acc2 = (reverse (principio_n2:acc1), reverse (final_n2:acc2))
		where
		principio_n2 = pos + (2 + length (unicode Directa))
		final_n2  = principio_n2 + length (show n2)
	loop ((c,r):xs) pos acc1 acc2 = loop xs (pos+c+r) (pos+c : acc1) (pos+c+r : acc2)


-- Calcula el tamaño de una pareja (operador, router).
lengthP :: (Conexion, Router) -> (Int, Int)
lengthP (c,r) = (2 + length (unicode c), length (show r))



-------------------------------------------------------------------------------
-- Cuando se crea el escenario inicial, cada router de éste se corresponde
-- con un router físico. Sin embargo, al irse aplicando las reglas, puede
-- suceder que se agrupen varias zonas en una sola.
--
-- El tipo Extension indica a cuantas zonas físicas equivale una zona lógica.
-------------------------------------------------------------------------------

newtype Extension = Ex [Int] deriving Show


-- Crea una Extensión a partir de un escenario físico.
crearExt :: Escenario -> Extension
crearExt (_,_,red,_,_) = Ex (replicate (2 + length red) 1)


-- Aplica un cambio a una extensión, compactando las zonas necesarias.
aplicarCambio :: Cambio -> Extension -> Extension
aplicarCambio cambio (Ex zonas) = if (antes cambio == despues cambio)
	then Ex zonas
	else Ex (a ++ [sum b] ++ c)
	where
	-- Un cambio puede dar como resultado una zona de uno o más nodos. Si el
	-- resultado es 1, entonces toda la zona original se compacta, pero si
	-- es N, entonces se compactan sólo los últimos N nodos.
	p1 = inicio cambio + despues cambio - 1
	p2 = inicio cambio + antes cambio
	(a,b,c) = partir2 p1 p2 zonas


-- Parte una lista por dos puntos, dando como resultado tres sublistas.
-- Ejemplo:
--     partir2 i j [1..] = ([1..i] , [i+1..j] , [j+1..])
partir2 :: Int -> Int -> [a] -> ([a], [a], [a])
partir2 p1 p2 lista = (a, b, c)
	where
	a = take p1 lista
	b = take (p2-p1) $ drop p1 lista
	c = drop (p2) lista
		

-- Expande un rango de una red lógica a la red física original.
expandir :: Extension -> (Int, Int) -> (Int, Int)
expandir (Ex zonas) (zi, zf) = (zi', zf')
	where
	zi' = (sum $ take zi zonas)
	zf' = (sum $ take zf zonas) - 1
