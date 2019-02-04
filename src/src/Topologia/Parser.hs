-------------------------------------------------------------------------------
-- Autor: José Franco Campos
-- 
-- Generación, análisis y optimización de escenarios de migración
-- de redes Ipv4/IPv6 mediante programación funcional.
-- 
-- Diciembre 2008
--
--
-- Descripción: analizador sintáctico de GML, y conversor de GML a texto.
-------------------------------------------------------------------------------

module Topologia.Parser (
	leerGML,
	escribirGML
	) where


-- Módulos externos
import Data.Map
import Text.ParserCombinators.Parsec


-- Módulos del programa
import Topologia.Tipos



-------------------------------------------------------------------------------
-- En GML, si aparece un carácter especial dentro de una cadena de texto,
-- se representa por medio de una entidad HTML. Estas dos tablas definen la
-- conversión de carácter a entidad y viceversa.
-------------------------------------------------------------------------------

-- Conversión de carácter a entidad.
char2ent :: Map Char String
char2ent = fromList [
	('"', "quot"),
	('&', "amp")]


-- Conversión de entidad a carácter.
ent2char :: Map String Char
ent2char = fromList [
	("quot", '"'),
	("amp", '&')]



-------------------------------------------------------------------------------
-- Análisis sintáctico
-------------------------------------------------------------------------------

-- Regla inicial de la gramática.
gml :: Parser Valor
gml = do
	elementos <- lista
	return $ VM elementos


-- Definición de una lista de pares clave-valor.
-- Se devuelve como un array asociativo indexado por clave.
-- List ::= (whitespace* String whitespace+ Valor)*
lista :: Parser (Map String [Valor])
lista = do
	elementos <- many par
	return $ fromListWith (flip (++)) $ Prelude.map (\(key,value) -> (key,[value])) elementos


-- Definición de un par clave-valor.
par :: Parser (String, Valor)
par = do
	spaces
	k <- clave
	skipMany1 space
	v <- valor
	spaces
	return (k, v)


-- Definición de los diferentes tipos de valores.
-- Valor ::= Integer | Real | String | [ List ]
valor :: Parser Valor
valor = try (do
		r <- real
		return $ VR r
	)
	<|> (do
		i <- entero
		return $ VI i
	)
	<|> (do
		s <- cadena
		return $ VS s
	)
	<|> (do
		char '['
		l <- lista
		char ']'
		return $ VM l
	)


-- Definición de un identificador.
-- String ::= [ a-z A-Z ] [ a-z A-Z 0-9 ]*
clave :: Parser String
clave = do
	a <- letter
	b <- many alphaNum
	return (a:b)

-- Definición de un número entero.
-- integer ::= sign digit+
entero :: Parser Int
entero = do
	s <- signo
	d <- many1 digit
	return $ read (s:d)


-- Definición de un número real.
-- Real ::= sign digit* . digit* exponent
real :: Parser Double
real = do
	s <- signo
	a <- many digit
	char '.'
	b <- many digit
	e <- option "0" exponente
	return $ read (s : a ++ "." ++ b ++ e)


-- Definición de una cadena de texto.
-- String ::= " instring "
cadena :: Parser String
cadena = do
	char '"'
	s <- many instring
	char '"'
	return s


-- Signo de un número.
-- sign ::= empty | '+' | '-'
signo :: Parser Char
signo = choice [do {char '+' ; return ' '}, char '-', return ' ']


-- Exponente de un número real.
-- exponent ::= empty | 'E' signo digit
exponente :: Parser String
exponente = do
	char 'e'
	s <- signo
	e <- many1 digit
	return $ 'e' : s : e


-- Caracteres que pueden aparecer dentro de una cadena
-- instring ::= ASCII - {&,"} | '&' character+ ';'
instring :: Parser Char
instring = noneOf "\"&"
 	<|> (do
 		char '&'
 		cs <- many1 letter
 		char ';'
 		return (ent2char ! cs)
 	)



-------------------------------------------------------------------------------
-- Conversión a texto
-------------------------------------------------------------------------------

-- Analizador sintáctico. El primer argumento debe ser el nombre del fichero
-- original que se usará en caso de error para informar al usuario.
leerGML :: SourceName -> String -> Valor
leerGML name text = case Text.ParserCombinators.Parsec.parse gml name text of
	Left err -> error (show err)
	Right val -> val
	

-- Conversión de un árbol sintáctico a texto.
escribirGML :: Valor -> String
escribirGML = show_val 0


-- Muestra un Valor con anidamiento.
show_val :: Int -> Valor -> String
show_val _ (VI i) = show i
show_val _ (VR r) = show r
show_val _ (VS s) = show (escape s)
show_val n (VM l) = apertura ++ cuerpo ++ cierre
	where
	apertura, cuerpo, cierre :: String
	apertura = if n == 0 then "" else "[\n"
	cierre   = if n == 0 then "" else replicate n '\t' ++ "]"
	cuerpo   = concatMap (show_pair (n+1)) (Data.Map.toList l)


-- Muestra un par clave-valor con anidamiento.
show_pair :: Int -> (String, [Valor]) -> String
show_pair n (key, values) = concat [replicate n '\t' ++ key ++ " " ++ show_val n val ++ "\n" | val <- values]


-- Convierte los caracteres no representables de una cadena a entidades HTML.
escape :: String -> String
escape [] = []
escape (c:cs) = c' ++ escape cs
	where
	c' = case Data.Map.lookup c char2ent of
		Nothing -> [c]
		Just ent -> "&" ++ ent ++ ";"
