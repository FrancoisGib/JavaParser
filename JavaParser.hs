module JavaParser (parse) where

import System.IO
import Parser
import System.Environment
import Data.Char

esp :: Parser ()
esp = many (carQuand (\x -> x == ' ' || x == '\t' || x == '\n')) >> pure ()

word :: Parser String
word = (many (carQuand (`elem` ['a'..'z'] ++ ['A'..'Z']))) >>= \x -> pure x

allowedNamesCharacters :: Parser String
allowedNamesCharacters = some $ carQuand (`elem` (['a'..'z']++['A'..'Z']++['_']))

allowedTypesCharacters :: Parser String
allowedTypesCharacters = (some $ carQuand (`elem` (['a'..'z']++['A'..'Z']++['0'..'9']))) >>= \x -> (car '<' >> wellClosed 1 >>= \y -> pure (x ++ "<" ++ y)) <|> pure x
    where
        wellClosed 0 = pure ""
        wellClosed i = (many (carQuand (\x -> x /= '>' && x /= '<'))) >>= \x -> (car '<' >> wellClosed (i + 1) >>= \y -> pure (x ++ "<" ++ y)) <|> (car '>' >> wellClosed (i - 1) >>= \y -> pure (x ++ ">" ++ y))

allowedTypesCharactersWithoutEnd :: Parser String
allowedTypesCharactersWithoutEnd = (some $ carQuand (`elem` (['a'..'z']++['A'..'Z']++['0'..'9']))) >>= \x -> (car '<' >> (many (carQuand (\x -> x /= '>'))) >> car '>' >> pure x) <|> pure x

parseArrobase :: Parser ()
parseArrobase = parseComments >> many (car '@' >> some (carQuand (\char -> char /= '\n'))) >> parseComments >> pure ()

toUpperCaseFirstLetter :: String -> String
toUpperCaseFirstLetter (x:xs) = chr (ord x - 32) : xs

parsePrivacy :: Parser String
parsePrivacy = (chaine "public" >> pure '+') <|> (chaine "private" >> pure '-') <|> (chaine "protected" >> pure '#') <|> pure '~' >>= \x -> pure (x : " ")

parseComments :: Parser ()
parseComments = esp >> (parseComments1 <|> parseComments2 <|> pure ()) >> esp >> pure ()
    where parseComments1 = (chaine "/**" <|> chaine "/*") >> parseUntilEnd >> pure ()
          parseComments2 = chaine "//" >> (many (carQuand (\x -> x /= '\n'))) >> pure ()
          parseUntilEnd = (many (carQuand (\x -> x /= '/'))) >>= \asterisk -> if last asterisk == '*' then (car '/') else parseUntilEnd
          last [x] = x
          last (_:xs) = last xs
          
parseImportAndPackage :: Parser ()
parseImportAndPackage = (many ( parsePackage <|> parseImport)) >> pure ()
    where parsePackage = chaine "package" >> many (carQuand (\x -> x /= '\n')) >> esp >> pure ();
          parseImport = chaine "import" >> many (carQuand (\x -> x /= '\n')) >> esp >> pure ();

parseHeader :: Parser String
parseHeader = do
    parseArrobase
    privacy <- parsePrivacy
    esp
    abstract <- chaine "abstract " <|> pure ""
    esp
    classType <- chaine "class " <|> chaine "interface " <|> chaine "enum "
    esp
    className <- allowedTypesCharacters
    if classType == "enum" then do pure (classType ++ className ++ "\n") else do
    esp
    inheritance <- (many (esp >> extends className <|> allImplements className)) >>= \x -> pure (concat x)
    esp >> car '{'
    pure (inheritance ++ abstract ++ classType ++ className ++ " {")
    where
        extends className = chaine "extends" >> esp >> allowedTypesCharactersWithoutEnd >>= \parent -> pure (parent ++ " <|-- " ++ className ++ "\n")
        implements className = esp >> allowedTypesCharactersWithoutEnd >>= \parent -> pure (parent ++ " <|.. " ++ className ++ "\n")
        allImplements className = esp >> chaine "implements" >> implements className >>= \impl -> esp >> (some (car ',' >> implements className) >>= \res -> pure (impl ++ concat res)) <|> pure impl >>= \x -> pure x

parseAttribute :: Parser String
parseAttribute = do
    parseComments
    hasGetterSetter <- (many ((car '@' >> some (carQuand (\char -> char /= '\n')) >>= \header -> pure ('@' : header))) >>= \x -> esp >> pure x) <|> pure []
    parseComments
    privacy <- parsePrivacy
    esp
    static <- (chaine "static" >> pure "{static} ") <|> pure ""
    esp
    (chaine "final" >> pure "{final} ") <|> pure ""
    esp
    attributeType <- allowedTypesCharacters
    esp
    attributeName <- allowedNamesCharacters
    esp >> ((car '=' >> (many (carQuand (\x -> x /= ';'))) >> esp) <|> pure ()) >> car ';'
    getter <- if contains "@Getter" hasGetterSetter then pure ("\t+ get" ++ toUpperCaseFirstLetter attributeName ++ "() : " ++ attributeType ++ "\n") else pure ""
    setter <- if contains "@Setter" hasGetterSetter then pure ("\t+ set" ++ toUpperCaseFirstLetter attributeName ++ "(" ++ attributeName ++ " : " ++ attributeType ++ ") : void\n") else pure ""
    esp
    pure ('\t' : privacy ++ static ++ attributeName ++ " : " ++ attributeType ++ "\n" ++ getter ++ setter)
    where
        contains _ [] = False
        contains word (x:xs) = if word == x then True else contains word xs

parseThrows :: Parser ()
parseThrows = (parseComments >> chaine "throws" >> (some (parseComments >> carQuand (\x -> x /= '{'))) >> pure ()) <|> pure ()

isWellParenthesized :: Integer -> Parser () 
isWellParenthesized 0 = pure ()
isWellParenthesized i = ((car '{' >> (isWellParenthesized (i + 1))) <|> (car '}' >> (isWellParenthesized (i - 1))) <|> ((many (carQuand (\x -> x/='{' && x /= '}'))) >> isWellParenthesized i)) <|> pure ()

parseMethod :: Parser String
parseMethod = do
    parseArrobase
    privacy <- parsePrivacy
    esp
    static <- (chaine "static" >> pure "{static} ") <|> pure ""
    abstract <- (chaine "abstract" >> pure "{abstract} ") <|> pure ""
    esp
    returnType <- allowedTypesCharacters
    esp
    name <- allowedNamesCharacters
    car '('
    parameters <- ((some (carQuand (\x -> x /= ')'))) >>= \params -> pure params) <|> pure ""
    esp >> car ')'
    parseThrows
    parseBracket <|> (car ';' >> pure ())
    esp
    pure ('\t' : privacy ++ static ++ abstract ++ name ++ "(" ++ parameters ++ ")" ++ " : " ++ returnType ++ "\n")
    where
        parseBracket = parseComments >> (car '{' >> isWellParenthesized 1) <|> (car ';' >> pure ()) <|> pure ()

parseConstructor :: Parser String
parseConstructor = do
    parseArrobase
    privacy <- parsePrivacy
    esp
    name <- allowedNamesCharacters
    parameters <- ((car '{' >> esp >> pure "()") <|> ((some (carQuand (\x -> x /= '{'))) >>= \params -> pure params)) >>= \res -> pure res
    esp >> (some (carQuand (\x -> x /= '}'))) >> unCaractereQuelconque >> esp >> pure ()
    pure ('\t' : privacy ++ name ++ parameters ++ "\n")

parseFile :: Parser String
parseFile = parseImportAndPackage >> parseHeader >>= \header -> many (parseAttribute <|> parseMethod <|> parseConstructor) >>= \body -> pure (header ++ "\n" ++ concat body ++ "}")

parse :: String -> String
parse chaine = resultat (runParser parseFile chaine)
