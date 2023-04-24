type Titulo = String
type Autor = String
type Paginas = Int
type Libro = (Titulo, Autor, Paginas)

type Biblioteca = [Libro]
type Saga = [Libro]

sagaDeEragon :: Saga
sagaDeEragon = [eragon, eldest, brisignr,legado]

biblioteca :: Biblioteca
biblioteca = [elVisitante, shingekiNoKyojin1, shingekiNoKyojin3, shingekiNoKyojin27, fundacion, sandman5, sandman10, sandman12, eragon, eldest, brisignr, legado]

elVisitante :: Libro
elVisitante = ("el visitante", "Stephen King", 592)

shingekiNoKyojin1 :: Libro
shingekiNoKyojin1 = ("shingeki no kyojin 1 ", "Hajime Isayama", 40)

shingekiNoKyojin3 :: Libro
shingekiNoKyojin3 = ("shingeki no kyojin 3 ", "Hajime Isayama", 40)

shingekiNoKyojin27 :: Libro
shingekiNoKyojin27 = ("shingeki no kyojin 27 ", "Hajime Isayama", 40)

fundacion :: Libro
fundacion = ("Fundacion", "Isaac Asimov", 230)

sandman5 :: Libro
sandman5 = ("Sandman", "Neil Gaiman", 35)

sandman10 :: Libro
sandman10 = ("Sandman", "Neil Gaiman", 35)

sandman12 :: Libro
sandman12 = ("Sandman", "Neil Gaiman", 35)

eragon :: Libro
eragon = ("Eragon", "Christopher Paolini", 544)

eldest :: Libro
eldest = ("Eldest", "Christopher Paolini", 704)

brisignr  :: Libro
brisignr = ("Brisignr", "Christopher Paolini", 700)

legado  :: Libro
legado = ("Legado", "Christopher Paolini", 811)

lecturaObligatoria :: Libro -> Bool
lecturaObligatoria (_,"Stephen King", _) = True
lecturaObligatoria ("Fundacion", "Isaac Asimov", 230) = True
--lecturaObligatoria esDeEragon unLibro = True
lecturaObligatoria _ = False

esDeEragon :: Libro -> Bool
esDeEragon unLibro = elem unLibro sagaDeEragon

promedioDeHojas :: Biblioteca -> Int
promedioDeHojas unaBiblioteca = div (cantidadDePaginasTotales unaBiblioteca) (length unaBiblioteca)

cantidadDePaginas :: Libro -> Int
cantidadDePaginas (_, _, cantPaginas) = cantPaginas

cantidadDePaginasTotales :: Biblioteca -> Int
cantidadDePaginasTotales unaBiblioteca = sum . map cantidadDePaginas $ unaBiblioteca

vocales :: [Char]
vocales = "aeiouAEIOUÁÉÍÓÚ"

esVocal :: Char -> Bool
esVocal unaLetra = elem unaLetra vocales

quitarVocales :: String -> String
quitarVocales unString = filter (not . esVocal) unString

nombreDeLaBiblioteca :: Biblioteca -> String
nombreDeLaBiblioteca unaBiblioteca = quitarVocales . concatenarTitulos $ unaBiblioteca

concatenarTitulos :: Biblioteca -> String
concatenarTitulos unaBiblioteca = concatMap obtenerTitulo unaBiblioteca

obtenerTitulo :: Libro -> String
obtenerTitulo (unTitulo,_,_)= unTitulo

esFantasiosa :: Biblioteca -> Bool
esFantasiosa (_, "Christopher Paolini", _) = True
esFantasiosa (_, "Neil Gaiman", _) = True
esFantasiosa = False

bibliotecaLigera :: Biblioteca -> Bool
bibliotecaLigera unaBiblioteca = all lecturaLigera unaBiblioteca

lecturaLigera :: Libro -> Bool
lecturaLigera unLibro = ((<=40) . cantidadDePaginas) unLibro

{--genero :: Libro -> String
genero (_,"Stephen King",_) = "Terror"
genero (_,"Hajime Isayama",_) = "Manga"
genero unLibro
    |lecturaObligatoria unLibro = "Comic"
    |otherwise                  = "Sin categoria"

type Persona = (String, String, Int)
gus :: Persona
gus = ("Gustavo", "Trucco",32)--}