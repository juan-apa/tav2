--data Asistente= nombre String| cedula String deriving Show
--data Curso = codigo Integer| docente Asistente| nombreCurso String| fecha String| asistentes [Asistente] deriving Show

--data Persona = Asistente String String String | Docente String String String

import Char

-- ============= ASISTENTE =============
data Asistente = Asistente String String String deriving (Show)

instance Eq Asistente where
    (Asistente nom1 ap1 ced1) == (Asistente nom2 ap2 ced2) = ((nom1 == nom2) && (ap1 == ap2) && (ced1 == ced2))

getCedulaAS :: Asistente -> String
getCedulaAS (Asistente _ _ cedula) = cedula

agregarAsistente :: [Asistente] -> String -> String -> String -> [Asistente] 
agregarAsistente l nom ap ced = l ++ [(Asistente nom ap ced)]


-- ============= DOCENTE =============
data Docente = Docente String String String deriving (Show)

instance Eq Docente where
    (Docente nom1 ap1 ced1) == (Docente nom2 ap2 ced2) = ((nom1 == nom2) && (ap1 == ap2) && (ced1 == ced2))

getCedula :: Docente -> String
getCedula (Docente _ _ cedula) = cedula



-- ============= CURSO =============
--                 Codigo  Docente Nombre Fecha  Asistentes
data Curso = Curso Integer Docente String String [Asistente] deriving (Show)

getCodCurso :: Curso -> Integer
getCodCurso (Curso cod _ _ _ _) = cod

getDocente :: Curso -> Docente
getDocente (Curso _ docente _ _ _) = docente

getFecha :: Curso -> String
getFecha (Curso _ _ _ fec _) = fec

getAsistentes :: Curso -> [Asistente]
getAsistentes (Curso _ _ _ _ lAsist) = lAsist

instance Ord Curso where
    (Curso cod _ _ _ _) > (Curso cod2 _ _ _ _) = (cod > cod2)
    (Curso cod _ _ _ _) < (Curso cod2 _ _ _ _) = (cod < cod2)

instance Eq Curso where
    (Curso cod _ _ _ _) == (Curso cod2 _ _ _ _) = (cod == cod2)


cantAlumnosCurso :: Curso -> Integer
cantAlumnosCurso c = toInteger (length (getAsistentes c))


-- ============= CURSOS =============
data Cursos a = CursosVacio | Nodo a (Cursos a) (Cursos a) deriving (Show)

crearHoja :: Curso -> Cursos Curso
crearHoja x = Nodo x CursosVacio CursosVacio

-- Lista los cursos en orden del abb Cursos
listCursos :: Cursos Curso -> [Curso]
listCursos CursosVacio = []
listCursos (Nodo a left right) = (listCursos left) ++ [a] ++ (listCursos right)

obtenerCurso :: Integer -> Cursos Curso -> Curso
obtenerCurso cod (Nodo cur left right)
    |cod == (getCodCurso cur) = cur
    |cod < (getCodCurso cur) = obtenerCurso cod left
    |cod > (getCodCurso cur) = obtenerCurso cod right

obtenerCursoConAlumnos :: Cursos Curso -> Int
obtenerCursoConAlumnos abb = foldr (max.length) 0 (listadoAsistentes (listCursos abb))

-- Obtiene los docentes de la lista de cursos
-- parametros: Lista Curso
listDocentesCursos :: [Curso] -> [Docente]
listDocentesCursos l = map (getDocente) l

-- Obtiene las cedulas de un listado de docentes
-- parametros: lista Docente
listCedulasDocentes :: [Docente] -> [String]
listCedulasDocentes l = map (getCedula) l

-- Obtiene las cedulas de los asistentes
-- parametros: Lista Asistente
listCedulasAsistentes :: [Asistente] -> [String]
listCedulasAsistentes l = map getCedulaAS l

listadoAsistentes :: [Curso] -> [[Asistente]]
listadoAsistentes l = map (getAsistentes) (l)

cantAlumnosCursos :: Cursos Curso -> [Int]
cantAlumnosCursos abb = map (length) (listadoAsistentes (listarCursosPorCodigo abb)) 

comparadorCantAlumnosCurso :: Curso -> Curso -> Curso
comparadorCantAlumnosCurso c1 c2
    |(cantAlumnosCurso c1) > (cantAlumnosCurso c2) = c1
    |(cantAlumnosCurso c1) < (cantAlumnosCurso c2) = c2
    |(cantAlumnosCurso c1) == (cantAlumnosCurso c2) = c1

alumnoEnAsistentes :: String -> [Asistente] -> Bool
alumnoEnAsistentes ced [] = False
alumnoEnAsistentes ced lAs
    |(getCedulaAS (head lAs)) == ced = True
    |otherwise = (alumnoEnAsistentes ced (tail lAs))

mismosAsistentes :: [Asistente] -> [Asistente] -> Bool
mismosAsistentes [] [] = True
mismosAsistentes l1 l2
    |(head l1) == (head l2) = (mismosAsistentes (tail l1) (tail l2)) && True
    |(head l1) /= (head l2) = False

cursosMismosAlumnos :: [Curso] -> Bool
cursosMismosAlumnos [] = False
cursosMismosAlumnos l
    |(tail l) /= [] = (mismosAsistentes (getAsistentes (head l)) (getAsistentes(head (tail l)))) && cursosMismosAlumnos (tail l)
    |otherwise = True

charToInteger :: Char -> Integer
charToInteger c
    | c == '0' = 0
    | c == '1' = 1
    | c == '2' = 2
    | c == '3' = 3
    | c == '4' = 4
    | c == '5' = 5
    | c == '6' = 6
    | c == '7' = 7
    | c == '8' = 8
    | c == '9' = 9

-- Devuelve True si f1 > f2
compararFechas :: String -> String -> Bool
compararFechas [] [] = True
compararFechas f1 f2
    |(charToInteger (head f1)) > (charToInteger (head f2)) = True
    |(charToInteger (head f1)) < (charToInteger (head f2)) = False
    |(charToInteger (head f1)) == (charToInteger (head f2)) = compararFechas (tail f1) (tail f2)

listCursosAux :: Cursos Curso -> String-> [Curso]
listCursosAux CursosVacio f = []
listCursosAux (Nodo a left right) f 
    |(compararFechas (getFecha a) f) = (listCursosAux left (getFecha a)) ++ [a] ++ (listCursosAux right (getFecha a))
    |(compararFechas f (getFecha a)) = (listCursosAux right (getFecha a)) ++ [a] ++ (listCursosAux left (getFecha a))

-- ========= Funciones Obligatorio =========

-- 04) Devuelve si el docente dicto algun curso
-- parametros: ABB de cursos; Cedula Docente
yaEsDocente :: Cursos Curso -> String -> Bool
yaEsDocente abb ced = (length (filter (== ced) (listCedulasDocentes (listDocentesCursos (listCursos abb)) )) > 0)

-- 05) Devuelve true si existe un curso con el mismo codigo
perteneceCurso :: Integer -> Cursos Curso -> Bool
perteneceCurso cod CursosVacio = False
perteneceCurso cod (Nodo a left right) 
            | cod == (getCodCurso a) = True
            | cod < (getCodCurso a) = perteneceCurso cod left
            | cod > (getCodCurso a) = perteneceCurso cod right

-- 06) Inserta el curso al abb
insertarCurso ::  Curso -> Cursos Curso -> Cursos Curso
insertarCurso x CursosVacio = crearHoja x
insertarCurso x (Nodo a left right)
    |x == a = Nodo x left right
    |x < a = Nodo a (insertarCurso x left) right
    |x > a = Nodo a left (insertarCurso x right)

-- 07) Devuelve true si la persona con la cedula ya esta registrada al curso
yaEstaRegistrado :: Cursos Curso -> String -> Integer -> Bool
yaEstaRegistrado abb ced cod
    |perteneceCurso cod abb = (length (filter (== ced) (listCedulasAsistentes (getAsistentes (obtenerCurso cod abb)))) > 0)
    |otherwise = False

-- 08)  insertarAlumno

-- 09) Lista los cursos ordenados por codigo
listarCursosPorCodigo :: Cursos Curso -> [Curso]
listarCursosPorCodigo abb = listCursos abb

-- 10) Lista los cursos ordenados por Fecha
listarCursosPorFecha :: Cursos Curso -> [Curso]
listarCursosPorFecha abb = listCursosAux abb "00010101"

-- 11) Retorna el curso con mas alumnos
cursoMasAlumnos :: Cursos Curso -> Curso
cursoMasAlumnos abb = foldr (comparadorCantAlumnosCurso) (Curso 0 (Docente "" "" "") "" "" []) (listCursos abb)

-- 12) Dada la cedula de un alumno y el abb devuelve lista de cursos asistidos por el alumno.
listarCursosAsistidos :: Cursos Curso -> String -> [Curso]
listarCursosAsistidos abb ced = filter ((alumnoEnAsistentes ced).getAsistentes) (listCursos abb)

-- 13) Devuelve True si hay 2 cursos con los mismos alumnos.
hayCursosMismosAlumnos :: Cursos Curso -> Bool
hayCursosMismosAlumnos abb = (cursosMismosAlumnos (listCursos abb))


-- ============= DATOS DE PRUEBA =============
asist = Asistente "Nombre" "Apellido" "Cedula" 
asis2 = Asistente "Nombre1" "Apellido1" "Cedula1" 
docente = Docente "Nombre" "Apellido" "Cedula" 
curso1 = Curso 0 docente "NombreMateria1" "20180101" [asist]
curso2 = Curso 1 docente "NombreMateria2" "20180102" [asist]
curso3 = Curso 2 docente "NombreMateria2" "20180103" [asist]
curso4 = Curso 3 docente "NombreMateria2" "20180104" [asist]
curso5 = Curso 4 docente "NombreMateria2" "20180105" [asist]
cursos = (insertarCurso curso2) (crearHoja curso1)
cursos2 = insertarCurso curso4 cursos
cursos3 = insertarCurso curso3 cursos2
cursos4 = insertarCurso curso5 cursos3
lcur = listCursos cursos
ldoccur = listDocentesCursos lcur
