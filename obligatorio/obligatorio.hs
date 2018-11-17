--data Asistente= nombre String| cedula String deriving Show
--data Curso = codigo Integer| docente Asistente| nombreCurso String| fecha String| asistentes [Asistente] deriving Show

--data Persona = Asistente String String String | Docente String String String

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
data Curso = Curso Integer Docente String [Asistente] deriving (Show)

getCodCurso :: Curso -> Integer
getCodCurso (Curso cod _ _ _) = cod

getDocente :: Curso -> Docente
getDocente (Curso _ docente _ _) = docente

getAsistentes :: Curso -> [Asistente]
getAsistentes (Curso _ _ _ lAsist) = lAsist

instance Ord Curso where
    (Curso cod _ _ _) > (Curso cod2 _ _ _) = (cod > cod2)
    (Curso cod _ _ _) < (Curso cod2 _ _ _) = (cod < cod2)

instance Eq Curso where
    (Curso cod _ _ _) == (Curso cod2 _ _ _) = (cod == cod2)


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

-- 11) Retorna el curso con mas alumnos
cursoMasAlumnos :: Cursos Curso -> Curso
cursoMasAlumnos abb = foldr (comparadorCantAlumnosCurso) (Curso 0 (Docente "" "" "") "" []) (listCursos abb)

-- 12) Dada la cedula de un alumno y el abb devuelve lista de cursos asistidos por el alumno.
listarCursosAsistidos :: Cursos Curso -> String -> [Curso]
listarCursosAsistidos abb ced = filter ((alumnoEnAsistentes ced).getAsistentes) (listCursos abb)

-- 13) Devuelve True si hay 2 cursos con los mismos alumnos.
hayCursosMismosAlumnos :: Cursos Curso -> Bool
hayCursosMismosAlumnos abb = (cursosMismosAlumnos (listCursos abb))

asist = Asistente "Nombre" "Apellido" "Cedula" 
asis2 = Asistente "Nombre1" "Apellido1" "Cedula1" 
docente = Docente "Nombre" "Apellido" "Cedula" 
curso = Curso 0 docente "NombreMateria" [asist]
curso2 = Curso 1 docente "NombreMateria2" [asist]
cursos = (insertarCurso curso) (crearHoja curso2)

lcur = listCursos cursos
ldoccur = listDocentesCursos lcur
