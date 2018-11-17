--data Asistente= nombre String| cedula String deriving Show
--data Curso = codigo Integer| docente Asistente| nombreCurso String| fecha String| asistentes [Asistente] deriving Show

--data Persona = Asistente String String String | Docente String String String

-- ============= ASISTENTE =============
data Asistente = Asistente String String String deriving (Show)

instance Eq Asistente where
    (Asistente nom1 ap1 ced1) == (Asistente nom2 ap2 ced2) = ((nom1 == nom2) && (ap1 == ap2) && (ced1 == ced2))

getCedulaAS :: Asistente -> String
getCedulaAS (Asistente _ _ cedula) = cedula
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




-- ============= CURSOS =============
data Cursos a = CursosVacio | Nodo a (Cursos a) (Cursos a) deriving (Show)

crearHoja :: a -> Cursos a
crearHoja x = Nodo x CursosVacio CursosVacio

insertarCurso :: (Ord a) => a -> Cursos a -> Cursos a
insertarCurso x CursosVacio = crearHoja x
insertarCurso x (Nodo a left right)
    |x == a = Nodo x left right
    |x < a = Nodo a (insertarCurso x left) right
    |x > a = Nodo a left (insertarCurso x right)

-- Lista los cursos en orden del abb Cursos
listCursos :: Cursos a -> [a]
listCursos CursosVacio = []
listCursos (Nodo a left right) = (listCursos left) ++ [a] ++ (listCursos right)

-- Devuelve true si existe un curso con el mismo codigo
existeCurso :: Integer -> Cursos Curso -> Bool
existeCurso cod CursosVacio = False
existeCurso cod (Nodo a left right) 
            | cod == (getCodCurso a) = True
            | cod < (getCodCurso a) = existeCurso cod left
            | cod > (getCodCurso a) = existeCurso cod right

obtenerCurso :: Integer -> Cursos Curso -> Curso
obtenerCurso cod (Nodo cur left right)
    |cod == (getCodCurso cur) = cur
    |cod < (getCodCurso cur) = obtenerCurso cod left
    |cod > (getCodCurso cur) = obtenerCurso cod right
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

-- Devuelve si el docente dicto algun curso
-- parametros: ABB de cursos; Cedula Docente
yaEsDocente :: Cursos Curso -> String -> Bool
yaEsDocente abb ced = (length (filter (== ced) (listCedulasDocentes (listDocentesCursos (listCursos abb)) )) > 0)

-- Devuelve true si la persona con la cedula ya esta registrada al curso
yaEstaRegistrado :: Cursos Curso -> String -> Integer -> Bool
yaEstaRegistrado abb ced cod
    |existeCurso cod abb = (length (filter (== ced) (listCedulasAsistentes (getAsistentes (obtenerCurso cod abb)))) > 0)
    |otherwise = False

asist = Asistente "Nombre" "Apellido" "Cedula" 
docente = Docente "Nombre" "Apellido" "Cedula" 
curso = Curso 0 docente "NombreMateria" [asist]
curso2 = Curso 1 docente "NombreMateria2" [asist]
cursos = (insertarCurso curso) (crearHoja curso2)

lcur = listCursos cursos
ldoccur = listDocentesCursos lcur
