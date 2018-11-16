--data Asistente= nombre String| cedula String deriving Show
--data Curso = codigo Integer| docente Asistente| nombreCurso String| fecha String| asistentes [Asistente] deriving Show

--data Persona = Asistente String String String | Docente String String String
data Asistente = Asistente String String String deriving (Show,Eq)

data Docente = Docente String String String deriving (Show, Eq)

getCedula :: Docente -> String
getCedula (Docente _ _ cedula) = cedula

-- ============= CURSO =============
data Curso = Curso Integer Docente String [Asistente] deriving (Show)

getCodCurso :: Curso -> Integer
getCodCurso (Curso cod _ _ _) = cod

getDocente :: Curso -> Docente
getDocente (Curso _ docente _ _) = docente

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
    |x > a = Nodo a (insertarCurso x left) right
    |x < a = Nodo a left (insertarCurso x right)

yaEsDocente :: (Ord a) => Cursos a -> [Docente]
yaEsDocente CursosVacio = []
yaseDocente (Nodo a left right) = [(getDocente a)] -- ++ (yaEsDocente left) ++ (yaEsDocente right)
    {- | x == a = Nodo x left right
    | x > a = Nodo a (yaEsDocente x left) right
    | x < a = Nodo a left (yaEsDocente x right) -}






asist = Asistente "Nombre" "Apellido" "Cedula" 
docente = Docente "Nombre" "Apellido" "Cedula" 
curso = Curso 0 docente "NombreMateria" [asist]
curso2 = Curso 1 docente "NombreMateria2" [asist]
cursos = (insertarCurso curso) (crearHoja curso2)




