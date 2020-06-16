type Apodo = String
type Dias = Int
type Peso = Double 

data Pollo = Pollo{
  apodo:: Apodo,
  dias:: Dias,
  peso:: Peso,
  artesMarciales :: [String]
} deriving (Show, Eq)

--Pollos

pepe :: Pollo
pepe = Pollo "pepe" 200 600 ["judo","aikido"]

pepeDebil :: Pollo
pepeDebil = Pollo "pepeDebil" 200 600 ["judo","aikido"]

pancho :: Pollo
pancho = Pollo "pancho" 15 200 []

anita :: Pollo
anita = Pollo "anita" 180 800 ["karate"]

anitaDebil :: Pollo
anitaDebil = Pollo "anitaDebil" 180 800 []

pepeEntrenado :: Pollo
pepeEntrenado = Pollo "pepeEntrenado" 183 800 ["karate","judo","aikido"]

chickenNorris :: Pollo 
chickenNorris = Pollo "chickenNorris" 9000000 100 ["judo","aikido","karate","box"]


--1 Engordar a un pollo una cierta cantidad de gramos
engordar:: Peso->Pollo->Pollo
engordar pnuevo pollo= pollo{peso=peso pollo + pnuevo}

--2 Saber si un pollo es mayor de edad
esMayorEdad :: Pollo -> Bool
esMayorEdad pollo = (dias pollo) > 180 

--3 Saber si el último atributo de un pollo es vacío
ultimoAtributoEsVacio:: Pollo ->Bool
ultimoAtributoEsVacio pollo = length (artesMarciales pollo) == 0

--4 Cruzar un conjunto de pollos
sumaPesos:: [Pollo]->Double
sumaPesos lista = foldr ((+) . peso) 0 lista

sumaApodos:: [Pollo]->String
sumaApodos lista = foldr ((++) . apodo) "" lista

sumaDias:: [Pollo]->Int
sumaDias lista = foldr ((+) . dias) 0 lista

sumaArtesMarciales:: [Pollo]->[String]
sumaArtesMarciales lista = foldr ((++) . artesMarciales) [] lista

rePollo::[Pollo] -> Pollo
rePollo pollos = Pollo (sumaApodos pollos) (sumaDias pollos) ( sumaPesos pollos) (sumaArtesMarciales pollos)

  
-- ******************************************************************************************************************
-- ***************************  POLLOS NINJA     ********************************************************************
-- ******************************************************************************************************************

data Raton = Raton{
  altura:: Double,
  pesoRaton:: Double,
  cantidadDeBigotes :: Double
} deriving (Show, Eq)

--definicion de Ratones
mikey :: Raton
mikey = Raton 7 20 3

jerry :: Raton
jerry = Raton 4 30 5

speedy :: Raton
speedy = Raton 7 10 1


instance Show Entrenador where 
    show (Entrenador a b ) = "Entrenador: " ++ a

data Entrenador = Entrenador{
  nombre:: String,
  aprendizaje ::Pollo -> Pollo
} 


--aprendizae del entrenador y el pollo (aprendizaje entrenador) pepe
-- 1 Engorda 100 gramos al pollo que entrena 
arguniano::Entrenador
arguniano = Entrenador "arguniano" (\pollo -> engordar 100 pollo)

--2 Si no sabe, le enseña karate al pollo
miyagi::Entrenador
miyagi = Entrenador "miyagi" ( \pollo -> enseñaKarate pollo)

--ALTERNATIVA MIYAGI
-- miyagi::Pollo->Pollo
-- miyagi pollo 
--  | not(practicaKarate pollo) = pollo { artesMarciales = ["karate"]++artesMarciales pollo }
--  |otherwise = pollo

practicaKarate::Pollo->Bool
practicaKarate pollo= elem "karate" (artesMarciales pollo)

enseñaKarate:: Pollo->Pollo
enseñaKarate pollo
  | practicaKarate pollo =pollo 
  | otherwise = pollo {artesMarciales = ["karate"]++artesMarciales pollo}

-- 3 Hace que el pollo se olvide todas las artes marciales y después lo manda a aprender de Miyagi
marcelito::Entrenador
marcelito=Entrenador "marcelito" ( \pollo -> aprenderConMiyagui pollo)

--alternativa
-- marcelito::Pollo->Pollo
-- marcelito pollo = miyagi pollo { artesMarciales = [] } 

aprenderConMiyagui::Pollo->Pollo
aprenderConMiyagui pollo = (aprendizaje miyagi) (pollo{ artesMarciales = [] } )

--4  Alimenta al pollo dándole de comer un ratón
-- brujaTapita::Pollo->Pollo
-- brujaTapita pollo = pollo { peso = peso pollo + alimenta raton } 

alimenta::Raton->Double
alimenta raton = (pesoRaton raton * altura raton) - cantidadDeBigotes raton


brujaTapita raton = Entrenador "bruja Tapita" (\pollo -> engordar (alimenta raton) pollo ) 


-- 5 Toma al pollo y le agrega al nombre la frase “super mario ”
marioBros = Entrenador "marioBros" (\pollo -> superMario pollo) 

superMario pollo 
  | not (sabeJudo pollo) = agregarApodo  ( agregarHabilidadMario  ( enseniaJudo pollo ) )
  | otherwise = agregarApodo (agregarHabilidadMario pollo) 


agregarHabilidadMario pollo  = pollo { artesMarciales = ["saltar"] ++ artesMarciales pollo }
enseniaJudo pollo  = pollo { artesMarciales = ["judo"] ++ artesMarciales pollo }
agregarApodo pollo = pollo { apodo =  apodo pollo ++ "super mario" }

sabeJudo::Pollo->Bool
sabeJudo pollo= elem "judo" (artesMarciales pollo)

-- Entrenamiento
-- 1 Hacer que un entrenador entrene a un pollo
entrenarPollo:: Entrenador->Pollo->Pollo
entrenarPollo entrenador unPollo  = (aprendizaje entrenador) unPollo

--2 Dados dos entrenadores y un pollo, averiguar cuál de los dos entrenadores lo entrena mejor, 
quienEntrenaMejorAlPollo::Entrenador->Entrenador->Pollo->String
quienEntrenaMejorAlPollo entrenador1 entrenador2 unPollo 
   | (cantidadHabilidades (entrenarPollo  entrenador1 unPollo)) > (cantidadHabilidades (entrenarPollo  entrenador2 unPollo)) = nombre entrenador1
   | otherwise = nombre entrenador2

cantidadHabilidades::Pollo->Int
cantidadHabilidades unPollo = length (artesMarciales unPollo)

-- ******************************************************************************************************************
-- *************************** POLLOS   NINJA    ESPACIALES *********************************************************
-- ******************************************************************************************************************

--planetas


instance Show Planeta where 
    show (Planeta a1 b1 c1) = "Planeta: {"++a1++"}"

data Planeta = Planeta{
  nombrePaneta::String,
  entrenadorAsignado::Entrenador ,
  pollosHabitantes:: [Pollo]
} 


polloLan = Planeta "Pollo Lan" miyagi [pepe,pancho,anita,anitaDebil]
polloLanDebil = Planeta "Paneta Debil" miyagi [pepeDebil,pancho,anita,anitaDebil]
polloLandia = Planeta "Pollo Landia" arguniano [anita,pepeEntrenado]
elPLanetaDeLosPollos = Planeta "El Planeta De Los Pollos "marcelito [pepe,pancho,anita,anitaDebil,pepeDebil,pepeEntrenado,chickenNorris]


--1 
-- elMejorPollo :: [Pollo]->Pollo
-- elMejorPollo pollos= head (filter (\x -> cantidadHabilidades x == mayorCantidadHabilidades pollos) pollos)

--elMejorPollo :: Planeta -> Pollo
elMejorPollo planeta =  mejor (pollosHabitantes planeta)

mayorCantidadHabilidades:: [Pollo]->Int
mayorCantidadHabilidades = maximum.(map cantidadHabilidades)

maximoSegun f (x:xs) 
  | aplicar f (x:xs) == f x = x
  | otherwise = maximoSegun f xs

aplicar f [x] = f x
aplicar f (x:xs) = max (aplicar f [x]) (aplicar f xs)

mejor lista = maximoSegun (cantidadHabilidades) lista  

--2 esDebil 
--  Un planeta es débil si ninguno de sus pollos adultos
-- sabe más de 2 artes marciales o si al menos dos
-- de sus pollos no saben ningún arte marcial
esDebil:: Planeta->Bool
esDebil planeta = not (algunMayorEntrenado (pollosHabitantes planeta) || (limiteDebiles.cantidadHabilidadesPlaneta) (pollosHabitantes planeta))

cantidadHabilidadesPlaneta::[Pollo]->[Int]
cantidadHabilidadesPlaneta lista = [ cantidadHabilidades pollo | pollo<-lista ] 

limiteDebiles::[Int]->Bool
limiteDebiles lista = ((2<).(length).(filter (0==))) lista

mayorEntrenado::Pollo->Bool
mayorEntrenado pollo = esMayorEdad pollo && (length (artesMarciales pollo) > 2)

algunMayorEntrenado::[Pollo]->Bool
algunMayorEntrenado lista = any mayorEntrenado lista 


--3  Recibe un planeta y hace que su entrenador haga lo correspondiente con todos los 
--  pollos del planeta. Devuelve el planeta con todos sus llos entrenados.
entrenadorPlaneta::Entrenador->[Pollo]->[Pollo]
entrenadorPlaneta entrenador pollos = map (aprendizaje entrenador) pollos

entrenar::Planeta->[Pollo]
entrenar planeta = entrenadorPlaneta (entrenadorAsignado planeta) (pollosHabitantes planeta)

--entrenar planeta = planeta { pollosHabitantes = entrenadorPlaneta (entrenadorAsignado planeta) (pollosHabitantes planeta)}

--4 entrenamientoKaio dado dos planetas los pollos del primer planeta son entrenados por 
--su entrenador asignado y después por el entrenador del segundo planeta, devolviendo el planeta con los pollos entrenados.

entrenamientoKaio::Planeta->Planeta->[Pollo]
entrenamientoKaio planeta1 planeta2 = entrenadorPlaneta (entrenadorAsignado planeta2) (entrenar planeta1)

--entrenamientoKaio planeta1 planeta2 = planeta1 { pollosHabitantes = entrenadorPlaneta (entrenadorAsignado planeta2) (entrenar planeta1) }


--5 hacerViajeEspiritual hace que un pollo se transforme en The Chicken One
-- (el Pollo Elegido). Hacer un viaje espiritual es entrenar a un pollo con todos los entrenadores de la lista 
--que se recibe como argumento. Realizar los cambios 
--necesarios para que las funciones de la primera parte sigan funcionando.


entrenadoresLista = [miyagi,arguniano]

entrenamientos lista = [ aprendizaje entrenador| entrenador <-lista]

hacerViaje::[t -> t] -> t -> t
hacerViaje (x:xs) pollo 
  | (length xs == 0) = x pollo
  | otherwise = x (hacerViaje xs pollo)

hacerViajeEspiritual::[Entrenador] -> Pollo -> Pollo
hacerViajeEspiritual lista pollo = hacerViaje (entrenamientos lista) pollo


--6 planetaDebilEntrenado saber si un planeta queda débil incluso 
--después de hacer que todos sus pollos hagan un viaje espiritual con ciertos entrenadores


aplicarEntrenamientos planeta lista = planeta { pollosHabitantes = [hacerViajeEspiritual lista pollo| pollo <-(pollosHabitantes planeta)] }

planetaDebilEntrenado planeta lista =  esDebil (aplicarEntrenamientos planeta lista)


-- ******************************************************************************************************************
-- *************************** POLLOS  NINJA  ESPACIALES  MUTANTES **************************************************
-- ******************************************************************************************************************


--1) chickenNorris
-- con arguniano se puede y le aumenta el peso 
--Pollo {apodo = "Jose", dias = 200, peso = 700.0, artesMarciales = ["judo","aikido"]}
-- con miyagui no tiene sentido entrenar porque ya sabe karate y todas las artes marciales que existe. a menos que primero pase por marcelito
--Pollo {apodo = "chickenNorris", dias = 9000000, peso = 100.0, artesMarciales = ["judo","aikido","karate"]}
-- marcelito al hacerle olvidar todas las artes marciales, ahora tiene sentido que vaya a entrenar con miyagui
--Pollo {apodo = "chickenNorris", dias = 9000000, peso = 100.0, artesMarciales = ["karate"]}
-- brujatapita lo hace engordar dándole un ratón
--Pollo {apodo = "chickenNorris", dias = 9000000, peso = 215.0, artesMarciales = ["judo","aikido","karate"]}
-- marioBros le enseña una habilidad nueva "saltar"
--Pollo {apodo = "chickenNorrissuper mario", dias = 9000000, peso = 100.0, artesMarciales = ["saltar","judo","aikido","karate"]}


--2) 



obtenerNuevoEntrenador :: Planeta -> Entrenador
obtenerNuevoEntrenador planeta = enseniarArtesMarciales (artesMarciales (elMejorPollo planeta))

enseniarArtesMarciales ::[String] -> Entrenador
enseniarArtesMarciales artes  =  Entrenador "Nuevo maestro" (\pollo -> agregarArtesMarciales artes pollo)

agregarArtesMarciales::[String]->Pollo->Pollo
agregarArtesMarciales artes pollo = pollo { artesMarciales = artesMarciales pollo ++ artes}



--3)

marceniano::Entrenador
marceniano = Entrenador "marceniano" (\pollo -> mutacion pollo)

mutacion pollo = (aprendizaje arguniano) ((aprendizaje marcelito) pollo ) 
