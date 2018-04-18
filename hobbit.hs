
data Anillo= AnilloHobbit{
pesoanillo:: Int,
fraseanillo:: String,
diasllevado:: Int

} deriving Show


data Hobbit = UnHobbit{
nombre::String,
estatura::Int,
salud::Int,
fuerza::Int,
comarca::Bool,
anillo::Anillo,
diasllevados :: Int
} deriving Show


--Anillos
anillonollevadopornadie =AnilloHobbit 17 "Anillo invisible" 15
anillofuerte= AnilloHobbit 15 "Un anillo que sube la fuerza del hobbit" 20
superanillo= AnilloHobbit 12 "Un Anillo para gobernarlos a todos. Un Anillo para encontrarlos, un Anillo para atraerlos a todos y atarlos en las tinieblas." 10 


--Hobbits
frodo= UnHobbit "Frodo" 106 10 50 True superanillo 15 
sam= UnHobbit "Sam"  110 12 30 False anillofuerte 15

poderAn :: Anillo -> Int
poderAn anillo = ((pesoanillo anillo) * ((length.fraseanillo) anillo))


--CAMBIO DE ANILLOS
--cambioAnillo:: Hobbit->Anillo-> Hobbit 
cambioAnillo hobit otroanillo= otroanillo{diasllevado = 0}
-- esto es asi o el cambio es no visible???


--Comarca
perteneceracomarcaresistencia hobit
                              | comarca hobit = ((salud hobit * estatura hobit) + fuerza hobit) 
							  |otherwise = (salud hobit * fuerza hobit)

							  
hobitconf hobit 
               | (head.nombre) hobit == 'f' = 10
			   | otherwise = 0

			   
resistenciaconanillo :: Hobbit-> Int
resistenciaconanillo hobit = max (perteneceracomarcaresistencia hobit + hobitconf hobit - (poderAn.anillo) hobit) 0

-- Comidas
desayuno :: Hobbit-> Hobbit
desayuno hobit = hobit{nombre = "errrp" ++ nombre hobit, salud = 5 + salud hobit}


segundodesayuno :: Int-> Hobbit-> Hobbit
segundodesayuno num hobit = hobit{fuerza = (4 * num) + fuerza hobit}


merienda :: Hobbit-> Hobbit
merienda hobit = (desayuno.segundodesayuno 2) hobit

-- Mas resistente con merienda 
resistenciapostmerienda :: Hobbit-> Int
resistenciapostmerienda hobit = resistenciaconanillo (merienda hobit)


ganadorconmerienda hobit hovit
                              |resistenciapostmerienda hobit > resistenciapostmerienda hovit = nombre hobit
							  |otherwise = nombre hovit

							  
-- hambre de hobit
tienehambre:: Hobbit-> Bool
tienehambre hobit = salud hobit < 50 && fuerza hobit > 9 || diasllevados hobit > 30

-- almuerzo

aumentoalmuerzo:: String -> Int -> Int
aumentoalmuerzo nombre cantidad
                               | head nombre == 'z' = div ((length nombre) * cantidad) 2
							   | otherwise = (length nombre) * cantidad 
            
			

almuerzo:: Hobbit-> String-> Int-> Hobbit			  
almuerzo hobit comida cantidad = hobit { salud = ((aumentoalmuerzo comida cantidad) * 2) + salud hobit, fuerza = (aumentoalmuerzo comida cantidad) + fuerza hobit}
              
			  
-- una mañana de viaje
unamanianadeviaje :: Hobbit-> Hobbit
unamanianadeviaje hobit = (merienda.desayuno.segundodesayuno 1) hobit 

manianadeviaje :: Hobbit-> Hobbit
manianadeviaje hobit = unamanianadeviaje (almuerzo hobit "liebres" 3) 



