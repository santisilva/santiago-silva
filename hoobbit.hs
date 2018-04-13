
data Anillo= AnilloHobbit{
pesoanillo:: Int,
fraseanillo:: String

} deriving Show


data Hobbit = UnHobbit{
nombre::String,
estatura::Int,
salud::Int,
fuerza::Int,
comarca::Bool,
anillo::Anillo
} deriving Show


--Anillos
anilloNoLlevadoPorNadie =AnilloHobbit 17 "Anillo invisible"
anilloFuerte= AnilloHobbit 15 "Un anillo que sube la fuerza del hobbit"
superAnillo= AnilloHobbit 12 "Un Anillo para gobernarlos a todos. Un Anillo para encontrarlos, un Anillo para atraerlos a todos y atarlos en las tinieblas." 


--Hobbits
frodo= UnHobbit "Frodo" 106 10 50 True superAnillo 
sam= UnHobbit "Sam"  110 12 30 False anilloFuerte

poderAn :: Anillo -> Int
poderAn anillo = ((pesoanillo anillo) * ((length.fraseanillo) anillo))


--CAMBIO DE ANILLOS
cambioAnillo:: Hobbit->Anillo->Hobbit
cambioAnillo cambio anillos= cambio{ anillo= anillos}

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
