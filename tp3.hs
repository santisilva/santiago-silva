
jugador1::(String,[Integer])
jugador1  = ("Juan perez", [5, 23, 35, 17, 56, 80])
jugador2::(String,[Integer])
jugador2  = ("lenadro perez", [5, 23, 35, 15, 56, 80])
jugador3::(String,[Integer])
jugador3  = ("martin perez", [33, 23, 35, 17, 56, 80])
jugador4::(String,[Integer])
jugador4  = ("miguel perez", [5, 23, 35, 17, 56, 55])
jugador5::(String,[Integer])
jugador5  = ("pablo pereza", [2, 23, 35, 17, 56, 80])

minimovalido :: (String,[Integer])-> Bool
minimovalido tupla = minimum (snd tupla) == head (snd tupla)

maximovalido :: (String,[Integer])-> Bool
maximovalido tupla = maximum (snd tupla) == last (snd tupla)

tamaniovalido :: (String,[Integer])-> Bool
tamaniovalido tupla = length (snd tupla) == 6 

listavalida ::(String,[Integer])-> Bool
listavalida tupla  = tamaniovalido tupla && maximovalido tupla && minimovalido tupla
--listavalida tupla  = minimum (snd tupla) == head (snd tupla) && maximum (snd tupla) == last (snd tupla) && length (snd tupla) == 6 




posibleganador ::(String,[Integer])-> String-> Integer -> Bool
posibleganador tupla duenio ultimabola = reverse (take 5 (reverse (fst tupla))) == duenio && elem ultimabola (snd tupla)
--posibleganador ::(String,[Integer])-> Bool
--posibleganador tupla = reverse (take 5 (reverse (fst tupla))) == "Perez" && elem 17 (snd tupla)



ganador ::(String,[Integer])-> String-> Integer -> Bool
ganador tupla duenio ultimabola = posibleganador tupla duenio ultimabola && listavalida tupla 


