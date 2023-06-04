module Artist where
    
import UdGraphic
import Test.QuickCheck
import Debug.Trace


-- Problema 1

-- Separa una comanda en una lista de comandes individuals sense el seu infix :#:.
separa :: Comanda -> [Comanda]
separa (p :#: q) = separa p ++ separa q -- Separar la comanda composta en les seves parts i concatenar les llistes resultants.
separa Para = [] -- Si la comanda és un 'Para', retornar una llista buida.
separa comanda = [comanda] -- Per a qualsevol altra comanda, retornar-la en una llista individual.


-- Problema 2

-- Junta una llista de comandes en una comanda amb l'infix :#: entre comandes.
ajunta :: [Comanda] -> Comanda
ajunta [] = Para -- Si la llista és buida, retornar 'Para'.
ajunta (x:xs) = x :#: ajunta xs -- Concatenar la primera comanda i l'infix :#: amb la resta de la llista recursivament.


-- Problema 3

-- Comprova si dues comandes són equivalents, és a dir, si es separen en la mateixa llista de comandes.
prop_equivalent :: Comanda -> Comanda -> Bool
prop_equivalent c1 c2 = (separa c1) == (separa c2)


-- Comprova si en separar i després ajuntar una comanda s'obté la comanda original.
prop_split_join :: Comanda -> Bool
prop_split_join c = c == ajunta (separa c)


-- Comprova que la funcio separar ha retornat efectivament la llista de comandes
-- sense cap Para ni comanda composta (infix :#: entre elles).
prop_split :: [Comanda] -> Bool
prop_split c = all comandaSimplif c
  where
    comandaSimplif (p :#: q) = False -- Cas que tingui infix
    comandaSimplif Para = False -- Cas que tingui Para
    comandaSimplif _ = True -- Cas que estigui OK


-- Problema 4

-- Realitza una còpia d'una comanda donada un nombre d'iteracions.
copia :: Int -> Comanda -> Comanda
copia i c
  | i == 1    = c -- Cas base.
  | otherwise = c :#: copia (i-1) c -- Cas recursiu.


-- Problema 5

-- Genera una comanda per dibuixar un pentàgon a partir d'una distància donada.
pentagon :: Distancia -> Comanda
pentagon d = copia 5 (Avança d :#: Gira 72) -- Fa la copia perque te 5 costats amb distancia d i gira 72 graus.


-- Problema 6

-- Genera una comanda per dibuixar un polígon regular a partir de la distància, el nombre de costats i l'angle donats.
poligon :: Distancia -> Int -> Angle -> Comanda
poligon d n angle = copia n (Avança d :#: Gira angle)

-- Comprova si la comanda generada per dibuixar un polígon regular amb distància d és equivalent a la comanda del pentàgon amb la mateixa distància.
-- Verifica si les dues comandes són equivalents utilitzant la funció prop_equivalent, la qual compara les llistes de comandes generades.
prop_poligon_pentagon :: Distancia -> Bool
prop_poligon_pentagon d = prop_equivalent  (poligon d 5 72)(pentagon d)


-- Problema 7

-- Genera una llista de distàncies per a cada costat de l'espiral
espiral :: Distancia -> Int -> Distancia -> Angle -> Comanda
espiral costat n pas angle = foldr (:#:) Para [Avança d :#: Gira angle | d <- take n [costat, costat+pas ..]]   -- En cada iteració, pren la distància `d` de la llista generada per `take`
                                                                                                                -- a partir de `costat`, incrementant en `pas` en cada pas


-- Problema 9

-- Optimitza una llista de comandes, reduint les repeticions i eliminant els `Para` innecessaris.
optimitzaComandes :: [Comanda] -> Comanda
optimitzaComandes [] = Para -- Cas base: si la llista és buida, retorna `Para`
optimitzaComandes [c]
                  | equivalentPara c = Para -- Si l'única comanda de la llista és `Para`, retorna `Para`
                  | otherwise = c -- Altrament, retorna la mateixa comanda
optimitzaComandes (c1 : c2 : cs)
  | equivalentPara c1 = optimitzaComandes (c2 : cs) -- Si la primera comanda és `Para`, la descarta i passa a l'element següent
  | equivalentPara c2 = optimitzaComandes (c1 : cs) -- Si la segona comanda és `Para`, descarta la segona comanda i passa a l'element següent
  | otherwise = case (c1, c2) of
      (Avança d1, Avança d2) -> optimitzaComandes (Avança (d1 + d2) : cs) -- Si les comandes són d'avançar, les combina en una sola comanda d'avanç amb la suma de les distàncies
      (Gira a1, Gira a2) -> optimitzaComandes (Gira (a1 + a2) : cs)  -- Si les comandes són de girar, les combina en una sola comanda de gir amb la suma dels angles
      _ -> c1 :#: optimitzaComandes (c2 : cs) -- En qualsevol altre cas, manté la primera comanda i continua amb la resta de la llista

-- Verifica si una comanda és equivalent a `Para`
equivalentPara :: Comanda -> Bool
equivalentPara Para = True -- `Para` és equivalent a `Para`
equivalentPara (Avança 0) = True -- L'acció d'avançar amb distància 0 és equivalent a `Para`
equivalentPara (Gira 0) = True -- L'acció de girar amb angle 0 és equivalent a `Para`
equivalentPara _ = False -- En qualsevol altre cas, la comanda no és equivalent a `Para`

-- Optimitza una comanda aplicant l'optimització de `optimitzaComandes` repetidament fins a obtenir una comanda final optimitzada.
optimitza :: Comanda -> Comanda
optimitza c = optimitzaComandes (separa (optimitzaComandes (separa c))) -- S'utiltiza dos cops el separa i l'optimitzaComandes per tal d'eliminar l'ultim Para


-- Problema 10

-- Genera una comanda que expandeix la lletra 'F' en forma recursiva
expandirF :: Int -> Comanda
expandirF 0 = Avança 20 -- Cas base
expandirF n = expandirF (n-1) :#: Gira 90 :#: expandirF (n-1) :#: Gira (-90) :#: expandirF (n-1) :#: Gira (-90) :#: expandirF (n-1) :#: Gira 90 :#: expandirF (n-1) -- Reescriptura de F

-- Genera una comanda per dibuixar el fractal triangle utilitzant la reescriptura de la lletra 'F'
triangle :: Int -> Comanda
triangle n = Gira 90 :#: expandirF n

-- Problema 11

-- Genera una comanda que expandeix la lletra 'F' en forma recursiva
expandirF1 :: Int -> Comanda
expandirF1 0 = CanviaColor verd :#: Avança 5 -- Cas base
expandirF1 n = expandirG (n-1) :#: Branca (Gira (-45) :#: expandirF1 (n-1)) :#: Branca (Gira 45 :#: expandirF1 (n-1)) :#: Branca (expandirG (n-1) :#: expandirF1 (n-1)) -- Reescriptura de F

-- Genera una comanda que expandeix la lletra 'G' en forma recursiva
expandirG :: Int -> Comanda
expandirG 0 = Avança 5 -- Cas base
expandirG n = expandirG (n-1) :#: expandirG (n-1) -- Reescriptura de G

-- Genera una comanda per dibuixar el fractal fulla  utilitzant la reescriptura de la lletra 'F'
fulla :: Int -> Comanda
fulla n = expandirF1 n


-- Problema 12

-- Genera una comanda que expandeix la lletra 'L' en forma recursiva
expandirL :: Int -> Comanda
expandirL 0 = Para -- Cas base
expandirL n = Gira 90 :#: expandirR (n-1) :#: Avança 20 :#: Gira (-90) :#: expandirL (n-1) :#: Avança 20 :#: expandirL (n-1) :#: Gira (-90) :#: Avança 20 :#: expandirR (n-1) :#: Gira 90 -- Reescriptura de L

-- Genera una comanda que expandeix la lletra 'R' en forma recursiva
expandirR :: Int -> Comanda
expandirR 0 = Para -- Cas base
expandirR n = Gira (-90) :#: expandirL (n-1) :#: Avança 20 :#: Gira 90 :#: expandirR (n-1) :#: Avança 20 :#: expandirR (n-1) :#: Gira 90 :#: Avança 20 :#: expandirL (n-1) :#: Gira (-90) -- Reescriptura de R

-- Genera una comanda per dibuixar el fractal hilbert  utilitzant la reescriptura de la lletra 'L'
hilbert :: Int -> Comanda
hilbert n = expandirL n

-- Problema 13

-- Genera una comanda que expandeix la lletra 'F' en forma recursiva
expandirF2 :: Int -> Comanda
expandirF2 0 = Avança 10 -- Cas base
expandirF2 n = expandirG1 (n-1) :#: Gira 60 :#: expandirF2 (n-1) :#: Gira 60 :#: expandirG1 (n-1) -- Reescriptura de F

-- Genera una comanda que expandeix la lletra 'G' en forma recursiva
expandirG1 :: Int -> Comanda
expandirG1 0 = Avança 10 -- Cas base
expandirG1 n = expandirF2 (n-1) :#: Gira (-60) :#: expandirG1 (n-1) :#: Gira (-60) :#: expandirF2 (n-1) -- Reescriptura de G

-- Genera una comanda per dibuixar el fractal fletxa  utilitzant la reescriptura de la lletra 'F'
fletxa :: Int -> Comanda
fletxa n = Gira 90 :#: expandirF2 n

-- Problema 14

-- Genera una comanda que expandeix la lletra 'G' en forma recursiva
expandirG2 :: Int -> Comanda
expandirG2 0 = Avança 10 -- Cas base
expandirG2 n = expandirF3 (n-1) :#: Gira (-22.5) :#: Branca (Branca (expandirG2 (n-1)) :#: Gira 22.5 :#: expandirG2 (n-1)) :#: Gira 22.5 :#: expandirF3 (n-1) :#: Branca (Gira 22.5 :#: expandirF3 (n-1) :#: expandirG2 (n-1)) :#:  Gira (-22.5) :#: expandirG2 (n-1) -- Reescriptura de G

-- Genera una comanda que expandeix la lletra 'F' en forma recursiva
expandirF3 :: Int -> Comanda
expandirF3 0 = Avança 10 -- Cas base
expandirF3 n = expandirF3 (n-1) :#: expandirF3 (n-1) -- Reescriptura de F

-- Genera una comanda per dibuixar el fractal branca  utilitzant la reescriptura de la lletra 'G'
branca :: Int -> Comanda
branca n =  expandirG2 n