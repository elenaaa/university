-- Grundlagen der Programmierung 2 / Aufgabenblatt 1
-- Lösungsvorschlag von Luca Ferrigno

-- Aufgabe 1
-- Funktionen 
-- Funktion zur Kontrolle der Bedingungen valider Punktzahlen
berechneNote :: Int -> Int -> Float
berechneNote x y = if x > 100 && y > 20 || x < 0 && y < 0
                    then error "Klausurpunkte & Bonuspunkte sind nicht korrekt"
                    else if x > 100 || x < 0
                          then error "Klausurpunkte sind nicht korrekt"
                          else if y > 20 || y < 0
                                then error "Bonuspunkte sind nicht korrekt"
                                else pruefe_Note (x + y)

-- Funktion, welche die Notevergabe nach Punktzahl enthält
pruefe_Note :: Int -> Float
pruefe_Note x = if x >= 86
                then 1.0
                else if x >= 82
                      then 1.3
                      else if x >= 78
                            then 1.7
                            else if x >= 74
                                  then 2.0
                                  else if x >= 70
                                        then 2.3
                                        else if x >= 66
                                              then 2.7
                                              else if x >= 62
                                                    then 3.0
                                                    else if x >= 58
                                                          then 3.3
                                                          else if x >= 54
                                                                then 3.7
                                                                else if x >= 50
                                                                      then 4.0
                                                                      else 5.0


-- Testfälle
-- Fehlerhafte Eingaben
{-
*Main> berechneNote 120 30
*** Exception: Klausurpunkte & Bonuspunkte sind nicht korrekt
*Main> berechneNote (-1) (-1)
*** Exception: Klausurpunkte & Bonuspunkte sind nicht korrekt
*Main> berechneNote 120 10
*** Exception: Klausurpunkte sind nicht korrekt
*Main> berechneNote (-1) 10
*** Exception: Klausurpunkte sind nicht korrekt
*Main> berechneNote 100 21
*** Exception: Bonuspunkte sind nicht korrekt
*Main> berechneNote 100 (-1)
*** Exception: Bonuspunkte sind nicht korrekt
-}
-- Beispielhafte korrekte Eingaben
{-
*Main> berechneNote 100 20
1.0
*Main> berechneNote 100 0
1.0
*Main> berechneNote 75 5
1.7
*Main> berechneNote 50 15
3.0
*Main> berechneNote 55 15
2.3
-}

-- Aufgabe 2
-- Aufgabenteil a & b
-- Aufgabenteile der Einfachheit halber zusammen gelöst

-- Zahlenfelder-Matrix 
matrix :: Int -> Int -> Int
matrix 1 1 = 2
matrix 1 2 = 1
matrix 1 3 = 2
matrix 2 1 = 2
matrix 2 2 = 2
matrix 2 3 = 3
matrix 3 1 = 2
matrix 3 2 = 1
matrix 3 3 = 1

-- Funktionen

-- Funktion um für eine Reihe alle Werte zu prüfen
pruefe_Reihe :: Int -> Int -> Int -> Int
pruefe_Reihe x 0 a = 0
pruefe_Reihe x y a = if matrix x y == a 
                     then 1 + pruefe_Reihe x (y - 1) a
                     else pruefe_Reihe x (y - 1) a

-- Funkton um für eine Matrix alle Werte zu prüfen
pruefe_Matrix :: Int -> Int -> Int -> Int
pruefe_Matrix 0 y a = 0
pruefe_Matrix x y a = pruefe_Reihe x y a + pruefe_Matrix (x - 1) y a

-- Wrapper für die Matrix-Funktion
-- maxIndex legt die Größe der Matrix fest, matrix muss einer "maxIndex x maxIndex"-Matrix entsprechen
maxIndex :: Int
maxIndex = 3 -- 3 angepasst an die Matrix aus Aufgabe 1

anzahlKartenMitZahl :: Int -> Int
anzahlKartenMitZahl x = if 0 < x && x <= maxIndex 
                         then pruefe_Matrix maxIndex maxIndex x
                         else error "Eingegebene Zahl nicht in Matrix enthalten"