-- Aufgabe 1
-- Vorbereitung
untersteZahl   = 9

obersteZahl 10 = True
obersteZahl _  = False

direktUeber 9  = 8
direktUeber 8  = 1
direktUeber 1  = 18
direktUeber 18 = 11
direktUeber 11 = 5
direktUeber 5  = 3
direktUeber 3  = 20
direktUeber 20 = 10
direktUeber 10 = error "dies ist die oberste Zahl"
direktUeber _  = error "diese Zahl liegt nicht im Stapel"

-- Aufgabe 1 a
stapelEnthaeltZahl x = pruefeStapel x untersteZahl
pruefeStapel x y = if (x == y)
                    then True 
                    else if obersteZahl y == True 
                          then False
                          else pruefeStapel x (direktUeber y)

-- Aufgabe 1 b 
summiereStapel = addiereStapel untersteZahl
addiereStapel x = if obersteZahl x
                   then x
                   else x + addiereStapel (direktUeber x)

-- Aufgabe 1 c
liegtDarueber b a = if (direktUeber a) == b 
                     then True
                     else if obersteZahl (direktUeber a) 
                           then False
                           else liegtDarueber (direktUeber a) b 
