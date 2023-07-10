### ---------------------------------------------------------------------
### Warteschlangensimulation
### --------------------------------------------------------------------- 


# Zu Beginn ausfuehren, um zu testen, ob der Code einwandfrei durchlaeuft
rm(list = ls())


### --------------------------------------------------------------------- 
### 1) Simulation der Warteschlange
### --------------------------------------------------------------------- 


### 1a) Funktion schreiben

# Hier hat es einige Kriterien gegeben:
# 1.) Die Formeln in der Angabe moegen zu einer exzessiven Benuetung der
#     for-Schleife verleiten. Unsere Aufgabe in der Simulation ist es, 
#     bessere Wege zu finden ;-)
# 2.) Wenn wir for-Schleifen verwenden, dann UNBEDINGT auf die Initialisierung
#     achten.

warteschlange <- function(n, lambda, mu) {
  # Simuliert die Warteschlange n Mal und gibt ein Data.frame zurueck mit
  #   T ... Die Ankunftszeiten
  #   w ... Wartezeiten
  #   x ... Bedienzeiten
  #   z ... Zeiten im System
  #   V ... Zeitpunkte des Verlassens des Systems
  # Parameter:
  # n ........ Anzahl der simulierten Personen
  # lambda ... Ankunftsrate: Wie viele Kunden kommen im Mittel pro
  #            Zeiteinheit ins System?
  # mu ....... Bedienrate: Wie viele Kunden koennen im Mittel pro
  #            Zeiteinheit (hoechstens) bedient werden?
  # Es muss mu > lambda gelten.

  # Pruefe, ob mu > lambda ist.
  if (mu <= lambda) {
    stop("Die Bedienrate mu muss groesser als die Ankunftsrate lambda sein!")
  }

  # Die Zwischenankunftszeiten und die Bedienzeiten koennen vektorwertig
  # simuliert werden! t[1] = 0, da der 1. Kunde sofort kommt.
  t <- c(0, rexp(n-1, rate = lambda))
  x <- rexp(n, rate = mu)

  # Daraus berechnen wir die Ankunftszeiten T
  T <- cumsum(t)

  # Berechnung der Wartezeiten w
  # Hier NICHT mit
  # w <- NULL
  # initialisieren, der Laufzeitunterschied ist enorm!
  # Ein anschauliches Beispiel, was das f?r R heisst:
  # Ihr habt 10 Buecher gekauft und wollt diese in ein Buecherregal stellen.
  # Das Buecherregal ist noch nicht vorhanden und muss noch gekauft werden.
  # Fall NULL: Ihr packt das erste Buch aus, fahrt in ein Moebelhaus und 
  #            kauft ein Regal, in das genau ein Buch passt. Zuhause
  #            angekommen, stellt ihr enttaeuscht fest, dass das naechste
  #            Buch nicht mehr hineinpasst. Folglich macht ihr euch erneut
  #            auf den Weg und erwerbt ein Regal fuer genau zwei Buecher.
  #            Wieder die Ernuechterung, das dritte Buch passt nicht hinein.
  #            Ihr macht euch erneut auf den Weg, um - richtig - ein Regal
  #            fuer 3 Buecher zu kaufen...
  # FALL numeric(n): Ihr kauft gleich ein Regal, in das alle Buecher hinein-
  #            passen.
  w <- numeric(n)

  # Alternativ die flexible Funktion vector() verwenden, die auch fuer
  # die Initialisierung von Listen verwendet werden kann.
  # w <- vector(mode = "numeric", length = n)

  # Der erste Kunde wird sofort bedient, danach for-Schleife.
  w[1] <- 0

  for (i in 2:n) {
    w[i] <- max(0, T[i-1] + w[i-1] + x[i-1] - T[i])
  }

  # Gesamtzeiten im System und Austrittszeitpunkte
  # Vektorwertige Berechnung moeglich!
  z <- w + x
  V <- T + z

  # Rueckgabe
  return(data.frame(T = T, w = w, x = x, z = z, V = V))
}


### 1b) Simulation

# Seed festlegen zwecks Reproduzierbarkeit
set.seed(10)

# Parameter festlegen
n <- 100000
lambda <- 4
mu <- 5

# Daten simulieren
queue1 <- warteschlange(n, lambda, mu)

# Daten geeignet abspeichern
file <- "C:/Users/Daniel/Desktop/Lehre_Univie/Statistisches Programmieren/WS_2022/Abschlussbeispiele/queue.csv"
# write.table(queue1, file = file, sep = ";", row.names = FALSE, quote = FALSE)



### --------------------------------------------------------------------- 
### 2) Diverse Berechnungen
### --------------------------------------------------------------------- 

### 2a) Auslastung

# Schaetzung: Summe der Bedienzeiten dividiert durch die Gesamtzeit.
rho.estimate <- sum(queue1$x) / queue1$V[nrow(queue1)]
rho.estimate

# Theoretische Auslastung
rho <- lambda / mu
rho

# Bemerkung: Da queue1$V aufsteigend sortiert ist, k?nnen wir mit 
# queue1$V[nrow(queue1)] die Gesamtzeit ermitteln. max(queue1$V) ist auch
# moeglich, wenngleich einen Tick weniger effizient.


### 2b) Histogramm mit den Gesamtverweilzeiten

# Histogramm zeichnen. freq = FALSE oder probability = TRUE bedeutet, dass
# relative Haeufigkeiten gezeichnet werden. hist() gibt uns nuetliche
# Berechnungen zurueck :-)
h <- hist(queue1$z, freq = FALSE, col = "lightblue",
  main = "Histogramm der Gesamtverweilzeiten",
  xlab = "Gesamtverweilzeit", ylab = "Relative Haeufigkeit")

# Theoretische Dichte einzeichnen
xx <- seq(0, h$breaks[length(h$breaks)], length = 201)
yy <- dexp(xx, rate = mu * (1-rho))
lines(xx, yy, col = "blue")

# Legende ist immer gut ;-)
legend("topright", legend = "Theoretische Dichte", lwd = 1, col = "blue",
  inset = 0.04)

# Bemerkung zur Farbwahl: Fuer die Balken bitte dezente Farben verwenden ;-)


### 2c) Warteschlangenlaenge pro Person

### Variante 1: "Brachiale" for-Schleife

# Erste (ineffiziente) Variante. Person i kommt zum Zeitpunkt T[i] im
# System an. Alle Personen, die vor dieser Person gekommen sind (= Personen
# 1 bis i-1) und erst nach der Ankunft dieser Person das System verlassen
# haben (also V > T[i]) zaehlen zur Warteschlange.

# Sehr langsam! Mit ESC kann der Code abgebrochen werden.
L <- rep(0, n)
for (i in 2:n) {
  L[i] <- sum(queue1$T[i] < queue1$V[1:(i-1)])
}
head(L, 100)
summary(L)

# Das Problem ist, dass wir in jeder Iteration immer von der ersten Person
# beginnen zu zaehlen und daher der Vektor queue1$V[1:(i-1)] sehr lange
# werden kann (viel unnoetiger Ballast).


### Variante 2: Effizientere for-Schleife

# Jetzt zur effizienteren Variante. Die Daten sind sortiert. Folgende
# Idee: Angenommen, wir kennen die Warteschlangenlaenge, sobald Person i-1
# ins System kommt. Bezeichne diese mit L[i-1]. Dann wissen wir, dass die
# Warteschlangenlaenge fuer die i. Person hoechstens L[i-1]+1 betragen
# kann. Wir brauchen also nicht den gesamten Vektor V durchzugehen, sondern
# es genuegt fuer die i. Person, den Bereich von (i-1)-L[i-1] bis (i-1)
# zu betrachten. Dadurch sparen wir enorm viel Zeit!

# Ein weiterer Effizienztrick: Im Gegensatz zu vielen Programmiersprachen
# gibt es in R keine Referenzen. Beim Zugriff queue1$V wird (schlimmstenfalls)
# eine Kopie des Vektors erstellt, und zwar 100000 Mal. Das ersparen wir uns,
# indem wir den Inhalt vor der Schleife kopieren.

L <- rep(0, n)
V <- queue1$V
T <- queue1$T

for (i in 2:n) {
  von <- (i-1) - L[i-1]
  bis <- (i-1)
  L[i] <- sum(V[von:bis] > T[i])
}

head(L, 100)
summary(L)


### Variante 3: Mit einer while-Schleife die Warteschlangenlaenge bestimmen

# In dieser Variante bestimmen wir die Warteschlange der i. Person so, dass
# wir der Reihe nach die Personen i-1, i-2, i-3, ... , i-j betrachten, und
# zwar so lange, bis die i-j. Person erstmals vor Person i gegangen ist.

L <- rep(0, n)
V <- queue1$V
T <- queue1$T

for(i in 2:n) {
    # j beschreibt die aktuelle Schlangenlaenge (inklusive dem aktuellen
    # Kunden) und wird laufend hochgezaehlt in der unten folgenden
    # while-Schleife.
    j <- 1

    # Jede Person, die nach der i. Person geht, zaehlt zur Schlange.
    # Solange also die i. Person frueher ins System kommt, wie die j.
    # Person das System verlaesst, wird die Schleife durchlaufen.
    while(T[i] < V[i-j]) {
        j <- j + 1

        # Wenn dir die erste Person erreichen (kommt zu beginn vor), dann
        # jedenfalls abbrechen.
        if (i-j <= 0) {
          break()
        }
    }

    # Warteschlangenlaenge zuweisen (j ist inklusive der i. Person, diese
    # muss weggerechnet werden).
    L[i] <- j - 1
}

head(L, 100)
summary(L)

# Kopiere die Daten und haenge die Warteschlangenlaengen an.
queue2 <- queue1
queue2$L <- L


### RData-Objekt erstellen
file <- "C:/Users/Daniel/Desktop/Lehre_Univie/Statistisches Programmieren/WS_2022/Abschlussbeispiele/Warteschlange.RData"
# save(queue1, queue2, file = file)



### --------------------------------------------------------------------- 
### 3) Wartezeiten und Warteschlangenlaengen
### --------------------------------------------------------------------- 

### 3a) Faktor mit NAs

# Eine Haeufigkeitstabelle bietet sich an:
L.tab <- table(queue2$L)
L.tab

# Suche jene Groesse, die erstmals seltener als 50 Mal vorkommt.
# Beachte: Klarerweise wird keine Zahl ausgelassen!
L.index <- as.numeric(names(L.tab)[L.tab < 50])[1]
L.index

# Setze all jene Werte auf NA, die >= L.index sind und erstelle einen Faktor.
L.faktor <- ifelse(queue2$L < L.index, queue2$L, NA)
L.faktor <- factor(L.faktor)

# Anmerkung: In factor() werden die Kategorien standardmaessig sortiert.
# In diesem Fall haben wir numerische Werte, das heisst, dass die Zahlen
# aufsteigend sortiert sind (was erwuenscht ist).


### 3b) Visualisierung

# Hier bietet sich zB ein Boxplot an.

# Neues Device oeffnen und Breite vergroessern
dev.new(width = 16)

main1 <- "Verteilung der Wartezeit einer neu ankommenden Person"
main2 <- "getrennt nach Warteschlangengroesse"
main <- paste(main1, main2)

plot(L.faktor, queue2$w, main = main, ylab = "Wartezeit", xlab = 
  "Groesse der Warteschlange bei der Ankunft")
# Auch moeglich:
# plot(queue$w ~ L.faktor, ...)

# Die Grafik kann noch verschoenert werden (siehe gleich).


### 3c)

# Wir kommen ins System an und erkennen, wie lange die Schlange ist.
# Wir suchen jene Wartezeit, die mit 90%iger Wahrscheinlichkeit nicht
# unterschritten wird. Also jene Zeit, die wir mit 90%iger Wahrscheinlich-
# kein mindestens warten muessen. Wir suchen also das 10%-Quantil. 

res10 <- tapply(queue2$w, L.faktor, quantile, prob = 0.1)
round(res10, 2)

# ZB Gegeben, dass 20 Personen vor uns in der Schlange stehen, so wartet
# man in 90% der Faelle mind. 3.06 Zeiteinheiten bis zur Bedienung.


### Grafiktipps zu 3b) - Oder besser: Pimp your boxplot

dev.new(width = 16)
plot(queue2$w ~ L.faktor, main = main, ylab = "Wartezeit", xlab = 
  "Groesse der Warteschlange bei der Ankunft", yaxt = "n")

# Jetzt ein Code, der fast unkommentiert bleibt, aber euch inspirieren kann,
# tiefer einzutauchen ;-)

xx <- extendrange(par()$usr[1:2], f = 1/20)
yy <- pretty(extendrange(par()$usr[3:4], f = 1/20), n = 30)

col <- hcl(120, 0, c(80, 90))

for (i in 2:length(yy)) {
  rect(xx[1], yy[i-1], xx[2], yy[i], col = col[(i %% 2) + 1], b = NA)
}


plot(queue2$w ~ L.faktor, main = main, ylab = "Wartezeit", xlab = 
  "Groesse der Warteschlange bei der Ankunft", add = TRUE, axes = FALSE,
  col = hcl(240, 80, 80))

box()
axis(2, at = yy, las = 1)
axis(4, at = yy, las = 1)


