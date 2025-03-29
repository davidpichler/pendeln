# Pakete laden
pacman::p_load(gmapsdistance)

# Google API-Key setzen (ersetze mit deinem Schlüssel)
set.api.key("AIzaSyDBsbOpAc2yhrSTxS14rcUk30J1XNOUX28")

# Pendler-Startorte definieren
startorte <- c("Leonding", "Gallneukirchen", "Ansfelden", 
               "Pregarten", "Enns", "Steyregg",
               "Ottensheim", "Bad Leonfelden", 
               "Wilhering")

# Zielpunkte in Linz definieren
zielorte <- c("Linz Hauptbahnhof", "Industriezeile, Linz", 
              "Johannes-Kepler-Universität Linz")

# Funktion zur Zeitabfrage definieren
zeit_abfrage <- function(start, ziel) {
  ergebnis <- gmapsdistance(
    origin = paste0(start, ", Oberösterreich"),
    destination = ziel,
    mode = "driving",
    departure = "now",
    traffic_model = "best_guess"
  )
  return(ergebnis$Time / 60) # Fahrtzeit in Minuten
}

# Schleife zur Abfrage aller Kombinationen
pendler_ergebnis <- lapply(startorte, function(start) {
  lapply(zielorte, function(ziel) {
    data.frame(
      datum_uhrzeit = Sys.time(),
      startort = start,
      zielort = ziel,
      fahrtzeit_minuten = zeit_abfrage(start, ziel)
    )
  })
})

# Ergebnisse zusammenfassen und speichern
pendelzeiten_df <- do.call(rbind, do.call(c, pendler_ergebnis))

# CSV-Dateiname dynamisch erzeugen
zeitstempel <- format(Sys.time(), "%Y-%m-%d_%H-%M")
dateiname <- paste0("pendelzeit_", zeitstempel, ".csv")

# Daten als CSV speichern
zeitstempel <- format(Sys.time(), "%Y-%m-%d_%H-%M")
dateiname <- paste0("output/pendelzeit_", zeitstempel, ".csv")

write.table(pendelzeiten_df, 
            file = dateiname,
            sep = ";",
            row.names = FALSE)

