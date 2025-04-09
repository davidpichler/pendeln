library(gmapsdistance)
library(googleway)
library(lubridate)

# Check if running on GitHub Actions
running_on_github <- Sys.getenv("GITHUB_ACTIONS") == "true"

# Set Google API key
api_key <- "AIzaSyDBsbOpAc2yhrSTxS14rcUk30J1XNOUX28"
set.api.key(api_key)
set_key(api_key)

# Pendler-Startorte und Zielorte
startorte <- c("Leonding", "Gallneukirchen", "Ansfelden", 
               "Pregarten", "Enns", "Steyregg",
               "Ottensheim", "Bad Leonfelden", 
               "Wilhering")

zielorte <- c("Linz Hauptbahnhof", "Industriezeile, Linz", 
              "Johannes-Kepler-Universität Linz")

# Zeitabfragefunktion
time_query <- function(start, ziel) {
  res <- gmapsdistance(
    origin = paste0(start, ", Oberösterreich"),
    destination = paste0(ziel, ", Oberösterreich"),
    mode = "driving",
    departure = "now",
    traffic_model = "best_guess"
  )
  return(res$Time / 60)
}

# Straßenerkennung via google_directions
check_road_usage <- function(start, ziel) {
  route <- google_directions(
    origin = paste0(start, ", Oberösterreich"),
    destination = paste0(ziel, ", Oberösterreich"),
    mode = "driving",
    departure_time = "now",
    traffic_model = "best_guess"
  )
  
  steps <- tryCatch(route$routes$legs[[1]]$steps, error = function(e) NULL)
  if (is.null(steps)) return(c(B127 = NA, B129 = NA))
  htmls <- sapply(steps, function(x) x$html_instructions)
  c(
    B127 = any(grepl("B127", htmls)),
    B129 = any(grepl("B129", htmls))
  )
}

# Funktion für eine komplette Abfragerunde
run_abfrage <- function() {
  pendler_ergebnis <- list()
  
  for (start in startorte) {
    for (ziel in zielorte) {
      hin_time <- time_query(start, ziel)
      retour_time <- time_query(ziel, start)
      hin_roads <- check_road_usage(start, ziel)
      retour_roads <- check_road_usage(ziel, start)
      
      abfragezeit <- as.POSIXct(Sys.time(), tz = "Europe/Vienna")
      
      pendler_ergebnis[[length(pendler_ergebnis) + 1]] <- data.frame(
        datum_uhrzeit = abfragezeit,
        richtung = "Hin",
        startort = start,
        zielort = ziel,
        fahrtzeit_minuten = hin_time,
        b127 = hin_roads["B127"],
        b129 = hin_roads["B129"]
      )
      
      pendler_ergebnis[[length(pendler_ergebnis) + 1]] <- data.frame(
        datum_uhrzeit = abfragezeit,
        richtung = "Retour",
        startort = ziel,
        zielort = start,
        fahrtzeit_minuten = retour_time,
        b127 = retour_roads["B127"],
        b129 = retour_roads["B129"]
      )
    }
  }
  
  do.call(rbind, pendler_ergebnis)
}

# -------------------
# Zeitsteuerung
# -------------------

start_time <- Sys.time()
max_duration <- minutes(45)  # z.B. 3,5 Stunden

gesamtergebnis <- list()

while (Sys.time() - start_time < max_duration) {
  cat("Starte neue Runde: ", Sys.time(), "\n")
  
  abfrage_resultat <- run_abfrage()
  gesamtergebnis[[length(gesamtergebnis) + 1]] <- abfrage_resultat
  
  next_time <- ceiling_date(Sys.time(), unit = "15 minutes")
  wait_seconds <- as.numeric(difftime(next_time, Sys.time(), units = "secs"))
  
  cat("Warte bis ", format(next_time, "%H:%M:%S"), " (", round(wait_seconds), " Sekunden)\n")
  Sys.sleep(wait_seconds)
}

# Alles zusammenführen
df <- do.call(rbind, gesamtergebnis)

# Output-Dateiname mit Startzeitstempel
zeitstempel <- format(start_time, "%Y%m%d_%H%M")
dateiname <- paste0("pendelzeit_", zeitstempel, ".csv")

# Output-Ordner
output_dir <- "output_git"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

if (running_on_github) {
  write.table(df, 
              file = file.path(output_dir, dateiname),
              sep = ";",
              row.names = FALSE)
}

# Dateiname exportieren
writeLines(dateiname, "output_filename.txt")
