library(gmapsdistance)
library(googleway)
library(httr)
library(jsonlite)

# Check if running on GitHub Actions
running_on_github <- Sys.getenv("GITHUB_ACTIONS") == "true"
dropbox_token <- Sys.getenv("DROPBOX_ACCESS_TOKEN")

# Set Google API key
api_key <- "AIzaSyDBsbOpAc2yhrSTxS14rcUk30J1XNOUX28"
set.api.key(api_key)
set_key(api_key)

# Pendler-Startorte und Zielorte
startorte <- c("Stadtamt Leonding", "Gemeindeamt Gallneukirchen", "Stadtamt Ansfelden", 
               "Gemeindeamt Pregarten", "Stadtamt Enns", "Steyregg",
               "Gemeindeamt Ottensheim", "Gemeindeamt Bad Leonfelden", 
               "Gemeindeamt Wilhering")

zielorte <- c("Linz Hauptbahnhof", "Prinz-Eugen-Strasse 22, 4020 Linz", 
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

# Funktion zur Rundung auf nächste Viertelstunde
round_up_to_next_quarter <- function(time) {
  mins <- as.integer(format(time, "%M"))
  secs <- as.integer(format(time, "%S"))
  add <- 15 - (mins %% 15)
  if (add == 15 && secs == 0) add <- 0
  rounded <- as.POSIXct(trunc(time, "mins")) + add * 60
  return(rounded)
}

# Dropbox Upload-Funktion
upload_to_dropbox <- function(file_path, dropbox_path, token) {
  if (is.na(token) || token == "") {
    message("⚠️ Kein Dropbox-Token gefunden – Upload wird übersprungen.")
    return(invisible(NULL))
  }
  
  res <- POST(
    url = "https://content.dropboxapi.com/2/files/upload",
    add_headers(
      Authorization = paste("Bearer", token),
      `Dropbox-API-Arg` = toJSON(
        list(path = dropbox_path, mode = "overwrite"),
        auto_unbox = TRUE
      ),
      `Content-Type` = "application/octet-stream"
    ),
    body = upload_file(file_path)
  )
  
  if (res$status_code == 200) {
    message("✅ Datei erfolgreich in Dropbox hochgeladen: ", dropbox_path)
  } else {
    warning("❌ Dropbox-Upload fehlgeschlagen: ", content(res, "text"))
  }
}

# -------------------
# Zeitsteuerung
# -------------------

start_time <- Sys.time()
max_duration_secs <- 4 * 60 * 60  # 3,5 Stunden
output_dir <- "output_git"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Wait until next 00, 15, 30, or 45
cat("Zeit Start:", format(Sys.time(), "%H:%M:%S"), "\n")
next_time <- round_up_to_next_quarter(Sys.time())
wait_seconds <- as.numeric(difftime(next_time, Sys.time(), units = "secs"))
cat("Warte auf erste Runde bis ", format(next_time, "%H:%M:%S"),"Wartezeit: ",round(wait_seconds/60,2)," Minuten", "\n")
Sys.sleep(wait_seconds)

while (as.numeric(difftime(Sys.time(), start_time, units = "secs")) < max_duration_secs) {
  cat("Starte neue Runde: ", Sys.time(), "\n")
  
  abfrage_df <- run_abfrage()
  
  # Write one CSV file per round
  vienna_now <- format(Sys.time(), tz = "Europe/Vienna", usetz = FALSE)
  file_timestamp <- format(as.POSIXct(vienna_now, tz = "Europe/Vienna"), "%Y%m%d_%H%M")
  filename <- paste0("pendelzeit_", file_timestamp, ".csv")
  local_path <- file.path(output_dir, filename)
  dropbox_path <- paste0("/funstuff/pendeln/output_git/", filename)
  
  write.table(abfrage_df, file = local_path, sep = ";", row.names = FALSE)
  
  if (running_on_github) {
    upload_to_dropbox(local_path, dropbox_path, dropbox_token)
  }
  
  # Wait until next quarter-hour
  next_time <- round_up_to_next_quarter(Sys.time())
  wait_seconds <- as.numeric(difftime(next_time, Sys.time(), units = "secs"))
  cat("Warte bis ", format(next_time, "%H:%M:%S"), " (", round(wait_seconds), " Sekunden)\n")
  Sys.sleep(wait_seconds)
}


