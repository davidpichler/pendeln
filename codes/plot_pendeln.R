library(tidyverse)
library(lubridate)
library(hms)

round_to_quarter_hour <- function(x) {
  x <- as.POSIXct(x)
  minutes <- as.numeric(format(x, "%M"))
  
  # Rundung in 15-Minuten-Schritten (00, 15, 30, 45)
  rounded <- round(minutes/15) * 15
  rounded <- ifelse(rounded == 60, 0, rounded)
  
  # Neues POSIXct mit gerundeten Minuten
  x_rounded <- as.POSIXct(format(x, "%Y-%m-%d %H:00:00"), tz = attr(x, "tzone")) +
    minutes(rounded)
  return(x_rounded)
}


# Alle CSVs einlesen
files <- list.files(path =  "C:/Users/pichl/Dropbox/funstuff/pendeln/output_git/", pattern = "^pendelzeit", full.names = TRUE)
data <- files %>%
  map_dfr(~ read.csv2(.x, sep = ";")) %>%
  mutate(
    datum_uhrzeit = ymd_hms(datum_uhrzeit),
    datum = as_date(datum_uhrzeit),
    uhrzeit = format(round_to_quarter_hour(datum_uhrzeit), "%H:%M"),
    fahrtzeit_minuten = fahrtzeit_minuten %>% as.numeric() %>% round(0)
  )


#%>%
 # mutate(file_datetime = str_extract(basename(.x), "//d{8}_//d{4}") %>%       parse_date_time(orders = "Ymd_HM"),     source_file = basename(.x)
    
    
    
    df <- read_all_pendel_csvs()
    
    # Beispieldatei einlesen zur Strukturüberprüfung
    # dat <- read_csv("output/pendelzeit_20250408_0332.csv")
    # glimpse(dat)
    
    # Beispielstruktur annehmen (falls nötig anpassen)
    # Spalten: von, nach, abfahrt, ankunft, dauer, dauer_in_minuten
    
    # Funktion zum Erstellen eines Plots
    plot_pendelzeiten <- function(df, strecke_von, strecke_nach, tage) {
      df %>%
        filter(
          startort %in% strecke_von,
          zielort %in% strecke_nach,
          datum %in% tage
        ) %>%
        ggplot(aes(x = uhrzeit, y = fahrtzeit_minuten, color = startort)) +
        geom_line() +
        geom_point() +
        labs(
          #title = paste0("Pendelzeit: ", strecke_von, " → ", strecke_nach, " am ", tage),
          x = "Abfragezeitpunkt",
          y = "Dauer in Minuten"
        ) +
        theme_minimal()
    }
    
    # Beispielnutzung
    # daten <- read_all_pendel_csvs()
    plot_pendelzeiten(data, strecke_von = c("Ottensheim","Wilhering"), "Industriezeile, Linz", "2025-04-08") +
      facet_wrap(~datum)
    