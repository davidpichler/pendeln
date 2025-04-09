#!/bin/bash

echo "==> Starte Token-Refresh mit:"
echo "APP_KEY: $APP_KEY"
echo "APP_SECRET: (versteckt)"
echo "REFRESH_TOKEN: ${REFRESH_TOKEN:0:6}..."

# API-Call und Antwort speichern
RESPONSE=$(curl -s -X POST https://api.dropboxapi.com/oauth2/token \
  -u "$APP_KEY:$APP_SECRET" \
  -d grant_type=refresh_token \
  -d refresh_token=$REFRESH_TOKEN)

# Debug-Ausgabe der ganzen Antwort
echo "Dropbox API Antwort:"
echo "$RESPONSE"

# Token extrahieren
ACCESS_TOKEN=$(echo "$RESPONSE" | jq -r '.access_token')
echo "Access Token erhalten: ${ACCESS_TOKEN:0:10}..."

# In Datei schreiben, damit cat funktioniert
echo "$ACCESS_TOKEN" > dropbox_access_token.txt

# In GitHub Actions-Umgebung setzen
echo "DROPBOX_ACCESS_TOKEN=$ACCESS_TOKEN" >> $GITHUB_ENV
