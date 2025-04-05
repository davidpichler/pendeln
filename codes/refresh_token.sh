#!/bin/bash

echo "==> Starte Token-Refresh mit:"
echo "APP_KEY: $APP_KEY"
echo "APP_SECRET: (versteckt)"
echo "REFRESH_TOKEN: ${REFRESH_TOKEN:0:6}..."

ACCESS_TOKEN=$(curl -s -X POST https://api.dropboxapi.com/oauth2/token \
  -u "$APP_KEY:$APP_SECRET" \
  -d grant_type=refresh_token \
  -d refresh_token=$REFRESH_TOKEN | jq -r '.access_token')

echo "Access Token erhalten: ${ACCESS_TOKEN:0:10}..."

# Speichern für GitHub Actions
echo "DROPBOX_ACCESS_TOKEN=$ACCESS_TOKEN" >> $GITHUB_ENV
