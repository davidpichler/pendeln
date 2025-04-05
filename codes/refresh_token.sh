#!/bin/bash

ACCESS_TOKEN=$(curl -s -X POST https://api.dropboxapi.com/oauth2/token \
  -u "$APP_KEY:$APP_SECRET" \
  -d grant_type=refresh_token \
  -d refresh_token=$REFRESH_TOKEN | jq -r '.access_token')

echo "DROPBOX_ACCESS_TOKEN=$ACCESS_TOKEN" >> $GITHUB_ENV
