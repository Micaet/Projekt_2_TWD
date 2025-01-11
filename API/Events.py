import requests
import csv
from datetime import datetime
import math
import requests
import pandas as pd
import os

# Konfiguracja
API_KEY = "RGAPI-cf78151b-c0f8-4631-ad98-9edc87ba8cdb"
PUUID = "gvoAqYp_sGkRdYvAy4SZD98TIy4KHMvrlH3GcYKcL4RZvrtT6UWK9WD4qJmRUL2p7u29gdGpifKVXw"
BASE_URL = "https://europe.api.riotgames.com"
MATCH_IDS_URL = f"{BASE_URL}/lol/match/v5/matches/by-puuid/{PUUID}/ids?start=0&count=3&api_key={API_KEY}"
OUTPUT_FILE = "recent_matches.csv"

# Pobierz identyfikatory ostatnich 3 meczów
match_ids_response = requests.get(MATCH_IDS_URL)
if match_ids_response.status_code != 200:
    raise Exception(f"Failed to fetch match IDs: {match_ids_response.status_code}")
match_ids = match_ids_response.json()

# Lista do przechowywania danych
data_records = []

# Iteruj przez mecze
for match_id in match_ids:
    # Pobierz szczegóły meczu
    match_details_url = f"{BASE_URL}/lol/match/v5/matches/{match_id}?api_key={API_KEY}"
    match_details_response = requests.get(match_details_url)
    if match_details_response.status_code != 200:
        print(f"Failed to fetch match details for {match_id}")
        continue
    match_details = match_details_response.json()

    # Pobierz dane gracza z meczu
    player_idx = match_details['metadata']['participants'].index(PUUID)
    player_data = match_details['info']['participants'][player_idx]
    champion_name = player_data['championName']
    champion_id = player_data['championId']
    position = player_data['teamPosition']
    team_id = player_data['teamId']
    win = player_data['win']

    # Pobierz wydarzenia meczu
    timeline_url = f"{BASE_URL}/lol/match/v5/matches/{match_id}/timeline?api_key={API_KEY}"
    timeline_response = requests.get(timeline_url)
    if timeline_response.status_code != 200:
        print(f"Failed to fetch timeline for {match_id}")
        continue
    timeline = timeline_response.json()
    for frame in timeline['info']['frames']:
        for event in frame['events']:
            # Przypadek: Gracz jako zabójca
            if event.get('killerId') == player_idx + 1:
                position = event.get('position', {})
                x = position.get('x', None)
                y = position.get('y', None)
                if x is not None and y is not None:  # Sprawdzenie, czy x i y istnieją
                    record = {
                        "player_id": PUUID,
                        "match_id": match_id,
                        "x": x,
                        "y": y,
                        "type": "kill",
                        "minute": frame['timestamp'] // 60000,
                        "champion_id": champion_id,
                        "champion_name": champion_name,
                    }
                    data_records.append(record)

            # Przypadek: Gracz jako asystent
            if event.get('assistingParticipantIds') and (player_idx + 1) in event['assistingParticipantIds']:
                position = event.get('position', {})
                x = position.get('x', None)
                y = position.get('y', None)
                if x is not None and y is not None:  # Sprawdzenie, czy x i y istnieją
                    record = {
                        "player_id": PUUID,
                        "match_id": match_id,
                        "x": x,
                        "y": y,
                        "type": "assist",
                        "minute": frame['timestamp'] // 60000,
                        "champion_id": champion_id,
                        "champion_name": champion_name,
                    }
                    data_records.append(record)

            # Przypadek: Gracz jako ofiara
            if event.get('victimId') == player_idx + 1:
                position = event.get('position', {})
                x = position.get('x', None)
                y = position.get('y', None)
                if x is not None and y is not None:  # Sprawdzenie, czy x i y istnieją
                    record = {
                        "player_id": PUUID,
                        "match_id": match_id,
                        "x": x,
                        "y": y,
                        "type": "death",
                        "minute": frame['timestamp'] // 60000,
                        "champion_id": champion_id,
                        "champion_name": champion_name,
                    }
                    data_records.append(record)

# Zapis do pliku CSV
df = pd.DataFrame(data_records)
df.to_csv(OUTPUT_FILE, index=False)
print(f"Data saved to {OUTPUT_FILE}")
