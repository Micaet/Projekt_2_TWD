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
    import requests
    import pandas as pd

    # Twój klucz API
    api_key = "RGAPI-cf78151b-c0f8-4631-ad98-9edc87ba8cdb"
    puuid = "gvoAqYp_sGkRdYvAy4SZD98TIy4KHMvrlH3GcYKcL4RZvrtT6UWK9WD4qJmRUL2p7u29gdGpifKVXw"

    # URL do pobrania ID meczów
    match_list_api_url = f"https://europe.api.riotgames.com/lol/match/v5/matches/by-puuid/{puuid}/ids?start=60&count=10&api_key={api_key}"

    # Pobranie listy meczów
    response = requests.get(match_list_api_url)
    match_ids = response.json()

    # Tylko 3 pierwsze mecze z listy
    selected_matches = match_ids[:3]

    # Lista na dane wyjściowe
    data_records = []

    # Przetwarzanie wybranych meczów
    for match_id in selected_matches:
        # URL do pobrania danych o meczu
        match_url = f"https://europe.api.riotgames.com/lol/match/v5/matches/{match_id}?api_key={api_key}"
        timeline_url = f"https://europe.api.riotgames.com/lol/match/v5/matches/{match_id}/timeline?api_key={api_key}"

        match_response = requests.get(match_url)
        timeline_response = requests.get(timeline_url)

        if match_response.status_code == 200 and timeline_response.status_code == 200:
            match_data = match_response.json()
            timeline_data = timeline_response.json()

            # Znajdź indeks gracza
            player_idx = next((i for i, p in enumerate(match_data['metadata']['participants']) if p == puuid), None)

            if player_idx is not None:
                participant = match_data['info']['participants'][player_idx]

                # Filtrujemy pozycję gracza - tylko "BOTTOM"
                if participant['teamPosition'] == "BOTTOM":
                    champion_name = participant['championName']
                    champion_id = participant['championId']

                    # Pobieranie wydarzeń z timeline
                    for frame in timeline_data['info']['frames']:
                        for event in frame['events']:
                            position = event.get('position', {})
                            x = position.get('x', None)
                            y = position.get('y', None)

                            if x is not None and y is not None:
                                if event.get('killerId') == player_idx + 1:
                                    data_records.append({
                                        "player_id": puuid,
                                        "match_id": match_id,
                                        "x": x,
                                        "y": y,
                                        "type": "kill",
                                        "minute": frame['timestamp'] // 60000,
                                        "timestamp": event.get('timestamp'),
                                        "champion_id": champion_id,
                                        "champion_name": champion_name,
                                    })
                                elif event.get('victimId') == player_idx + 1:
                                    data_records.append({
                                        "player_id": puuid,
                                        "match_id": match_id,
                                        "x": x,
                                        "y": y,
                                        "type": "death",
                                        "minute": frame['timestamp'] // 60000,
                                        "timestamp": event.get('timestamp'),
                                        "champion_id": champion_id,
                                        "champion_name": champion_name,
                                    })
                                elif event.get('assistingParticipantIds') and (player_idx + 1) in event[
                                    'assistingParticipantIds']:
                                    data_records.append({
                                        "player_id": puuid,
                                        "match_id": match_id,
                                        "x": x,
                                        "y": y,
                                        "type": "assist",
                                        "minute": frame['timestamp'] // 60000,
                                        "timestamp": event.get('timestamp'),
                                        "champion_id": champion_id,
                                        "champion_name": champion_name,
                                    })

    # Tworzenie DataFrame z wynikami
    df = pd.DataFrame(data_records)

    # Zapis do pliku CSV
    df.to_csv("bottom_match_events.csv", index=False)

# Zapis do pliku CSV
df = pd.DataFrame(data_records)
df.to_csv(OUTPUT_FILE, index=False)
print(f"Data saved to {OUTPUT_FILE}")
