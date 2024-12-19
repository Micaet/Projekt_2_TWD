import requests
import csv
api_key = "RGAPI-4f75404b-cef0-4d3f-a472-af1482c89a3e"
api_url = "https://europe.api.riotgames.com/lol/match/v5/matches/by-puuid/PtPkNOB3uqYHHzUVtJi6h1YfYv7otoqTDdWXjxzjUjL6KPdzXF6cfh-QnozGLgS9dZTibyb3W8Xj6Q/ids?start=0&count=20"
api_url = api_url + '&api_key=' + api_key

resp = requests.get(api_url)

match_ids = resp.json()
match_data = resp.json()
match_data
recent_match = match_ids[1]
api_url = "https://europe.api.riotgames.com/lol/match/v5/matches/" + str(recent_match)
api_url = api_url + '?api_key=' + api_key
resp = requests.get(api_url)
match_data = resp.json()
# player_data = match_data['info']['participants'][0]
# name = player_data['championName']
# position = player_data['teamPosition']
# k = player_data['kills']
# d = player_data['deaths']
# a = player_data['assists']
# print("Champion:", name)
# print("Position:", position)
# print("Kills:", k)
# print("Deaths:", d)
# print("Assists:", a)
# print("KDA:", (k + a) / d)
cus = match_data['info']['gameStartTimestamp']
print(cus)
participants = match_data['metadata']['participants']
player_index = participants.index("PtPkNOB3uqYHHzUVtJi6h1YfYv7otoqTDdWXjxzjUjL6KPdzXF6cfh-QnozGLgS9dZTibyb3W8Xj6Q")
player_data = match_data['info']['participants'][player_index]
name = player_data['championName']
position = player_data['teamPosition']
k = player_data['kills']
d = player_data['deaths']
a = player_data['assists']
win = player_data['win']
print("Champion:", name)
print("Position:", position)
print("Kills:", k)
print("Deaths:", d)
print("Assists:", a)
print("KDA:", (k + a) / d)
print("Win:", win)


