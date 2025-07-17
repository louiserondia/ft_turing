import os
from pathlib import Path
import json

# Obtenir le chemin du dossier où se trouve ce script
script_dir = Path(__file__).parent

l = [json.loads(open(file).read()) for file in script_dir.iterdir() if file.is_file() and file.name.endswith(".json")]

main = next(data for data in l if "name" in data)

l = [e for e in l if e != main]


res = []

for data in l:
    for k in data.keys():
        res.append(k)

for k in main["transitions"].keys():
    res.append(k)

res.append("end")

print(json.dumps(sorted(res)))

# Parcourir tous les fichiers dans ce dossier