import os
from pathlib import Path
import json

l = [json.loads(open(file).read()) for file in Path(__file__).parent.iterdir() if file.is_file() and file.name.endswith(".json")]

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
