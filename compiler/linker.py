import json
import sys

l = [json.loads(open(filename).read()) for filename in sys.argv[1:]]

main = next(data for data in l if "name" in data)

l = [e for e in l if e != main]

for d in l:
    for k,v in d.items():
        main["transitions"][k] = v

print(json.dumps(main))