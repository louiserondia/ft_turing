import os
import json
import sys
from copy import deepcopy

raw = open(sys.argv[1]).read()
data = json.loads(raw)

alphabet = data["alphabet"]

transitions = data["transitions"]

def step1():
    for k,v  in list(transitions.items()):
        for o in deepcopy(v) :
            if len(o["read"]) > 1:
                for c in o["read"]:
                    v.append(deepcopy(o))
                    if "[" in v[-1]["to_state"]:
                        v[-1]["to_state"] = v[-1]["to_state"].replace("[r]", c)
                    if v[-1]["write"] == "[r]" : v[-1]["write"] = c
                    v[-1]["read"] = c
        transitions[k] = list(o for o in v if len(o["read"]) == 1)

def step2():
    for k, v in list(transitions.items()):
        if not '[' in k: continue
        i1 = k.index("[")
        i2 = k.index("]")
        placeholder = k[i1:i2+1]
        for c in alphabet:
            s = k.replace(placeholder, c)
            transitions[s] = deepcopy(v)
            for o in transitions[s] :
                o["to_state"] = o["to_state"].replace(placeholder, c)
                o["write"] = o["write"].replace(placeholder, c)
        del transitions[k]

step1()

for _ in range(10):
    step2()

print(json.dumps(data))
