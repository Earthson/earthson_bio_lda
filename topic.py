#!/usr/bin/env python

from itertools import *
from functools import *

savefile = "save/r4999"
to_word = None
to_id = None

kkk = 0
mcnt = 0
wcnt = 0
mdist = None
wdist = None
nk = None
nm = None
times = 0

#read words
with open("data/words") as words_file:
    words_file.readline()
    tmp = [each.split() for each in words_file]
    to_word = dict((int(e[0]), e[1]) for e in tmp)
    to_id = dict((e[1], int(e[0])) for e in tmp)

with open(savefile) as save_file:
    kkk, mcnt, wcnt = (int(e) for e in save_file.readline().split())
    dist_all = [[int(e) for e in line.split()] for line in save_file]
    mdist = dist_all[:mcnt]
    wdist = dist_all[mcnt:]
    nk = [sum(wdist[i][k] for i in range(wcnt)) for k in range(kkk)]
    nm = [sum(each) for each in mdist]
    times = 1

def append_file(f):
    global times
    f.readline()
    for i in range(mcnt):
        for k, n in zip(count(), (int(e) for e in f.readline().split())):
            mdist[i][k] += n
            nm[i] += n
    for i in range(wcnt):
        for k, n in zip(count(), (int(e) for e in f.readline().split())):
            wdist[i][k] += n
            nk[k] += n
    times += 1

def append_files(vers):
    for each in vers:
        with open("save/r"+str(each)) as f:
            append_file(f)

#append_files(range(4950, 5000, 10))

alpha, beta = 50.0/kkk, 0.01

t_map = [[i for i in range(wcnt) if wdist[i][k] > 0] 
                    for k in range(kkk)]

#print(kkk, mcnt, wcnt)
#print(len(mdist), len(wdist))
#print(nk)
#print(nm)

def top_n(n):
    return [sorted(t_map[k], key=lambda it: wdist[it][k], reverse=True)[:n]
                    for k in range(kkk)]

#print(times)

for each in top_n(50):
    print(' '.join(repr(to_word[e]) for e in each))
    print()
