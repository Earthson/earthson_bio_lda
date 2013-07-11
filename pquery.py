#!/usr/bin/env python

from itertools import *
from functools import *

to_word = None
to_id = None

kkk = 256
mcnt = 0
tcnt = 0
wcnt = 0

with open("data/info") as ff:
    mcnt, tcnt, wcnt = (int(each) for each in ff.readline().split())

alpha, beta = 50.0/kkk, 0.01

#print(times)

with open("data/doc_ids") as tmpfile:
    doc_ids = [int(each) for each in tmpfile.read().split()]

def with_save(rd):
    fname = "save/distances"+str(rd)
    with open(fname) as tmpfile:
        return [float(each) for each in tmpfile.read().split()]

from db_utils import *

for rd in range(499, 5000, 500):
    doc_dis = with_save(rd)
    print("#Round: %d\n\n\n" % rd)
    for eid, edis in sorted(zip(doc_ids, doc_dis), key = lambda x: x[1])[:10]:
        dtmp = biodata.origin_doc.find_one({"_id":eid})
        print(r"%s & %s\\\hline" % (edis, dtmp["title"]))
#    print("########")
#    print(eid, edis)
#    print("# Title")
#    print(dtmp["title"])
#    print("# Abstract")
#    print(dtmp["abstract"])
#    print("# Text")
#    print(dtmp["body"])

