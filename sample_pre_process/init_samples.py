#!/usr/bin/env python


from word_info import to_word, to_id
from translate import *

with open("sample_words") as ff:
    ws = translate(ff.read()).split()
    #print(ws)
    ws = [e for e in ws if e in to_id]
    ids = [to_id[e] for e in ws]
    print(ws)
    with open("reduced_words", "w") as rd:
        #print(len(ws), file=rd)
        print('\n'.join("-1 "+str(each) for each in ids), file=rd)
