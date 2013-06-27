#!/usr/bin/env python

to_word = None
to_id = None

with open("../data/words") as words_file:
    words_file.readline()
    tmp = [each.split() for each in words_file]
    to_word = dict((int(e[0]), e[1]) for e in tmp)
    to_id = dict((e[1], int(e[0])) for e in tmp)
