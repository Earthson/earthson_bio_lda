#!/usr/bin/env python

import os
import re

def pattern_gen(pattern, repl):
    pattern = re.compile(pattern)
    return lambda text: pattern.sub(repl, text)

def tag_pattern_gen(tag, repl=' '):
    pattern = re.compile(r'<%s[^>]*>[\s\S]*?</%s>' % (tag, tag))
    return lambda text: pattern.sub(repl, text)

def tag_gen(tag):
    pattern = re.compile(r'<%s[^>]*>([\s\S]+)</%s>' % (tag, tag))
    def get_tag(text):
        mobj = pattern.search(text)
        if mobj is None:
            return ' '
        return ' ' + mobj.group(1) + ' '
    return get_tag

def split_words_by(pat=r'[^A-Z^a-z]+'):
    pat = re.compile(pat)
    def wrapper(txt):
        return pat.split(txt)
    return wrapper

split_words = split_words_by()

clear_script = tag_pattern_gen('script', repl=' ')
clear_style = tag_pattern_gen('style', repl=' ')
#get_title = tag_gen('title')
#get_body = tag_gen('body')
clear_tag = pattern_gen(r'<[^>]+>', ' ')
merge_blank = pattern_gen(r'\s+', ' ')


def getwords(xmlstr):
    words = split_words(clear_tag(xmlstr))
    return [each.lower() for each in words if each != '']
