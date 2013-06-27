#!/usr/bin/env python

from word_utils import *
import re

_strip_pat0 = re.compile(r'<[^>]*?>')#|{{[^}]*?}}')
_strip_pat2 = re.compile(r"[^0-9a-zA-Z_-]+|-[-]+|- | -|_[_]+|_ | _")
_strip_pat3 = re.compile(r'[-0-9]+ | \d+? | \d+|[\^]+')
_strip_pat4 = re.compile(r' [ ]+| -|- |_ | _')

_url_pat = re.compile(
    r'^https?://'  # http:// or https://
    r'(?:(?:[A-Z0-9](?:[A-Z0-9-]{0,61}[A-Z0-9])?\.)+[A-Z]{2,6}\.?|'  # domain...
    r'localhost|'  # localhost...
    r'\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3})' # ...or ip
    r'(?::\d+)?'  # optional port
    r'(?:/?|[/?]\S+)$', re.IGNORECASE)

def sub_all(*pats):
    def sub_func(txt):
        for each in pats:
            txt = each.sub(' ', txt)
        return txt
    return sub_func

txt_sub = sub_all(_url_pat, _strip_pat0, _strip_pat2, _strip_pat3, _strip_pat4)

def strip_tags(text):
    text = txt_sub(text)
    text = text.strip()
    return text.lower()

def trans_body(doc):
    return strip_tags(doc)

def translate(doc):
    return strip_tags(doc)
