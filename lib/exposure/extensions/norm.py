#!/usr/bin/env python3
from numpy import linalg

def norm(pt):
    return linalg.norm([pt[x] for x in pt])
