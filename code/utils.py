#!/usr/bin/env python

import numpy as np


def enmo(data, axis=1):
    enmo = np.linalg.norm(data, axis=axis) - 1
    l = np.where(enmo < 0)
    enmo[l] = 0
    return enmo


def zangle(data):
    # From: https://www.nature.com/articles/s41598-018-31266-z
    tri = data[:,2]/(np.square(data[:,0]) +\
                     np.square(data[:,1]) +\
                     np.finfo(np.float16).eps)
    return np.rad2deg(np.arctan(tri))

