#!/usr/bin/env python

from datetime import datetime, date, timedelta
from dataclasses import dataclass
from collections import OrderedDict

import numpy as np

from utils import enmo, zangle



@dataclass()
class ActiGraph:
    '''Class for keeping track of the actigraph metadata.'''
    start_datetime: datetime = datetime(1900, 1, 1, 0, 0, 0)
    download_datetime: datetime = datetime(1900, 1, 1, 0, 0, 0)
    freq_sample: float = 30
    freq_output: float = 1
    header_raw: str = ""
    signaldata: np.array = np.empty((1))
    enmo: np.array = np.empty((1))
    zangle: np.array = np.empty((1))

    def load(self, fname, headerlen=11, fin=30, fout=1):
        self.freq_sample = fin
        self.freq_sample = fout

        # Open file
        with open(fname) as fhandle:
            # Extract header
            hdr = []
            for i in range(headerlen):
                hdr += fhandle.readline()
            self.header_raw = "".join(hdr).split('\n')
            self.extract_header_info()

            # Prepare to extract signal data
            dsfactor = int(fin/fout)  # <- number of steps to group in sampling
            dat = []
            buff = []
            for idx, line in enumerate(fhandle.readlines()):
                # Collect samples until we fill the downsample buffer
                if (idx + 1) % dsfactor or dsfactor == 1:
                    buff += [np.array(line.strip().split(','),
                                      dtype=np.float16)]

                # If buffer is full, aggregate samples
                if len(buff) == dsfactor:
                    dat += [ np.mean(buff, axis=0)  ]
                    buff = []

        self.signaldata = np.array(dat)
        # self.extract_basic_info()

    def reload(self, data, header, fin=30, fout=1):
        self.header_raw = header
        self.signaldata = data

        self.extract_header_info()
        self.extract_basic_info()

    def extract_header_info(self):
        #TODO: extract all fields
        #TODO: make more general than these specific filetypes

        tmp_time = self.header_raw[3].split()[-1] + " " +\
                   self.header_raw[2].split()[-1]
        self.start_datetime = datetime.strptime(tmp_time, '%m/%d/%Y %H:%M:%S')

    def extract_basic_info(self):
        # Compute enmo and zangle
        self.enmo = enmo(self.signaldata)
        self.zangle = zangle(self.signaldata)

        # Set day labels for timeseries
        td_inds = int(timedelta(days=1).total_seconds() * self.freq_output)
        self.daytoloc = OrderedDict()
        self.loctoday = OrderedDict()
        self.daytoloc[0] = 0
        self.loctoday[0] = 0

        tmp = self.start_datetime.date() + timedelta(days=1)
        end_day0 = datetime.combine(tmp, datetime.min.time())
        diff = (end_day0 - self.start_datetime).total_seconds()
        diff_ind = int(diff * self.freq_output)
        for day, ind in enumerate(range(diff_ind,
                                        len(self.signaldata),
                                        td_inds)):
            self.daytoloc[day+1] = ind
            self.loctoday[ind] = day+1  # Move to func
        
        # Set weekday labels
        self.num_days = len(self.daytoloc.keys())
        self.weekday = OrderedDict()
        for day in range(self.num_days):
            tmp = self.start_datetime.date() + timedelta(days=day)
            self.weekday[day] = tmp.weekday()

    def get_timestamp(self, loc):
        pass

    def get_dat(self, loc):
        pass

    def get_timeofday(self, loc):
        pass

    def get_dayofweek(self, loc):
        pass

    def get_week(self, loc):
        pass


