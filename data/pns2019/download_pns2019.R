

# This script downloads and saves the raw PNS 2019 data. The code relies on the PNSIBGE package.

rm(list=ls())

library(PNSIBGE)
library(tidyverse)

DATA_DIR = "data/pns2019/pns2019_rawdata/"

data = get_pns(2019, savedir=DATA_DIR)
