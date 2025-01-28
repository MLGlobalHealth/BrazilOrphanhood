# PNS 2019 data

We assume that the IBGE package has been used to download the PNS 2019 data and that the data have stored in the relative directory ``../largedata.nosync/pns2019/``. A simple example script to do this is given in ``download_pns2019.R`` (in this folder).

These data are processed into the file ``pns2019_processed.csv`` using the script ``process_pns2019.R``. This script reads in the raw data, extracts and renames the variables of interest, and saves a single dataframe in a memory-compact .RDS file.

Other documentation relating to the PNS dataset are also included in this folder.