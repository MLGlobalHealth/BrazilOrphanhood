# Estimating orphanhood in Brazil during the COVID-19 pandemic

Code to reproduce the results in *"Regional and national estimates of children affected by all-cause and COVID-19-associated orphanhood and caregiver death in Brazil, by age and family circumstance."*

The primary analysis is performed in two steps:
 1. We generate samples of excess mortality and fertility, and then of orphanhood
 2. These samples are processed into the results

Generating the samples of fertility and orphanhood can be computationally expensive, particularly when estimating across many groups (such as by region and child-age, for example). We encourage you to leverage pre-generated samples available in the `/samples/` folder for any downstream-analysis.

<!-- 
### Primary scripts

- `generate_samples.R`: Sources and calls appropriate functions for generating samples of excess mortality, fertility, and orphanhood. Some of these functions are computationally expensive, so additional scripts `_hpc.R` and `_hpc_array.R` are included for running on HPC services.
- `example.R`: Example code for post-processing orphanhood samples. -->


## Structure of this repository

This repo contains the following scripts and folders:
 - `generate_samples.R`: Calls sampling functions from the `/R/` folder. Corresponding scripts `*_hpc.R` and `*_hpc_array.R` are included for running on HPC services. If re-running these scripts from scratch, they should be run in the order presented, as later scripts may depend on the output of earlier scripts.
 - `/data/`: Raw data (where available) and scripts to process them into consistent formats.
 - `/outputs/`: Figures from the paper and supplementary material
 - `/paper/`: R scripts to generate tables and figures for the paper
 - `/R/`: Functions to generate samples of excess mortality, fertility, and orphanhood
 - `/samples/`: Pre-generated samples of excess mortality, fertility, and orphanhood
