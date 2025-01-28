
# This script is used to generate the samples required for results.R
# It is not recommended to run all samplers. Just choose the ones you need. The male fertility and co-resident samplers take a long time to run.

rm(list=ls())

library(tidyverse)
library(zoo)
library(viridis)
library(cowplot)
library(parallel)
library(doParallel)

source("R/supportFunctions.R")

cl <- makeCluster(5)
registerDoParallel(cl)

# Excess mortality (these functions save samples to the /samples/ folder)
source("R/sampleExcessMortality.R")
sampleExcessMortality()
loadAllCauseMortality()
loadMonthlyExcessMortality()
loadMonthlyAllCauseMortality()

# Female fertility
source("R/sampleFemaleFertility.R")
sampleFemaleFertility(by_child_age=FALSE)
sampleFemaleFertility(by_child_age=TRUE)

 # Male fertility
source("R/sampleMaleFertility.R")
sampleMaleFertility(by_child_age=FALSE)
sampleMaleFertility(by_child_age=TRUE)

# Orphanhood
source("R/sampleOrphanhoodParental.R")
getPartnerAgeDist()
getPartnerAgeDist(regional=TRUE)
parentalOrphanhood = sampleParentalOrphanhood(mortality="allcause")
parentalOrphanhood = sampleParentalOrphanhood(mortality="excess")

# To estimate orphanhood by child-age, run the following command as an array job over CURRENT_AGE
# parentalOrphanhood = sampleParentalOrphanhood(mortality="excess", by_child_age=TRUE, current_child_age=CURRENT_AGE)
# and then call source("R/aggregateChildAgeSamples.R"), aggregateChildAgeSamples() 

# Co-resident orphanhood. See R/sampleOrphanhoodCoresident.R for options. Run by_child_age as an array job as specified above.
source("R/sampleOrphanhoodCoresident.R")
coresidentOrphanhood = sampleCoresidentOrphanhood(N=5, mortality="allcause", output="any")

# Joint parental and co-res
source("R/sampleDoubleCoresAndParental.R")
sampleCoresAndParentalOrphanhood(mortality="allcause")

# Generate samples for supplementary material
source("R/estimateMaleFertilityAnnualRates.R")
estimateMaleAgeSpecificFertilityRates(regional=FALSE)
estimateMaleAgeSpecificFertilityRates(regional=TRUE)

stopCluster(cl)

