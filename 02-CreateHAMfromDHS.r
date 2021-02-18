source("05-HAM_Util_Functions.r")

library(foreign)
library(tools)
library(countrycode)
###############################################################################
##Create Household Age Matrix
###############################################################################


AGE_DHS = HHAGE_DHS = HAM_DHS = list()

## Load_HAM_DHS Boolean flag decides whether to load the saved data or to recreate the dta
## As HAM creation could take lot of time, its preferred to load the 
## previously created data.
Load_HAM_DHS <- TRUE
if (Load_HAM_DHS)
{
  AGE_DHS <- readRDS("age_dhs.rds")
  HAM_DHS <- readRDS("ham_dhs.rds")
  HHAGE_DHS <- readRDS ("hhage_dhs.rds") 
  POPRATIO_STATEDHS <- readRDS("POPRATIO_STATEDHS.rds")
  state_popratios   <- readRDS("state_popratios.rds")
}else {
# Bihar
data = cleaner(state_bihar)
AGE_DHS[["BIHAR"]] = getAgeProfile(state_bihar)
HHAGE_DHS[["BIHAR"]] = getHHage(data)
HAM_DHS[["BIHAR"]] = getHAMdhs(ISO = "BIHAR")
image(HHAGE_DHS[["BIHAR"]])
image(HAM_DHS[["BIHAR"]])
rm(data,"BIHAR")

 
# kerala
data = cleaner(state_kerala)
AGE_DHS[["KERALA"]] = getAgeProfile(state_kerala)
HHAGE_DHS[["KERALA"]] = getHHage(data)
HAM_DHS[["KERALA"]] = getHAMdhs(ISO = "KERALA")
image(HHAGE_DHS[["KERALA"]])
image(HAM_DHS[["KERALA"]])
rm(data,"KERALA")

# Maharashtra
data = cleaner(state_Maha)
AGE_DHS[["MAHARASHTRA"]] = getAgeProfile(state_Maha)
HHAGE_DHS[["MAHARASHTRA"]] = getHHage(data)
HAM_DHS[["MAHARASHTRA"]] = getHAMdhs(ISO = "MAHARASHTRA")
image(HHAGE_DHS[["MAHARASHTRA"]])
image(HAM_DHS[["MAHARASHTRA"]])
rm(data,"MAHARASHTRA")

saveRDS(AGE_DHS, file = "age_dhs.rds")
saveRDS(HAM_DHS, file = "ham_dhs.rds")
saveRDS(HHAGE_DHS, file = "hhage_dhs.rds")
}
