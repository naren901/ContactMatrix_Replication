library(haven)
library(dplyr)


## Load_Saved_DHS_data Boolean flag decides whether to load the saved data or to recreate the dta
## As HAM creation could take lot of time, its preferred to load the 
## previously created data.
Load_Saved_DHS_data <- TRUE

if (Load_Saved_DHS_data)
{
  state_bihar   <- readRDS('Bihar_dhs_data.rds')
  state_kerala  <- readRDS('Kerala_dhs_data.rds')
  state_Maha    <- readRDS('Maha_dhs_data.rds')
}else{
##Increase the R memory limit, to load dhs data
##Defualut limit of 7976 is not enough to load india DHS datafile  
gc()
#memory.limit() 
memory.limit(24000)  
 
india_hh_dhs_2015 <- read_dta('../DHS-India House Hold Data/IAPR74DT/IAPR74FL.DTA')

##Bihar state
state_bihar <- india_hh_dhs_2015 %>% dplyr::filter(hv024 %in% c(5))
##kerala state dhs
state_kerala <- india_hh_dhs_2015 %>% dplyr::filter(hv024 %in% c(17))
##maharashtra state
state_Maha<- india_hh_dhs_2015 %>% dplyr::filter(hv024 %in% c(20))

saveRDS(state_bihar, file = 'Bihar_dhs_data.rds')
saveRDS(state_kerala, file = 'Kerala_dhs_data.rds')
saveRDS(state_Maha, file = 'Maha_dhs_data.rds')
}
 
## No of households participated in the sruvey for each state
bihar_hhold_no  <- length(unique(state_bihar$hhid))
kerala_hhold_no <- length(unique(state_kerala$hhid))
Maha_hhold_no   <- length(unique(state_Maha$hhid))
 


 