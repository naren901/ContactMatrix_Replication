library(tidyr)
library(dplyr)
library(readr)

## Load_Saved_State_Data Boolean flag decides whether to load the saved data or to recreate the dta
## As creating state age wise population structure could consume time,
## load previously created data.
Load_Saved_State_Data <- TRUE

##Load state population age structured data, data file is Tab separated 
##Convert the data into 16 age groups
Load_State_Population_Age_Structure <- function(filepath)
{
  #Load Kerala population Age structured data, Tab separated data file
  Pop_Age_str_Full <- readr::read_tsv(file = filepath, col_names = TRUE)
  Pop_Age_str  <- Pop_Age_str_Full
  ## Drop Extra X7 column
  Pop_Age_str <- Pop_Age_str[,!(names(Pop_Age_str) %in% "X7")]
  
  ## Remove last row (Total Summary)
  Pop_Age_str <- Pop_Age_str[1:nrow(Pop_Age_str)-1, ]
  
  ### Collapse last 7 rows into single row, sum all details, 
  ### so that we've 16 age groups
  
  Pop_16_Age_str <-data.frame(
    agegroup = 1:16,
    Male = c(Pop_Age_str$Male[1:15], sum(Pop_Age_str$Male[16:22], na.rm = TRUE)),
    Female = c(Pop_Age_str$Female[1:15], sum(Pop_Age_str$Female[16:22], na.rm = TRUE)),
    TotalPersons = c(Pop_Age_str$TotalPersons[1:15], sum(Pop_Age_str$TotalPersons[16:22], na.rm = TRUE)),
    TotalShare = c(Pop_Age_str$`TotalShare(%)`[1:15], sum(Pop_Age_str$`TotalShare(%)`[16:22], na.rm = TRUE)),
    MPer100F = c(Pop_Age_str$Mper100F[1:15], sum(Pop_Age_str$Mper100F[16:22], na.rm =  TRUE))
  )
  
  return (Pop_16_Age_str)
}

if (Load_Saved_State_Data)
{
  Kerala_Pop_16_Age_str <- readRDS("Kerala_Pop_16_Age_str.rds")
  Bihar_Pop_16_Age_str <- readRDS("Bihar_Pop_16_Age_str.rds")
  Maharashtra_Pop_16_Age_str <- readRDS("Maharashtra_Pop_16_Age_str.rds")
  POPRATIO_STATEDHS <- readRDS("POPRATIO_STATEDHS.rds")
  state_popratios <- readRDS("state_popratios.rds")
}else {

  
#Load Kerala population age structured data Tab separated data file
#Convert the data into 16 age groups
Kerala_Pop_16_Age_str <- Load_State_Population_Age_Structure("States/Kerala Population Age structure.txt")
saveRDS(Kerala_Pop_16_Age_str, file = "Kerala_Pop_16_Age_str.rds")
View(Kerala_Pop_16_Age_str)

#Load Bihar population Age structured data, Tab separated data file
Bihar_Pop_16_Age_str <- Load_State_Population_Age_Structure("States/Biahr Population Age Structure.txt")
View(Bihar_Pop_16_Age_str)
saveRDS(Bihar_Pop_16_Age_str, file = "Bihar_Pop_16_Age_str.rds")

Maharashtra_Pop_16_Age_str <- Load_State_Population_Age_Structure("States/Maharashtra Population Age Structure.txt") 
View(Maharashtra_Pop_16_Age_str)
saveRDS(Maharashtra_Pop_16_Age_str, file = "Maharashtra_Pop_16_Age_str.rds")

########################################################
###Create Population ration model from HAM Data
########################################################
Popratio_kl <- Kerala_Pop_16_Age_str$TotalShare/100
Popratio_br <- Bihar_Pop_16_Age_str$TotalShare/100
Popratio_mh <- Maharashtra_Pop_16_Age_str$TotalShare/100

state_popratios<- data.frame(factor('KERALA', 'BIHAR', 'MAHARAHSTRA'),
                             factor('KERALA', 'BIHAR', 'MAHARAHSTRA'),
                             factor(2011,2011,2011),
                             c(Popratio_kl[1], Popratio_br[1], Popratio_mh[1]),
                             c(Popratio_kl[2], Popratio_br[2], Popratio_mh[2]),
                             c(Popratio_kl[3], Popratio_br[3], Popratio_mh[3]),
                             c(Popratio_kl[4], Popratio_br[4], Popratio_mh[4]),
                             c(Popratio_kl[5], Popratio_br[5], Popratio_mh[5]),
                             c(Popratio_kl[6], Popratio_br[6], Popratio_mh[6]),
                             c(Popratio_kl[7], Popratio_br[7], Popratio_mh[7]),
                             c(Popratio_kl[8], Popratio_br[8], Popratio_mh[8]),
                             c(Popratio_kl[9], Popratio_br[9], Popratio_mh[9]),
                             c(Popratio_kl[10],Popratio_br[10],Popratio_mh[10]),
                             c(Popratio_kl[11],Popratio_br[11],Popratio_mh[11]),
                             c(Popratio_kl[12],Popratio_br[12],Popratio_mh[12]),
                             c(Popratio_kl[13],Popratio_br[13],Popratio_mh[13]),
                             c(Popratio_kl[14],Popratio_br[14],Popratio_mh[14]),
                             c(Popratio_kl[15],Popratio_br[15],Popratio_mh[15]),
                             c(Popratio_kl[16],Popratio_br[16],Popratio_mh[16]),
                             c(sum (Kerala_Pop_16_Age_str$TotalPersons),
                               sum (Bihar_Pop_16_Age_str$TotalPersons),
                               sum (Maharashtra_Pop_16_Age_str$TotalPersons) )
)

names (state_popratios) =c ( "iso3c","countryname","year",
                             "age0","age5","age10", "age15",    
                             "age20","age25","age30","age35",
                             "age40", "age45","age50","age55",
                             "age60","age65","age70", "age75",
                             "total")
state_popratios$iso3c <- c('KERALA', 'BIHAR', 'MAHARAHSTRA')
state_popratios$countryname <- c('KERALA', 'BIHAR', 'MAHARAHSTRA')

saveRDS(state_popratios, file =  "state_popratios.rds")

#################################################################
##Adjust HAM matrix my POPULATION RATIOs
#################################################################
getPopratio = function(HAM,pop){
  apply(HAM[1:16,1:16],2,function(x) x/pop[1:16])
}


POPRATIO_STATEDHS = list()
pop_ratio_states <- matrix(nrow = 16, ncol = 3)
pop_ratio_states[,1] <- Popratio_br
pop_ratio_states[,2] <- Popratio_kl
pop_ratio_states[,3] <- Popratio_mh

POPRATIO_STATEDHS[["BIHAR"]] = getPopratio(HAM_DHS$BIHAR[1:16,1:16],pop_ratio_states[,1])
POPRATIO_STATEDHS[["KERALA"]] = getPopratio(HAM_DHS$BIHAR[1:16,1:16],pop_ratio_states[,2])
POPRATIO_STATEDHS[["MAHARASHTRA"]] = getPopratio(HAM_DHS$BIHAR[1:16,1:16],pop_ratio_states[,3])
### Save POPRATIO DHS
saveRDS(POPRATIO_STATEDHS, "POPRATIO_STATEDHS.rds")
}
 
  
 