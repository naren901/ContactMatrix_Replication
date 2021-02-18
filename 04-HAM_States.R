source("05-HAM_Util_Functions.r")
load('PremDATA/HAMweights.rdata')
load('PremDATA/popratio_polymoddhs.rdata')
load('PremDATA/KAPPA_HOME.RData')

 
 
load_save_contact_at_home <- TRUE
if (load_save_contact_at_home)
{
  contact_at_home <- readRDS("contact_at_home.rds")
}else
{
  HAM_STATES_IND = list()
  HAM_STATES_IND[["KERALA"]] <-  getHAMWORLD(Popratio_kl)[['HAM']]
  HAM_STATES_IND[["BIHAR"]]  <-  getHAMWORLD(Popratio_br)[['HAM']]
  HAM_STATES_IND[["MAHARASHTRA"]] <- getHAMWORLD(Popratio_mh)[['HAM']]
  
  contact_at_home = list()
  contact_at_home[["KERALA"]] <- HHcontact(HAM_STATES_IND$KERALA)
  contact_at_home[["BIHAR"]] <-  HHcontact(HAM_STATES_IND$BIHAR)
  contact_at_home[["MAHARASHTRA"]] <- HHcontact(HAM_STATES_IND$MAHARASHTRA)
  
  saveRDS(contact_at_home, "contact_at_home.rds")
  
}
 ### PLOT CONTACT DATA
getPlot.contact.full(nrow=16,ncol=16,
                      cols=colour_blue(contact_at_home[["KERALA"]], 
                                       max.value = ceiling(max(contact_at_home[["KERALA"]]))),
                      LOCATION = 'Home',
                      a=5,
                      INDEX = 'KL')
getPlot.contact.full(nrow=16,ncol=16,
                      cols=colour_blue(contact_at_home[["BIHAR"]], 
                                       max.value = ceiling(max(contact_at_home[["BIHAR"]]))),
                      LOCATION = 'Home',
                      a=5,
                      INDEX = 'BR')
getPlot.contact.full(nrow=16,ncol=16,
                     cols=colour_blue(contact_at_home[["MAHARASHTRA"]], 
                                      max.value = ceiling(max(contact_at_home[["MAHARASHTRA"]]))),
                     LOCATION = 'Home',
                     a=5,
                     INDEX = 'MH')

