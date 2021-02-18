#########################################################################
## Acknowlegments:Below functions are from Petra Klepac (petrakle) 
## func to create HHAGE, HAM data 
## These functions Creat Household Age Matrices (HAM)
## Compute HouseHold Age matrices
## Plot House HOld Contacts as a GRID image
#########################################################################

cleaner = function(hh)
{
  unihh=unique(hh$hhid)
  age1=age2=id1=id2=vector("list", length = length(unihh))
  for(h in 1:length(unihh))
  {
    if(h %% 1000 == 0) print(h)
    J=which(hh$hhid==unihh[h])
    maxhh = hh$hv009[J[1]]
    if(maxhh > 1)
    {
      i1 = rep(1:maxhh,maxhh)
      i2 = rep(1:maxhh,each=maxhh)
      ages = hh$hv105[J]
      a1 = ages[i1]
      a2 = ages[i2]
      age1[[h]]=a1
      age2[[h]]=a2
      id1[[h]]=i1
      id2[[h]]=i2
    }
  }
  hhout = list(age1=unlist(age1),
               age2=unlist(age2),
               id1=unlist(id1),
               id2=unlist(id2))
  return(hhout)
}

getHHage = function(data)
{ 
  
  id1 = data$id1
  id2 = data$id2
  age1 = data$age1
  age2 = data$age2
  i=which(id1==id2)
  age1=age1[-i]
  age2=age2[-i]
  id1=id1[-i]
  id2=id2[-i]
  
  M=matrix(0,100,100)
  M5=matrix(0,20,20)
  
  for(i in 1:100)
  {
    cat(i)
    for(j in 1:100)M[i,j]=sum((age1 == i)*(age2 == j),na.rm = TRUE)
  }
  
  for(i in 1:100)
  {
    i5=ceiling(i/5)
    for(j in 1:100)
    {
      j5=ceiling(j/5)
      M5[i5,j5]=M5[i5,j5]+M[i,j]
    }
  }
  return(M5)
}

###Gets the age profile of (Twenty 5 Year age gropus) house hold members
getAgeProfile = function(hh)
{
  tabulate((1+floor(hh$hv105/5)))
}

getHAMdhs = function(ISO,AGEMAX=16)
{
  hhage = data.matrix(HHAGE_DHS[[ISO]])
  ageindiv = AGE_DHS[[ISO]]
  HH5 = matrix(0,20,20)
  for(indiv in 1:20)
  {
    HH5[indiv,] = hhage[indiv,]/ageindiv[indiv]
  }
  HH5 = HH5[1:AGEMAX,1:AGEMAX]
  # image(HH5)
  return(HH5)
}

getHAMWORLD = function(PopState){
  finalweights = HAMweights[["IND"]]
  i=1;weightedsumpopratio = POPRATIO_POLYMODDHS[[names(finalweights)[i]]] * finalweights[i]
  for(i in 2:length(finalweights))
  { weightedsumpopratio = weightedsumpopratio + POPRATIO_POLYMODDHS[[names(finalweights)[i]]] * finalweights[i]}
  HAM = estimateHH(weightedsumpopratio,PopState) 
  piagevalidated = rowSums(PopState * HAM)
  correlation_pi_pa = (cor(piagevalidated,PopState))
  output = list(HAM,PopState,piagevalidated,correlation_pi_pa)
  names(output) = c('HAM','popage','piage','correlation_pi_pa')
  return(output)
} 


HHcontact = function(HH){
  contact_home =  data.matrix(KAPPA_HOME$POLYMOD[1:16,1:16]*(HH[1:16,1:16]))
  return(contact_home)
}

##########################################
### Function to plot contact matrices as gri
##########################################
getPlot.contact.full = function(nrow,ncol,cols,a,LOCATION,INDEX, CONTACT = TRUE,FONTSIZEPLUS = 0){
  pushViewport(plotViewport(c(1.5,1.5,1,.8),xscale=c(0,nrow*a),yscale=c(0,ncol*a)))
  grid.rect()
  grid.xaxis(at=seq(0,nrow/2,by=1)*(a*2),label=T,gp=gpar(fontsize=6+FONTSIZEPLUS))
  grid.yaxis(at=seq(0,ncol/2,by=1)*(a*2),label=T,gp=gpar(fontsize=6+FONTSIZEPLUS))
  
  if(CONTACT) grid.text('Age group of individual',y=unit(-2.1,'lines'),gp=gpar(fontsize=7+FONTSIZEPLUS))
  if(CONTACT) grid.text('Age group of contact',x=unit(-2.4,'lines'),rot=90,gp=gpar(fontsize=7+FONTSIZEPLUS))
  if(!CONTACT) grid.text('Age of household member',y=unit(-2.1,'lines'),gp=gpar(fontsize=7+FONTSIZEPLUS))
  if(!CONTACT) grid.text('Age of household member',x=unit(-2.4,'lines'),rot=90,gp=gpar(fontsize=7+FONTSIZEPLUS))
  
  x <- (1:ncol)/ncol
  y <- (1:nrow)/nrow
  right <- rep(x, nrow)
  top <- rep(y, each=ncol)
  grid.rect(x=right, y=top,width=1/ncol, 
            height=1/nrow,just=c("right", "top"),
            gp=gpar(col=cols,fill=cols))
  if(CONTACT) grid.text(paste(LOCATION),0.5,unit(1,'npc')+unit(-0.8,'lines'), gp=gpar(fontsize=8.5+FONTSIZEPLUS,col='black'))
  if(!CONTACT) grid.text(paste(LOCATION),0.5,unit(1,'npc')+unit(0.5,'lines'), gp=gpar(fontsize=8.5+FONTSIZEPLUS,col='black'))
  grid.text(paste0('(',INDEX,')'),unit(1,'lines'),unit(1,'npc')+unit(-0.8,'lines'),gp=gpar(fontsize=6.5+FONTSIZEPLUS,col='black'))
  grid.rect(gp=gpar(col='black',fill=NA)) 
  popViewport()
}
