# 7. Calculating lnCVR and sampling variance
#-----------------------------------------------------------------------------#
calclnCVR <- function(SDtreat, SDcont, mtreat, mcont, ntreat, ncont){
  
  lnCVR <- log((SDtreat/mtreat) / (SDcont/mcont)) + (1/(2 * (ntreat - 1))) - (1/(2 * (ncont - 1)))
  
  return(lnCVR)
  
}

calcsamplnCVR <- function(SDtreat, SDcont, mtreat, mcont, ntreat, ncont, CID=F){
  
  if(CID == F){
    
    uniquemcont <- mcont 
    uniqueSDcont <- SDcont  
    
  }else{
    
    
    uniqueCID <- unique(CID)
    locations <- match(uniqueCID, CID)
    uniquemcont <- mcont[locations]
    uniqueSDcont <- SDcont[locations]
    
  }
  
  samplnCVR <- (SDcont^2/(ncont * mcont^2)) + (1/(2 * (ncont - 1))) - (2 * (cor(log(uniquemcont), log(uniqueSDcont))) * sqrt(((SDcont^2) / (ncont * mcont^2)) * (1 / (2 * (ncont - 1))))) + (SDtreat^2 / (ntreat * mtreat^2)) + (1 / (2 * (ntreat - 1))) - (2 * (cor(log(mtreat), log(SDtreat))) * sqrt(((SDtreat^2) / (ntreat * mtreat^2)) * (1 / (2 * (ntreat - 1)))))
  
  return(samplnCVR)
  
}
