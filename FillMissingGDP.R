for(i in 1:nrow(combined)){
  if(is.na(combined$ln_Country1_GDP[i]&is.na(combined$ln_Adjusted_Imports[i])==F)){
    combined$ln_Country1_GDP[i]<-unique_dat$Predicted_GDP[unique_dat$Country1==combined$Country1[i]&
                                                          unique_dat$Year==combined$Year[i]]  
  }  
  if(is.na(combined$ln_Country2_GDP[i]&is.na(combined$ln_Adjusted_Imports[i])==F)){
    combined$ln_Country2_GDP[i]<-unique_dat$Predicted_GDP[unique_dat$Country1==combined$Country2[i]&
                                                            unique_dat$Year==combined$Year[i]]  
  }  

}

