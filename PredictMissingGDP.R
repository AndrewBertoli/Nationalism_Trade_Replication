

d1<-sample[,c("Country1","Year","Irst1","Tpop1","Democracy1",
              "Country1_GATT","Country1_EU","ln_Country1_GDP")]

d2<-sample[,c("Country2","Year","Irst2","Tpop2","Democracy2",
              "Country2_GATT","Country2_EU","ln_Country2_GDP")]

colnames(d2)<-colnames(d1)

dat<-rbind(d2,d1)

mean(is.na(dat$ln_Country1_GDP))


unique_dat<-dat[-which(duplicated(paste(dat$Country1,dat$Year))),]

head(unique_dat)

prediction<-lm(ln_Country1_GDP~Irst1+Tpop1+Democracy1+Year,unique_dat)

unique_dat$Predicted_GDP<-predict(prediction,unique_dat)

head(unique_dat)

for(i in 1:nrow(sample)){
  if(is.na(sample$ln_Country1_GDP[i])){
    sample$ln_Country1_GDP[i]<-unique_dat$Predicted_GDP[unique_dat$Country1==sample$Country1[i]&
                                                          unique_dat$Year==sample$Year[i]]  
  }  
  if(is.na(sample$ln_Country2_GDP[i])){
    sample$ln_Country2_GDP[i]<-unique_dat$Predicted_GDP[unique_dat$Country1==sample$Country2[i]&
                                                          unique_dat$Year==sample$Year[i]]  
  }    
  
}

