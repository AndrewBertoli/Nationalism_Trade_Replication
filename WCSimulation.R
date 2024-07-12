setwd("~/Dropbox/WCNaturalExperiment/Data_and_Code")
real_groups<-read.csv("WC_Groups.csv",stringsAsFactors=FALSE,sep=";")

groups1930<-real_groups[1:4,1:5]
groups1934<-real_groups[5:12,1:5]
groups1938<-real_groups[13:19,1:5]
groups1950<-real_groups[20:23,1:5]
groups1954<-real_groups[24:39,1:5]
groups1958<-real_groups[40:43,1:5]
groups1962<-real_groups[44:47,1:5]
groups1966<-real_groups[48:51,1:5]
groups1970<-real_groups[52:55,1:5]
groups1974<-real_groups[56:59,1:5]
groups1978<-real_groups[60:63,1:5]
groups1982<-real_groups[64:69,1:5]
groups1986<-real_groups[70:75,1:5]
groups1990<-real_groups[76:81,1:5]
groups1994<-real_groups[82:87,1:5]
groups1998<-real_groups[88:95,1:5]
groups2002<-real_groups[96:103,1:5]
groups2006<-real_groups[104:111,1:5]
groups2010<-real_groups[112:119,1:5]
groups2014<-real_groups[120:127,1:5]
groups2018<-real_groups[128:135,1:5]

groups<-list(groups1930,groups1934,groups1938,groups1950,
             groups1954,groups1958,groups1962,groups1966,
             groups1970,groups1974,groups1978,groups1982,
             groups1986,groups1990,groups1994,groups1998,
             groups2002,groups2006,groups2010,groups2014,
             groups2018)

years<-c(1930,1934,1938,1950,1954,1958,1962,1966,
         1970,1974,1978,1982,1986,1990,1994,1998,
         2002,2006,2010,2014,2018)

source("GetPairs.R")


get_info=function(v){
  return(all_pairs[all_pairs[,1]%in%c(v[1],v[2])&all_pairs[,2]%in%c(v[1],v[2])&all_pairs[,3]==v[3],])
}

all_pairs<-read.csv("All_Pairs_July24.csv", stringsAsFactors=F)
real_data<-read.csv("RealData_July24.csv",stringsAsFactors=F)
real_data_full<-read.csv("RealDataFull_July24.csv",stringsAsFactors=F)


perm_data=matrix(0,nrow=100000,ncol=42)

set.seed(0)

for(z in 1:nrow(perm_data)){
  
  source("DrawSimulation.R")
  source("MakeFakePairs.R")
  
  perm_data_full_all<-do.call(rbind,apply(perm_pairs,1,get_info))
  
  perm_data_full_all<-perm_data_full_all[perm_data_full_all$Country1!=perm_data_full_all$Country2,]
  
  perm_data_full<-perm_data_full_all[is.na(perm_data_full_all$Bi_Trade_Pre)==FALSE&is.na(perm_data_full_all$Bi_Trade_Post)==FALSE,]
  
perm_data[z,1:21]<-with(perm_data_full,
                  c(mean(ln_Adjusted_Trade-ln_Bi_Trade_Pre,na.rm=T),
                    mean(Percent_Change_Adjusted,na.rm=TRUE), 
                    mean(y,na.rm=TRUE), 
                    mean(Total_Disputes_Before),
                    mean(Any_Disputes_Before),
                    mean(Both_Dem,na.rm=TRUE), 
                    mean(Both_NonDem,na.rm=TRUE),
                    mean(Diff_Regime,na.rm=TRUE),
                    mean(Both_GATT,na.rm=TRUE), 
                    mean(One_GATT,na.rm=TRUE),
                    mean(Both_EU,na.rm=TRUE), 
                    mean(One_EU,na.rm=TRUE), 
                    mean(Colony,na.rm=TRUE),
                    mean(Sibling,na.rm=TRUE),
                    mean(Dist,na.rm=TRUE),
                    mean(ln_Dist,na.rm=TRUE), 
                    mean(Contiguous,na.rm=TRUE), 
                    mean(Alliance_Year_Before,na.rm=TRUE),
                    mean(ln_Adjusted_Trade_Prev-ln_Bi_Trade_M2,na.rm=TRUE),
                    mean(Percent_Change_Prev_Adjusted,na.rm=TRUE),
                    mean(ym1,na.rm=TRUE)))
  
  perm_data[z,22:42]<-with(perm_data_full[perm_data_full$Both_Soccer==1,],
                          c(mean(ln_Adjusted_Trade-ln_Bi_Trade_Pre,na.rm=T),
                            mean(Percent_Change_Adjusted,na.rm=TRUE), 
                            mean(y,na.rm=TRUE), 
                            mean(Total_Disputes_Before),
                            mean(Any_Disputes_Before),
                            mean(Both_Dem,na.rm=TRUE), 
                            mean(Both_NonDem,na.rm=TRUE),
                            mean(Diff_Regime,na.rm=TRUE),
                            mean(Both_GATT,na.rm=TRUE), 
                            mean(One_GATT,na.rm=TRUE),
                            mean(Both_EU,na.rm=TRUE), 
                            mean(One_EU,na.rm=TRUE), 
                            mean(Colony,na.rm=TRUE),
                            mean(Sibling,na.rm=TRUE),
                            mean(Dist,na.rm=TRUE),
                            mean(ln_Dist,na.rm=TRUE), 
                            mean(Contiguous,na.rm=TRUE), 
                            mean(Alliance_Year_Before,na.rm=TRUE),
                            mean(ln_Adjusted_Trade_Prev-ln_Bi_Trade_M2,na.rm=TRUE),
                            mean(Percent_Change_Prev_Adjusted,na.rm=TRUE),
                            mean(ym1,na.rm=TRUE)))
  
  if(z%%100==0){
  
  print(z)
  
  print("Change in ln(Trade)")  
  print(mean(perm_data[1:z,1]<=as.numeric(real_data[1])))
  
  print("Change in ln(Trade) - Soccer")  
  print(mean(perm_data[1:z,22]<=as.numeric(real_data[22])))
  
  print("Percent Change") 
  print(mean(perm_data[1:z,2]<=as.numeric(real_data[2])))
  
  print("Percent Change - Soccer") 
  print(mean(perm_data[1:z,23]<=as.numeric(real_data[23])))

  print("Trade Drop - y") 
  print(mean(perm_data[1:z,3]>=as.numeric(real_data[3])))
  
  print("Trade Drop - y - Soccer") 
  print(mean(perm_data[1:z,24]>=as.numeric(real_data[24])))
  
  }
  
}


perm_data<-as.data.frame(perm_data)

colnames(perm_data)<-c("ln_Trade_Change_Adjusted","Percent_Change_Adjusted","y",
                       "Total_Disputes_Before","Any_Disputes_Before","Both_Dem","Both_NonDem",
                       "Diff_Regime","Both_GATT","One_GATT","Both_EU","One_EU","Colony",
                       "Sibling","Dist","ln_Dist","Contiguous","Alliance_Year_Before",
                       "ln_Trade_Change_Adjusted_Prev","Percent_Change_Adjusted_Prev","ym1",
                       "Soccer_ln_Trade_Change_Adjusted","Soccer_Percent_Change_Adjusted",
                       "Soccer_y","Soccer_Total_Disputes_Before","Soccer_Any_Disputes_Before",
                       "Soccer_Both_Dem","Soccer_Both_NonDem","Soccer_Diff_Regime",
                       "Soccer_Both_GATT","Soccer_One_GATT","Soccer_Both_EU","Soccer_One_EU",
                       "Soccer_Colony","Soccer_Sibling","Soccer_Dist","Soccer_ln_Dist",
                       "Soccer_Contiguous","Soccer_Alliance_Year_Before",
                       "Soccer_ln_Trade_Change_Adjusted_Prev",
                       "Soccer_Percent_Change_Adjusted_Prev","Soccer_ym1")


write.csv(perm_data,"PermData_July24.csv",row.names=F)

rm(list=ls())
