setwd("~/Downloads/Nationalism_Trade_Replication-main")
real_groups<-read.csv("FIFAGroups_Expanded.csv",stringsAsFactors=FALSE,sep=";")

# We will now subset to the groups.
euro1980=real_groups[136:137,]
euro1984=real_groups[138:139,]
euro1988=real_groups[140:141,]
euro1992=real_groups[142:143,]
euro1996=real_groups[144:147,]
euro2000=real_groups[148:151,]
euro2004=real_groups[152:155,]
euro2008=real_groups[156:159,]
c=real_groups[160:161,] # ?????????
asia1976=real_groups[162:163,]
asia1980=real_groups[164:165,]
asia1984=real_groups[166:167,]
asia1988=real_groups[168:169,]
asia1992=real_groups[170:171,]
asia1996=real_groups[172:174,]
asia2000=real_groups[175:177,]
asia2004=real_groups[178:181,]
asia2007=real_groups[182:185,]
africa1968=real_groups[186:187,]
africa1970=real_groups[188:189,]
africa1972=real_groups[190:191,]
africa1974=real_groups[192:193,]
africa1976=real_groups[194:195,]
africa1978=real_groups[196:197,]
africa1980=real_groups[198:199,]
africa1982=real_groups[200:201,]
africa1984=real_groups[202:203,]
africa1986=real_groups[204:205,]
africa1988=real_groups[206:207,]
africa1990=real_groups[208:209,]
africa1992=real_groups[210:213,]
africa1994=real_groups[214:217,]
africa1996=real_groups[218:221,]
africa1998=real_groups[222:225,]
africa2000=real_groups[226:229,]
africa2002=real_groups[230:233,]
africa2004=real_groups[234:237,]
africa2006=real_groups[238:241,]
africa2008=real_groups[242:245,]
africa2010=real_groups[246:249,]
africa2012=real_groups[250:253,]
euro2012=real_groups[254:257,]
africa2013=real_groups[258:261,]
africa2015=real_groups[262:265,]
africa2017=real_groups[266:269,]
africa2019=real_groups[270:275,]
africa2022=real_groups[276:281,]
euro2016=real_groups[282:287,]
euro2021=real_groups[288:293,]

# Next, we can put the groups into a list.
groups<-list(euro1980,euro1984,euro1988,euro1992,
            euro1996,euro2000,euro2004,euro2008,
            euro2012,euro2016,euro2021)

# We will also make a vector with corresponding years for the list.
years<-c(1992,1996,2000,
        2004,2008,2012,2016,2021) # 1980,1984,1988,
        
source("GetPairs.R")


get_info=function(v){
  return(all_pairs[all_pairs[,1]%in%c(v[1],v[2])&all_pairs[,2]%in%c(v[1],v[2])&all_pairs[,3]==v[3],])
}

all_pairs<-read.csv("All_Pairs_Regional_9_25.csv", stringsAsFactors=F)
all_pairs<-all_pairs[all_pairs$Event=="Europe",]
real_data<-read.csv("RealData_Euro_Sept25.csv",stringsAsFactors=F)
real_data_full<-read.csv("RealDataFull_Euro_Sept25.csv",stringsAsFactors=F)


perm_data=matrix(0,nrow=1000,ncol=21)

set.seed(0)

for(z in 1:nrow(perm_data)){
  
  source("DrawSimulationEurope.R")
  source("MakeFakePairsEurope.R")
  
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
  
  if(z%%100==0){
  
  print(z)
  
  print("Change in ln(Trade)")  
  print(mean(perm_data[1:z,1]<=as.numeric(real_data[1])))
    
  print("Percent Change") 
  print(mean(perm_data[1:z,2]<=as.numeric(real_data[2])))
  
  }
  
}


perm_data<-as.data.frame(perm_data)

colnames(perm_data)<-c("ln_Trade_Change_Adjusted","Percent_Change_Adjusted","y",
                       "Total_Disputes_Before","Any_Disputes_Before","Both_Dem","Both_NonDem",
                       "Diff_Regime","Both_GATT","One_GATT","Both_EU","One_EU","Colony",
                       "Sibling","Dist","ln_Dist","Contiguous","Alliance_Year_Before",
                       "ln_Trade_Change_Adjusted_Prev","Percent_Change_Adjusted_Prev","ym1")


write.csv(perm_data,"PermDataEurope_Sept25.csv",row.names=F)













setwd("~/Downloads/Nationalism_Trade_Replication-main")
real_groups<-read.csv("FIFAGroups_Expanded.csv",stringsAsFactors=FALSE,sep=";")

# We will now subset to the groups.
euro1980=real_groups[136:137,]
euro1984=real_groups[138:139,]
euro1988=real_groups[140:141,]
euro1992=real_groups[142:143,]
euro1996=real_groups[144:147,]
euro2000=real_groups[148:151,]
euro2004=real_groups[152:155,]
euro2008=real_groups[156:159,]
c=real_groups[160:161,] # ?????????
asia1976=real_groups[162:163,]
asia1980=real_groups[164:165,]
asia1984=real_groups[166:167,]
asia1988=real_groups[168:169,]
asia1992=real_groups[170:171,]
asia1996=real_groups[172:174,]
asia2000=real_groups[175:177,]
asia2004=real_groups[178:181,]
asia2007=real_groups[182:185,]
africa1968=real_groups[186:187,]
africa1970=real_groups[188:189,]
africa1972=real_groups[190:191,]
africa1974=real_groups[192:193,]
africa1976=real_groups[194:195,]
africa1978=real_groups[196:197,]
africa1980=real_groups[198:199,]
africa1982=real_groups[200:201,]
africa1984=real_groups[202:203,]
africa1986=real_groups[204:205,]
africa1988=real_groups[206:207,]
africa1990=real_groups[208:209,]
africa1992=real_groups[210:213,]
africa1994=real_groups[214:217,]
africa1996=real_groups[218:221,]
africa1998=real_groups[222:225,]
africa2000=real_groups[226:229,]
africa2002=real_groups[230:233,]
africa2004=real_groups[234:237,]
africa2006=real_groups[238:241,]
africa2008=real_groups[242:245,]
africa2010=real_groups[246:249,]
africa2012=real_groups[250:253,]
euro2012=real_groups[254:257,]
africa2013=real_groups[258:261,]
africa2015=real_groups[262:265,]
africa2017=real_groups[266:269,]
africa2019=real_groups[270:275,]
africa2022=real_groups[276:281,]
euro2016=real_groups[282:287,]
euro2021=real_groups[288:293,]

# Next, we can put the groups into a list.
groups<-list(
			 # africa1968,africa1970,africa1972,africa1974,
			 # africa1976,africa1978,africa1980,africa1982,
			 # africa1984,africa1986,africa1988,africa1990,
			 # africa1992,africa1994,africa1996,
			 africa1998,
			 # africa2000,
			 africa2002,africa2004,africa2006,
			 africa2008,africa2010,africa2012,africa2013,
			 africa2015,africa2017,africa2019,africa2022)

# We will also make a vector with corresponding years for the list.
years<-c(seq(1968,2012,by=2),2013,2015,2017,2019,2022) 

years<-c(1998,seq(2002,2012,by=2),2013,2015,2017,2019,2022) 
        
source("GetPairs.R")


get_info=function(v){
  return(all_pairs[all_pairs[,1]%in%c(v[1],v[2])&all_pairs[,2]%in%c(v[1],v[2])&all_pairs[,3]==v[3],])
}

all_pairs<-read.csv("All_Pairs_Regional_9_25.csv", stringsAsFactors=F)
all_pairs<-all_pairs[all_pairs$Event=="Africa"&
					 all_pairs$Year%in%years,]
real_data<-read.csv("RealData_Africa_Sept25.csv",stringsAsFactors=F)
real_data_full<-read.csv("RealDataFull_Africa_Sept25.csv",stringsAsFactors=F)

real_data_full<-real_data_full[real_data_full$Year%in%years,]


real_data<-with(real_data_full,
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

real_data<-c(real_data,
             with(real_data_full[real_data_full$Both_Soccer==1,],
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
                    mean(ym1,na.rm=TRUE))))

real_data<-matrix(real_data,nrow=1)

real_data<-data.frame(real_data)

all_pairs$Index<-NA

for(i in 1:nrow(all_pairs)){
	all_pairs$Index[i]<-with(all_pairs, paste(sort(c(Country1[i],Country2[i], Year[i])),collapse="--"))
}



colnames(real_data)<-c("ln_Trade_Change_Adjusted","Percent_Change_Adjusted","y",
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





perm_data=matrix(0,nrow=1000,ncol=21)

all_pairs$Could_Play_GS<-0

set.seed(0)

for(z in 1:nrow(perm_data)){
  
  source("DrawSimulationAfrica.R")
  source("MakeFakePairsAfrica.R")
  
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
  
  if(z%%100==0){
  
  print(z)
  
  print("Change in ln(Trade)")  
  print(mean(perm_data[1:z,1]<=as.numeric(real_data[1])))
    
  print("Percent Change") 
  print(mean(perm_data[1:z,2]<=as.numeric(real_data[2])))
  
  }
  
for(d in 1:nrow(perm_data_full_all)){
	index<-with(perm_data_full_all, paste(sort(c(Country1[d],Country2[d], Year[d])),collapse="--"))
	row_number<-which(all_pairs$Index==index)
	all_pairs$Could_Play_GS[row_number]<-1
}

  
  
}


perm_data<-as.data.frame(perm_data)

colnames(perm_data)<-c("ln_Trade_Change_Adjusted","Percent_Change_Adjusted","y",
                       "Total_Disputes_Before","Any_Disputes_Before","Both_Dem","Both_NonDem",
                       "Diff_Regime","Both_GATT","One_GATT","Both_EU","One_EU","Colony",
                       "Sibling","Dist","ln_Dist","Contiguous","Alliance_Year_Before",
                       "ln_Trade_Change_Adjusted_Prev","Percent_Change_Adjusted_Prev","ym1")







write.csv(perm_data,"PermDataAfrica_Dec25.csv",row.names=F)

write.csv(all_pairs,"All_Pairs_Africa_Dec25.csv",row.names=F)




