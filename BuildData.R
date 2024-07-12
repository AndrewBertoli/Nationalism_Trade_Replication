require("readxl")

setwd("~/Dropbox/WCNaturalExperiment/Data_and_Code")
real_groups<-read.csv("WC_Groups.csv",stringsAsFactors=FALSE,sep=";")

# To make the data merging easier, we will convert the Soviet Union
# to Russia and both Serbia and Serbia-Montenegro to Yugoslavia (and 
# change them back later)

real_groups$Country1[real_groups$Country1=="Soviet Union"]<-"Russia"
real_groups$Country2[real_groups$Country2=="Soviet Union"]<-"Russia"

real_groups$Country4[real_groups$Country4=="Serbia"]<-"Yugoslavia"
real_groups$Country2[real_groups$Country2=="Serbia-Montenegro"]<-"Yugoslavia"
real_groups$Country4[real_groups$Country4=="Serbia-Montenegro"]<-"Yugoslavia"



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


covariates<-read.csv("Covariates.csv",stringsAsFactors=FALSE,sep=";")

games<-read.csv("WorldCupGames.csv",stringsAsFactors=F,sep=";")

covariates$Country[covariates$Country=="Soviet Union"]<-"Russia"
covariates$Country[covariates$Country=="Serbia"]<-"Yugoslavia"
covariates$Country[covariates$Country=="Serbia-Montenegro"]<-"Yugoslavia"

games$Country1[games$Country1=="Soviet Union"]<-"Russia"
games$Country1[games$Country1=="Serbia"]<-"Yugoslavia"
games$Country1[games$Country1=="Serbia-Montenegro"]<-"Yugoslavia"

games$Country2[games$Country2=="Soviet Union"]<-"Russia"
games$Country2[games$Country2=="Serbia"]<-"Yugoslavia"
games$Country2[games$Country2=="Serbia-Montenegro"]<-"Yugoslavia"


source("GetPairs.R")

source("Adapt_Names.R")




# You can download the National Material Capabilities (v6.0)
# data from this website:
# https://correlatesofwar.org/data-sets/national-material-capabilities/

setwd("~/Downloads/NMC_Documentation-6/NMC-60-abridged")
nmc<-read.csv("NMC-60-abridged.csv")

nmc$stateabb<-adapt_names_cow(nmc$stateabb)



for(i in 1:nrow(covariates)){
index<-which(nmc$stateabb==covariates$Country[i]&
               nmc$year==min(2012,covariates$Year[i]))
if(length(index)==1) covariates[i,3:7]<-nmc[index,c(6,4,5,8,9)]
}

# You can download the Polity V data from this website:
# https://www.systemicpeace.org/inscrdata.html

setwd("~/Downloads")
polity<-read_excel("p5v2018.xls")
polity<-data.frame(polity)

polity$country<-adapt_names_pol(polity$country)

covariates$regime<-NA

for(i in 1:nrow(covariates)){
  index<-which(polity$country==covariates$Country[i]&
               polity$year==covariates$Year[i])
  if(length(index)==1){
  covariates$regime[i]<-as.numeric(polity$polity[index]>5)}
}

rm(polity)

covariates$regime[covariates$Country=="Iceland"]<-1
covariates$regime[covariates$Country=="Yugoslavia"&covariates$Year==2006]<-1
ind<-read.csv("https://correlatesofwar.org/wp-content/uploads/states2016.csv")

ind$stateabb<-adapt_names_cow(ind$stateabb)

covariates$IndYear<-NA

for(i in 1:nrow(covariates)){
  covariates$IndYear[i]<-
  ind$styear[ind$stateabb==covariates$Country[i]][1]
}

countries<-c("United States", "Australia", 
             "New Zealand", "Canada", 
             "Jamaica","Japan", "Cuba", 
             "Trinidad-Tobago")

covariates$SoccerMostPopular<-1
covariates$SoccerMostPopular[covariates$Country%in%countries]<-0

# You can download the data on contiguity from this website:
# https://correlatesofwar.org/data-sets/direct-contiguity/

setwd("~/Downloads/DirectContiguity320")
contiguous<-read.csv("contdird.csv")

contiguous$state1ab<-adapt_names_cow(contiguous$state1ab)

contiguous$state2ab<-adapt_names_cow(contiguous$state2ab)


# You can download the data on Militarized Interstate
# Disputes from this website:
# https://correlatesofwar.org/data-sets/mids/

setwd("~/Downloads/MID-5-Data-and-Supporting-Materials")
data<-read.csv("MIDB 5.0.csv", stringsAsFactors=F)

data$stabb<-adapt_names_cow(data$stabb)

# You can download the COW trade data from this website:
# https://correlatesofwar.org/data-sets/mids/

setwd("~/Downloads/COW_Trade_4.0")
trade<-read.csv("Dyadic_Cow_4.0.csv",stringsAsFactors=FALSE)

trade<-trade[-which(trade$year<1913),]

trade$flow1[trade$flow1==-9]<-NA
trade$flow2[trade$flow2==-9]<-NA

setwd("~/Dropbox/WCNaturalExperiment/Data_and_Code")

trade$importer1<-adapt_names_trade(trade$importer1)
trade$importer2<-adapt_names_trade(trade$importer2)






month<-rep(6,27)

all_pairs<-matrix(0,nrow=1,ncol=4)
for(k in 1:length(groups)){
for(j in 1:length(as.matrix(groups[[k]]))){
new_pairs<-cbind(rep(as.matrix(groups[[k]])[j],length(as.matrix(groups[[k]]))-1),c(as.matrix(groups[[k]])[-j]),years[k],month[k])
all_pairs<-rbind(all_pairs,new_pairs)	
}}



all_pairs<-all_pairs[-unique(c(which(all_pairs[,1]=="NONE"),
                               which(all_pairs[,2]=="NONE"))),]

for(i in 1:length(all_pairs[,1])){all_pairs[i,1:2]<-
  sort(all_pairs[i,1:2])}

all_pairs<-unique(all_pairs,rows=TRUE)

all_pairs<-data.frame(all_pairs,stringsAsFactors=FALSE)

colnames(all_pairs)<-c("Country1","Country2","Year","Month")

all_pairs$Year<-as.numeric(all_pairs$Year)
all_pairs$Month<-as.numeric(all_pairs$Month)



games$GroupStage[games$Year%in%c(1934,1938)&games$Round1==1]<-1

all_pairs$GS_Score1<-NA
all_pairs$GS_Score2<-NA

all_pairs$KS_Score1<-NA
all_pairs$KS_Score2<-NA

for(i in 1:nrow(all_pairs)){
	index<-which(games$Country1==all_pairs$Country1[i]&
	               games$Country2==all_pairs$Country2[i]&
	               games$Year==all_pairs$Year[i]&
	               games$GroupStage==1)
	if(length(index)==1) {all_pairs$GS_Score1[i]<-games$Score1[index] 
	all_pairs$GS_Score2[i]<-games$Score2[index]}
	index<-which(games$Country1==all_pairs$Country2[i]&
	              games$Country2==all_pairs$Country1[i]&
	              games$Year==all_pairs$Year[i]&
	              games$GroupStage==1)
	if(length(index)==1) {all_pairs$GS_Score1[i]<-games$Score2[index] 
	all_pairs$GS_Score2[i]<-games$Score1[index]}}
	
	
	
	
for(i in 1:nrow(all_pairs)){
	index<-which(games$Country1==all_pairs$Country1[i]&
	               games$Country2==all_pairs$Country2[i]&
	               games$Year==all_pairs$Year[i]&
	               games$GroupStage==0)
	if(length(index)==1) {all_pairs$KS_Score1[i]<-games$Score1[index] 
	all_pairs$KS_Score2[i]<-games$Score2[index]}
	index<-which(games$Country1==all_pairs$Country2[i]&
	              games$Country2==all_pairs$Country1[i]&
	              games$Year==all_pairs$Year[i]&
	              games$GroupStage==0)
	if(length(index)==1) {all_pairs$KS_Score1[i]<-games$Score2[index] 
	all_pairs$KS_Score2[i]<-games$Score1[index]}}
	
	
	


for(i in 1:nrow(all_pairs)){
	if(is.na(all_pairs$GS_Score1[i])==F){
		if(all_pairs$GS_Score1[i]<all_pairs$GS_Score2[i]){
		all_pairs[i,c("Country1","Country2")]<-all_pairs[i,c("Country2","Country1")]
		all_pairs[i,c("GS_Score1","GS_Score2")]<-all_pairs[i,c("GS_Score2","GS_Score1")]}}
	if(is.na(all_pairs$KS_Score1[i])==F){
		if(all_pairs$KS_Score1[i]<all_pairs$KS_Score2[i]){
		all_pairs[i,c("Country1","Country2")]<-all_pairs[i,c("Country2","Country1")]
		all_pairs[i,c("KS_Score1","KS_Score2")]<-all_pairs[i,c("KS_Score2","KS_Score1")]}}}	
		

all_pairs[all_pairs$Country1=="Costa Rica"&
            all_pairs$Country2=="Netherlands"&
            all_pairs$Year==2014,c("Country1","Country2")]<-
  c("Netherlands","Costa Rica")

all_pairs[all_pairs$Country1=="United Kingdom"&
            all_pairs$Country2=="West Germany"&
            all_pairs$Year==1982,c("Country1","Country2")]<-
  c("West Germany","United Kingdom")

all_pairs[all_pairs$Country1=="Spain"&
            all_pairs$Country2=="United Kingdom"&
            all_pairs$Year==1982,c("Country1","Country2")]<-
  c("United Kingdom","Spain")

all_pairs[all_pairs$Country1=="Argentina"&
            all_pairs$Country2=="East Germany"&
            all_pairs$Year==1974,c("Country1","Country2")]<-
  c("East Germany","Argentina")

all_pairs[all_pairs$Country1=="Spain"&
            all_pairs$Country2=="Uruguay"&
            all_pairs$Year==1950,c("Country1","Country2")]<-
  c("Uruguay","Spain")




trade_flow<-function(v){
a<-trade[c(which(trade$importer1==as.character(v[1])),
          which(trade$importer2==as.character(v[1]))),]
a<-a[c(which(a$importer1==as.character(v[2])),
       which(a$importer2==as.character(v[2]))),]
t<-c()
for(i in -15:10){
a1<-a[a$year==(as.numeric(v[3])+i),]
if(dim(a1)[1]!=1){tadd<-NA}
if(dim(a1)[1]==1){tadd<-a1$flow1+a1$flow2}
t<-c(t,tadd)}
return(as.numeric(t))}

t<-apply(as.matrix(all_pairs),1,trade_flow)
trade1<-rep(0, length(t))
all_pairs<-cbind(all_pairs,t(t))


gest_covs1<-function(v){
covs<-covariates[covariates$Country==v[1]&
                   covariates$Year==v[3],]	
return(covs[1,3:10])	
}

gest_covs2<-function(v){
covs<-covariates[covariates$Country==v[2]&
                covariates$Year==v[3],]	
return(covs[1,3:10])	
}

covs1<-do.call(rbind,apply(all_pairs,1,gest_covs1))
covs2<-do.call(rbind,apply(all_pairs,1,gest_covs2))

all_pairs<-data.frame(all_pairs,covs1,covs2,stringsAsFactors=F)

rm(covariates)



all_pairs$Both_Dem<-as.numeric(all_pairs$regime==1&all_pairs$regime.1==1)

all_pairs$Both_NonDem<-as.numeric(all_pairs$regime==0&all_pairs$regime.1==0)

all_pairs$Diff_Regime<-as.numeric(all_pairs$regime!=all_pairs$regime.1)

all_pairs$Both_Soccer<-as.numeric(all_pairs$SoccerMostPopular==1&all_pairs$SoccerMostPopular.1==1)

all_pairs$Both_NonSoccer<-as.numeric(all_pairs$SoccerMostPopular==0&all_pairs$SoccerMostPopular.1==0)

all_pairs$One_Soccer<-as.numeric(all_pairs$SoccerMostPopular!=all_pairs$SoccerMostPopular.1)

all_pairs$Contiguous<-NA

for(i in 1:nrow(all_pairs)){
a<-contiguous[c(which(contiguous$state1ab==as.character(all_pairs[i,1])),
                which(contiguous$state2ab==as.character(all_pairs[i,1]))),]
a<-a[c(which(a$state1ab==as.character(all_pairs[i,2])),
       which(a$state2ab==as.character(all_pairs[i,2]))),]
a<-a[which(a$year==min(2016,as.numeric(all_pairs[i,3]))),]
if(dim(a)[1]==0){all_pairs$Contiguous[i]<-0}
if(dim(a)[1]==2){all_pairs$Contiguous[i]<-1}
}


rm(contiguous)



all_pairs$Total_Disputes_Before<-NA


 for(i in 1:nrow(all_pairs)){
 v<-all_pairs[i,]
 a<-which(data$stabb%in%c(v[1],v[2])&data$sidea==1)
 dispute.numbers<-data[a,]$dispnum
 a1<-data[which(data$styear==(as.numeric(v[3])-1)&data$dispnum%in%dispute.numbers&data$sidea==0&data$stabb%in%c(v[1])),]$dispnum
 a2<-data[which(data$styear==(as.numeric(v[3])-1)&data$dispnum%in%dispute.numbers&data$sidea==1&data$stabb%in%c(v[2])),]$dispnum 
 disputes1<-intersect(a1,a2)
 a1<-data[which(data$styear==(as.numeric(v[3])-1)&data$dispnum%in%dispute.numbers&data$sidea==0&data$stabb%in%c(v[2])),]$dispnum
 a2<-data[which(data$styear==(as.numeric(v[3])-1)&data$dispnum%in%dispute.numbers&data$sidea==1&data$stabb%in%c(v[1])),]$dispnum
 disputes2<-intersect(a1,a2)

 all_pairs$Total_Disputes_Before[i]<-length(c(disputes1,disputes2))}


rm(data)










all_pairs$Any_Disputes_Before<-as.numeric(all_pairs$Total_Disputes_Before>0)


# You can download the alliance data from this website:
# https://correlatesofwar.org/data-sets/formal-alliances/

setwd("~/Downloads/version4-2.1_csv")
alliances<-read.csv("alliance_v4.1_by_dyad_yearly.csv",stringsAsFactors = F)

alliances<-alliances[alliances$year>1925,]

setwd("~/Dropbox/WCNaturalExperiment/Data_and_Code")

alliances$state_name1<-adapt_names_trade(alliances$state_name1)
alliances$state_name2<-adapt_names_trade(alliances$state_name2)


getalliances<-function(v){
alliances_year<-alliances[alliances$year==(as.numeric(v[3])-1),]
a<-which(alliances_year$state_name1==as.character(v[1])&alliances_year$state_name2==as.character(v[2]))
b<-which(alliances_year$state_name1==as.character(v[2])&alliances_year$state_name2==as.character(v[1]))
d<-c(a,b)
return(as.numeric(length(d)>0))}

all_pairs<-cbind(all_pairs,apply(all_pairs,1,getalliances))



rm(alliances)



# The data on distances between countries is available at this website:
# http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=6

setwd("~/Downloads")
distances<-read_xls("dist_cepii.xls")
distances<-data.frame(distances)

head(distances)

distances$iso_o<-as.character(adapt_names_iso(distances$iso_o))
distances$iso_d<-as.character(adapt_names_iso(distances$iso_d))

dist1<-function(v){
  if(v[1]=="Czechoslovakia") v[1]<-"Czech Republic"  
  if(v[2]=="Czechoslovakia") v[2]<-"Czech Republic"  
  if(v[1]=="West Germany") v[1]<-"Germany"  
  if(v[2]=="West Germany") v[2]<-"Germany"  
  if(v[1]=="East Germany") v[1]<-"Germany"  
  if(v[2]=="East Germany") v[2]<-"Germany" 
a<-which(distances$iso_o==as.character(v[1]))
b<-which(distances$iso_d==as.character(v[2]))
a<-intersect(a,b)
if(length(a)==0){t<-NA}
if(length(a)==1){t<-distances[a,]$dist}
return(t)}

all_pairs$dist1=apply(all_pairs,1,dist1)

cap_dist<-function(v){
  if(v[1]=="Czechoslovakia") v[1]<-"Czech Republic"  
  if(v[2]=="Czechoslovakia") v[2]<-"Czech Republic"  
  if(v[1]=="West Germany") v[1]<-"Germany"  
  if(v[2]=="West Germany") v[2]<-"Germany"  
  if(v[1]=="East Germany") v[1]<-"Germany"  
  if(v[2]=="East Germany") v[2]<-"Germany"
  a<-which(distances$iso_o==as.character(v[1]))
  b<-which(distances$iso_d==as.character(v[2]))
  a<-intersect(a,b)
  if(length(a)==0){t<-NA}
  if(length(a)==1){t<-distances[a,]$distcap}
  return(t)}

all_pairs$capdist=apply(all_pairs,1,cap_dist)


rm(distances)




all_pairs$Group_Stage<-as.numeric(is.na(all_pairs$GS_Score1)==F)

all_pairs$Knockout_Stage<-as.numeric(is.na(all_pairs$KS_Score1)==F)



colnames(all_pairs)<-c("Country1","Country2","Year","Month",
                       "GS_Score1","GS_Score2","KS_Score1",
                       "KS_Score2","Bi_Trade_M15","Bi_Trade_M14",
                       "Bi_Trade_M13","Bi_Trade_M12","Bi_Trade_M11",
                       "Bi_Trade_M10","Bi_Trade_M9","Bi_Trade_M8",
                       "Bi_Trade_M7","Bi_Trade_M6","Bi_Trade_M5",
                       "Bi_Trade_M4","Bi_Trade_M3","Bi_Trade_M2",
                       "Bi_Trade_Pre","Bi_Trade_Post",
                       "Bi_Trade_P1","Bi_Trade_P2",
                       "Bi_Trade_P3","Bi_Trade_P4",
                       "Bi_Trade_P5","Bi_Trade_P6",
                       "Bi_Trade_P7","Bi_Trade_P8",
                       "Bi_Trade_P9","Bi_Trade_P10",
                       "Irst1","Milex1","Milper1",
                       "Tpop1","Upop1","Democracy1",
                       "IndependenceYear1","SoccerMostPopular1",
                       "Irst2","Milex2","Milper2","Tpop2","Upop2",
                       "Democracy2","IndependenceYear2",
                       "SoccerMostPopular2","Both_Dem", 
                       "Both_NonDem", "Diff_Regime", 
                       "Both_Soccer","Both_NonSoccer",
                       "One_Soccer","Contiguous",
                       "Total_Disputes_Before",
                       "Any_Disputes_Before","Alliance_Year_Before",
                       "Dist1","Cap_Dist","Group_Stage",
                       "Knockout_Stage")



all_pairs<-all_pairs[,c("Country1","Country2","Year","Month",
                        "Bi_Trade_M15","Bi_Trade_M14","Bi_Trade_M13",
                        "Bi_Trade_M12","Bi_Trade_M11","Bi_Trade_M10",
                        "Bi_Trade_M9","Bi_Trade_M8","Bi_Trade_M7",
                        "Bi_Trade_M6","Bi_Trade_M5","Bi_Trade_M4",
                        "Bi_Trade_M3","Bi_Trade_M2","Bi_Trade_Pre",
                        "Bi_Trade_Post","Bi_Trade_P1","Bi_Trade_P2",
                        "Bi_Trade_P3","Bi_Trade_P4","Bi_Trade_P5",
                        "Bi_Trade_P6","Bi_Trade_P7","Bi_Trade_P8",
                        "Bi_Trade_P9","Bi_Trade_P10","Irst1","Milex1",
                        "Milper1","Tpop1","Upop1","Democracy1",
                        "IndependenceYear1","SoccerMostPopular1",
                        "Irst2","Milex2","Milper2","Tpop2","Upop2",
                        "Democracy2","IndependenceYear2",
                        "SoccerMostPopular2","Both_Dem", "Both_NonDem", 
                        "Diff_Regime", "Both_Soccer","Both_NonSoccer",
                        "One_Soccer","Contiguous","Total_Disputes_Before",
                        "Any_Disputes_Before","Alliance_Year_Before",
                        "Dist1","Cap_Dist","Group_Stage", 
                        "Knockout_Stage","GS_Score1","GS_Score2",
                        "KS_Score1","KS_Score2")]







all_pairs$Imports1_Pre15<-NA
all_pairs$Imports2_Pre15<-NA

all_pairs$Imports1_Pre14<-NA
all_pairs$Imports2_Pre14<-NA

all_pairs$Imports1_Pre13<-NA
all_pairs$Imports2_Pre13<-NA

all_pairs$Imports1_Pre12<-NA
all_pairs$Imports2_Pre12<-NA

all_pairs$Imports1_Pre11<-NA
all_pairs$Imports2_Pre11<-NA

all_pairs$Imports1_Pre10<-NA
all_pairs$Imports2_Pre10<-NA

all_pairs$Imports1_Pre9<-NA
all_pairs$Imports2_Pre9<-NA

all_pairs$Imports1_Pre8<-NA
all_pairs$Imports2_Pre8<-NA

all_pairs$Imports1_Pre7<-NA
all_pairs$Imports2_Pre7<-NA

all_pairs$Imports1_Pre6<-NA
all_pairs$Imports2_Pre6<-NA

all_pairs$Imports1_Pre5<-NA
all_pairs$Imports2_Pre5<-NA

all_pairs$Imports1_Pre4<-NA
all_pairs$Imports2_Pre4<-NA

all_pairs$Imports1_Pre3<-NA
all_pairs$Imports2_Pre3<-NA

all_pairs$Imports1_Pre2<-NA
all_pairs$Imports2_Pre2<-NA

all_pairs$Imports1_Pre<-NA
all_pairs$Imports2_Pre<-NA

all_pairs$Imports1<-NA
all_pairs$Imports2<-NA

all_pairs$Imports1_Post1<-NA
all_pairs$Imports2_Post1<-NA

all_pairs$Imports1_Post2<-NA
all_pairs$Imports2_Post2<-NA

all_pairs$Imports1_Post3<-NA
all_pairs$Imports2_Post3<-NA

all_pairs$Imports1_Post4<-NA
all_pairs$Imports2_Post4<-NA

all_pairs$Imports1_Post5<-NA
all_pairs$Imports2_Post5<-NA

all_pairs$Imports1_Post6<-NA
all_pairs$Imports2_Post6<-NA

all_pairs$Imports1_Post7<-NA
all_pairs$Imports2_Post7<-NA

all_pairs$Imports1_Post8<-NA
all_pairs$Imports2_Post8<-NA

all_pairs$Imports1_Post9<-NA
all_pairs$Imports2_Post9<-NA

all_pairs$Imports1_Post10<-NA
all_pairs$Imports2_Post10<-NA



for(i in 1:nrow(all_pairs)){
a<-trade[c(which(trade$importer1==all_pairs[i,1]),which(trade$importer2==all_pairs[i,1])),]
a<-a[c(which(a$importer1==all_pairs[i,2]),which(a$importer2==all_pairs[i,2])),]
a<-a[a$year%in%((as.numeric(all_pairs[i,3])-15):(as.numeric(all_pairs[i,3])+10)),]
if(nrow(a)>1){

for(j in 1:nrow(a)){
thatyear<-a$year[j]	

if(thatyear==(as.numeric(all_pairs[i,3])-15)){
if(a$importer1[j]==all_pairs[i,1]){
  all_pairs$Imports1_Pre15[i]<-a$flow1[j]
  all_pairs$Imports2_Pre15[i]=a$flow2[j]}	
if(a$importer1[j]==all_pairs[i,2]){
  all_pairs$Imports2_Pre15[i]<-a$flow1[j]
  all_pairs$Imports1_Pre15[i]=a$flow2[j]}}	

if(thatyear==(as.numeric(all_pairs[i,3])-14)){
if(a$importer1[j]==all_pairs[i,1]){
  all_pairs$Imports1_Pre14[i]<-a$flow1[j]
  all_pairs$Imports2_Pre14[i]<-a$flow2[j]}	
if(a$importer1[j]==all_pairs[i,2]){
  all_pairs$Imports2_Pre14[i]<-a$flow1[j]
  all_pairs$Imports1_Pre14[i]<-a$flow2[j]}}	

if(thatyear==(as.numeric(all_pairs[i,3])-13)){
if(a$importer1[j]==all_pairs[i,1]){
  all_pairs$Imports1_Pre13[i]<-a$flow1[j]
  all_pairs$Imports2_Pre13[i]<-a$flow2[j]}	
if(a$importer1[j]==all_pairs[i,2]){
  all_pairs$Imports2_Pre13[i]<-a$flow1[j]
  all_pairs$Imports1_Pre13[i]<-a$flow2[j]}}	

if(thatyear==(as.numeric(all_pairs[i,3])-12)){
if(a$importer1[j]==all_pairs[i,1]){
  all_pairs$Imports1_Pre12[i]<-a$flow1[j]
  all_pairs$Imports2_Pre12[i]<-a$flow2[j]}	
if(a$importer1[j]==all_pairs[i,2]){
  all_pairs$Imports2_Pre12[i]<-a$flow1[j]
  all_pairs$Imports1_Pre12[i]<-a$flow2[j]}}	

if(thatyear==(as.numeric(all_pairs[i,3])-11)){
if(a$importer1[j]==all_pairs[i,1]){
  all_pairs$Imports1_Pre11[i]<-a$flow1[j]
  all_pairs$Imports2_Pre11[i]<-a$flow2[j]}	
if(a$importer1[j]==all_pairs[i,2]){
  all_pairs$Imports2_Pre11[i]<-a$flow1[j]
  all_pairs$Imports1_Pre11[i]<-a$flow2[j]}}	

if(thatyear==(as.numeric(all_pairs[i,3])-10)){
if(a$importer1[j]==all_pairs[i,1]){
  all_pairs$Imports1_Pre10[i]<-a$flow1[j]
  all_pairs$Imports2_Pre10[i]<-a$flow2[j]}	
if(a$importer1[j]==all_pairs[i,2]){
  all_pairs$Imports2_Pre10[i]<-a$flow1[j]
  all_pairs$Imports1_Pre10[i]<-a$flow2[j]}}	

if(thatyear==(as.numeric(all_pairs[i,3])-9)){
if(a$importer1[j]==all_pairs[i,1]){
  all_pairs$Imports1_Pre9[i]<-a$flow1[j]
  all_pairs$Imports2_Pre9[i]<-a$flow2[j]}	
if(a$importer1[j]==all_pairs[i,2]){
  all_pairs$Imports2_Pre9[i]<-a$flow1[j]
  all_pairs$Imports1_Pre9[i]<-a$flow2[j]}}	

if(thatyear==(as.numeric(all_pairs[i,3])-8)){
if(a$importer1[j]==all_pairs[i,1]){
  all_pairs$Imports1_Pre8[i]<-a$flow1[j]
  all_pairs$Imports2_Pre8[i]<-a$flow2[j]}	
if(a$importer1[j]==all_pairs[i,2]){
  all_pairs$Imports2_Pre8[i]<-a$flow1[j]
  all_pairs$Imports1_Pre8[i]<-a$flow2[j]}}	

if(thatyear==(as.numeric(all_pairs[i,3])-7)){
if(a$importer1[j]==all_pairs[i,1]){
  all_pairs$Imports1_Pre7[i]<-a$flow1[j]
  all_pairs$Imports2_Pre7[i]<-a$flow2[j]}	
if(a$importer1[j]==all_pairs[i,2]){
  all_pairs$Imports2_Pre7[i]<-a$flow1[j]
  all_pairs$Imports1_Pre7[i]<-a$flow2[j]}}	

if(thatyear==(as.numeric(all_pairs[i,3])-6)){
if(a$importer1[j]==all_pairs[i,1]){
  all_pairs$Imports1_Pre6[i]<-a$flow1[j]
  all_pairs$Imports2_Pre6[i]<-a$flow2[j]}	
if(a$importer1[j]==all_pairs[i,2]){
  all_pairs$Imports2_Pre6[i]<-a$flow1[j]
  all_pairs$Imports1_Pre6[i]<-a$flow2[j]}}	

if(thatyear==(as.numeric(all_pairs[i,3])-5)){
if(a$importer1[j]==all_pairs[i,1]){
  all_pairs$Imports1_Pre5[i]<-a$flow1[j]
  all_pairs$Imports2_Pre5[i]<-a$flow2[j]}	
if(a$importer1[j]==all_pairs[i,2]){
  all_pairs$Imports2_Pre5[i]<-a$flow1[j]
  all_pairs$Imports1_Pre5[i]<-a$flow2[j]}}	

if(thatyear==(as.numeric(all_pairs[i,3])-4)){
if(a$importer1[j]==all_pairs[i,1]){
  all_pairs$Imports1_Pre4[i]<-a$flow1[j]
  all_pairs$Imports2_Pre4[i]<-a$flow2[j]}	
if(a$importer1[j]==all_pairs[i,2]){	
  all_pairs$Imports2_Pre4[i]<-a$flow1[j]
  all_pairs$Imports1_Pre4[i]<-a$flow2[j]}}	

if(thatyear==(as.numeric(all_pairs[i,3])-3)){
if(a$importer1[j]==all_pairs[i,1]){	
  all_pairs$Imports1_Pre3[i]<-a$flow1[j]
  all_pairs$Imports2_Pre3[i]<-a$flow2[j]}	
if(a$importer1[j]==all_pairs[i,2]){
  all_pairs$Imports2_Pre3[i]<-a$flow1[j]
  all_pairs$Imports1_Pre3[i]<-a$flow2[j]}}	

if(thatyear==(as.numeric(all_pairs[i,3])-2)){
if(a$importer1[j]==all_pairs[i,1]){
  all_pairs$Imports1_Pre2[i]<-a$flow1[j]
  all_pairs$Imports2_Pre2[i]<-a$flow2[j]}	
if(a$importer1[j]==all_pairs[i,2]){
  all_pairs$Imports2_Pre2[i]<-a$flow1[j]
  all_pairs$Imports1_Pre2[i]<-a$flow2[j]}}	

if(thatyear==(as.numeric(all_pairs[i,3])-1)){
if(a$importer1[j]==all_pairs[i,1]){
  all_pairs$Imports1_Pre[i]<-a$flow1[j]
  all_pairs$Imports2_Pre[i]<-a$flow2[j]}	
if(a$importer1[j]==all_pairs[i,2]){
  all_pairs$Imports2_Pre[i]<-a$flow1[j]
  all_pairs$Imports1_Pre[i]<-a$flow2[j]}}	

if(thatyear==(as.numeric(all_pairs[i,3]))){
if(a$importer1[j]==all_pairs[i,1]){
  all_pairs$Imports1[i]<-a$flow1[j]
  all_pairs$Imports2[i]<-a$flow2[j]}	
if(a$importer1[j]==all_pairs[i,2]){
  all_pairs$Imports2[i]<-a$flow1[j]
  all_pairs$Imports1[i]<-a$flow2[j]}}	

if(thatyear==(as.numeric(all_pairs[i,3])+1)){
if(a$importer1[j]==all_pairs[i,1]){
  all_pairs$Imports1_Post1[i]<-a$flow1[j]
  all_pairs$Imports2_Post1[i]<-a$flow2[j]}	
if(a$importer1[j]==all_pairs[i,2]){
  all_pairs$Imports2_Post1[i]<-a$flow1[j]
  all_pairs$Imports1_Post1[i]<-a$flow2[j]}}	

if(thatyear==(as.numeric(all_pairs[i,3])+2)){
if(a$importer1[j]==all_pairs[i,1]){
  all_pairs$Imports1_Post2[i]<-a$flow1[j]
  all_pairs$Imports2_Post2[i]<-a$flow2[j]}	
if(a$importer1[j]==all_pairs[i,2]){
  all_pairs$Imports2_Post2[i]<-a$flow1[j]
  all_pairs$Imports1_Post2[i]<-a$flow2[j]}}	

if(thatyear==(as.numeric(all_pairs[i,3])+3)){
if(a$importer1[j]==all_pairs[i,1]){
  all_pairs$Imports1_Post3[i]<-a$flow1[j]
  all_pairs$Imports2_Post3[i]<-a$flow2[j]}	
if(a$importer1[j]==all_pairs[i,2]){
  all_pairs$Imports2_Post3[i]<-a$flow1[j]
  all_pairs$Imports1_Post3[i]<-a$flow2[j]}}	

if(thatyear==(as.numeric(all_pairs[i,3])+4)){
if(a$importer1[j]==all_pairs[i,1]){
  all_pairs$Imports1_Post4[i]<-a$flow1[j]
  all_pairs$Imports2_Post4[i]<-a$flow2[j]}	
if(a$importer1[j]==all_pairs[i,2]){
  all_pairs$Imports2_Post4[i]<-a$flow1[j]
  all_pairs$Imports1_Post4[i]<-a$flow2[j]}}	

if(thatyear==(as.numeric(all_pairs[i,3])+5)){
if(a$importer1[j]==all_pairs[i,1]){
  all_pairs$Imports1_Post5[i]<-a$flow1[j]
  all_pairs$Imports2_Post5[i]<-a$flow2[j]}	
if(a$importer1[j]==all_pairs[i,2]){
  all_pairs$Imports2_Post5[i]<-a$flow1[j]
  all_pairs$Imports1_Post5[i]<-a$flow2[j]}}	

if(thatyear==(as.numeric(all_pairs[i,3])+6)){
if(a$importer1[j]==all_pairs[i,1]){
  all_pairs$Imports1_Post6[i]<-a$flow1[j]
  all_pairs$Imports2_Post6[i]<-a$flow2[j]}	
if(a$importer1[j]==all_pairs[i,2]){
  all_pairs$Imports2_Post6[i]<-a$flow1[j]
  all_pairs$Imports1_Post6[i]<-a$flow2[j]}}	

if(thatyear==(as.numeric(all_pairs[i,3])+7)){
if(a$importer1[j]==all_pairs[i,1]){
  all_pairs$Imports1_Post7[i]<-a$flow1[j]
  all_pairs$Imports2_Post7[i]<-a$flow2[j]}	
if(a$importer1[j]==all_pairs[i,2]){
  all_pairs$Imports2_Post7[i]<-a$flow1[j]
  all_pairs$Imports1_Post7[i]<-a$flow2[j]}}	

if(thatyear==(as.numeric(all_pairs[i,3])+8)){
if(a$importer1[j]==all_pairs[i,1]){
  all_pairs$Imports1_Post8[i]<-a$flow1[j]
  all_pairs$Imports2_Post8[i]<-a$flow2[j]}	
if(a$importer1[j]==all_pairs[i,2]){
  all_pairs$Imports2_Post8[i]<-a$flow1[j]
  all_pairs$Imports1_Post8[i]<-a$flow2[j]}}	

if(thatyear==(as.numeric(all_pairs[i,3])+9)){
if(a$importer1[j]==all_pairs[i,1]){
  all_pairs$Imports1_Post9[i]<-a$flow1[j]
  all_pairs$Imports2_Post9[i]<-a$flow2[j]}	
if(a$importer1[j]==all_pairs[i,2]){
  all_pairs$Imports2_Post9[i]<-a$flow1[j]
  all_pairs$Imports1_Post9[i]<-a$flow2[j]}}

if(thatyear==(as.numeric(all_pairs[i,3])+10)){
if(a$importer1[j]==all_pairs[i,1]){
  all_pairs$Imports1_Post10[i]<-a$flow1[j]
  all_pairs$Imports2_Post10[i]<-a$flow2[j]}	
if(a$importer1[j]==all_pairs[i,2]){
  all_pairs$Imports2_Post10[i]<-a$flow1[j]
  all_pairs$Imports1_Post10[i]<-a$flow2[j]}}

	}}}




rm(trade)



# You can download the trade data for 2018 here:
# http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37
# The file is HS 92

setwd("~/Downloads/BACI_HS92_V202401b")

year<-2018

index<-which(all_pairs$Year%in%(year))


dat<-read.csv("BACI_HS92_Y2003_V202401b.csv",stringsAsFactors=F)

setwd("~/Dropbox/WCNaturalExperiment/Data_and_Code")

source("ChangeNamesUN.R")

all_pairs$Imports1_Pre15[index]<-NA
all_pairs$Imports2_Pre15[index]<-NA
all_pairs$Bi_Trade_M15[index]<-NA


for(i in index){
	p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
	if(nrow(p)>0) all_pairs$Imports2_Pre15[i]<-sum(p$v)/1000
	p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
	if(nrow(p)>0) all_pairs$Imports1_Pre15[i]<-sum(p$v)/1000

}

all_pairs$Bi_Trade_M15[index]<-all_pairs$Imports1_Pre15[index]+all_pairs$Imports2_Pre15[index]

rm(dat)



setwd("~/Downloads/BACI_HS92_V202401b")

dat<-read.csv("BACI_HS92_Y2004_V202401b.csv",stringsAsFactors=F)

setwd("~/Dropbox/WCNaturalExperiment/Data_and_Code")

source("ChangeNamesUN.R")

all_pairs$Imports1_Pre14[index]<-NA
all_pairs$Imports2_Pre14[index]<-NA
all_pairs$Bi_Trade_M14[index]<-NA


for(i in index){
	p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
	if(nrow(p)>0) all_pairs$Imports2_Pre14[i]<-sum(p$v)/1000
	p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
	if(nrow(p)>0) all_pairs$Imports1_Pre14[i]<-sum(p$v)/1000

}

all_pairs$Bi_Trade_M14[index]<-all_pairs$Imports1_Pre14[index]+all_pairs$Imports2_Pre14[index]

rm(dat)




setwd("~/Downloads/BACI_HS92_V202401b")

dat<-read.csv("BACI_HS92_Y2005_V202401b.csv",stringsAsFactors=F)

setwd("~/Dropbox/WCNaturalExperiment/Data_and_Code")

source("ChangeNamesUN.R")

all_pairs$Imports1_Pre13[index]<-NA
all_pairs$Imports2_Pre13[index]<-NA
all_pairs$Bi_Trade_M13[index]<-NA


for(i in index){
	p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
	if(nrow(p)>0) all_pairs$Imports2_Pre13[i]<-sum(p$v)/1000
	p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
	if(nrow(p)>0) all_pairs$Imports1_Pre13[i]<-sum(p$v)/1000

}

all_pairs$Bi_Trade_M13[index]<-all_pairs$Imports1_Pre13[index]+all_pairs$Imports2_Pre13[index]

rm(dat)




setwd("~/Downloads/BACI_HS92_V202401b")

dat<-read.csv("BACI_HS92_Y2006_V202401b.csv",stringsAsFactors=F)

setwd("~/Dropbox/WCNaturalExperiment/Data_and_Code")

source("ChangeNamesUN.R")

all_pairs$Imports1_Pre12[index]<-NA
all_pairs$Imports2_Pre12[index]<-NA
all_pairs$Bi_Trade_M12[index]<-NA


for(i in index){
	p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
	if(nrow(p)>0) all_pairs$Imports2_Pre12[i]<-sum(p$v)/1000
	p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
	if(nrow(p)>0) all_pairs$Imports1_Pre12[i]<-sum(p$v)/1000

}

all_pairs$Bi_Trade_M12[index]<-all_pairs$Imports1_Pre12[index]+all_pairs$Imports2_Pre12[index]

rm(dat)



setwd("~/Downloads/BACI_HS92_V202401b")

dat<-read.csv("BACI_HS92_Y2007_V202401b.csv",stringsAsFactors=F)

setwd("~/Dropbox/WCNaturalExperiment/Data_and_Code")

source("ChangeNamesUN.R")

all_pairs$Imports1_Pre11[index]<-NA
all_pairs$Imports2_Pre11[index]<-NA
all_pairs$Bi_Trade_M11[index]<-NA


for(i in index){
	p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
	if(nrow(p)>0) all_pairs$Imports2_Pre11[i]<-sum(p$v)/1000
	p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
	if(nrow(p)>0) all_pairs$Imports1_Pre11[i]<-sum(p$v)/1000

}

all_pairs$Bi_Trade_M11[index]<-all_pairs$Imports1_Pre11[index]+all_pairs$Imports2_Pre11[index]

rm(dat)



setwd("~/Downloads/BACI_HS92_V202401b")

dat<-read.csv("BACI_HS92_Y2008_V202401b.csv",stringsAsFactors=F)

setwd("~/Dropbox/WCNaturalExperiment/Data_and_Code")

source("ChangeNamesUN.R")

all_pairs$Imports1_Pre10[index]<-NA
all_pairs$Imports2_Pre10[index]<-NA
all_pairs$Bi_Trade_M10[index]<-NA


for(i in index){
	p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
	if(nrow(p)>0) all_pairs$Imports2_Pre10[i]<-sum(p$v)/1000
	p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
	if(nrow(p)>0) all_pairs$Imports1_Pre10[i]<-sum(p$v)/1000

}

all_pairs$Bi_Trade_M10[index]<-all_pairs$Imports1_Pre10[index]+all_pairs$Imports2_Pre10[index]

rm(dat)





setwd("~/Downloads/BACI_HS92_V202401b")

dat<-read.csv("BACI_HS92_Y2009_V202401b.csv",stringsAsFactors=F)

setwd("~/Dropbox/WCNaturalExperiment/Data_and_Code")

source("ChangeNamesUN.R")

all_pairs$Imports1_Pre9[index]<-NA
all_pairs$Imports2_Pre9[index]<-NA
all_pairs$Bi_Trade_M9[index]<-NA


for(i in index){
	p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
	if(nrow(p)>0) all_pairs$Imports2_Pre9[i]<-sum(p$v)/1000
	p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
	if(nrow(p)>0) all_pairs$Imports1_Pre9[i]<-sum(p$v)/1000

}

all_pairs$Bi_Trade_M9[index]<-all_pairs$Imports1_Pre9[index]+all_pairs$Imports2_Pre9[index]

rm(dat)




setwd("~/Downloads/BACI_HS92_V202401b")

dat<-read.csv("BACI_HS92_Y2010_V202401b.csv",stringsAsFactors=F)

setwd("~/Dropbox/WCNaturalExperiment/Data_and_Code")

source("ChangeNamesUN.R")

all_pairs$Imports1_Pre8[index]<-NA
all_pairs$Imports2_Pre8[index]<-NA
all_pairs$Bi_Trade_M8[index]<-NA


for(i in index){
	p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
	if(nrow(p)>0) all_pairs$Imports2_Pre8[i]<-sum(p$v)/1000
	p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
	if(nrow(p)>0) all_pairs$Imports1_Pre8[i]<-sum(p$v)/1000

}

all_pairs$Bi_Trade_M8[index]<-all_pairs$Imports1_Pre8[index]+all_pairs$Imports2_Pre8[index]

rm(dat)





setwd("~/Downloads/BACI_HS92_V202401b")

dat<-read.csv("BACI_HS92_Y2011_V202401b.csv",stringsAsFactors=F)

setwd("~/Dropbox/WCNaturalExperiment/Data_and_Code")

source("ChangeNamesUN.R")

all_pairs$Imports1_Pre7[index]<-NA
all_pairs$Imports2_Pre7[index]<-NA
all_pairs$Bi_Trade_M7[index]<-NA


for(i in index){
	p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
	if(nrow(p)>0) all_pairs$Imports2_Pre7[i]<-sum(p$v)/1000
	p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
	if(nrow(p)>0) all_pairs$Imports1_Pre7[i]<-sum(p$v)/1000

}

all_pairs$Bi_Trade_M7[index]<-all_pairs$Imports1_Pre7[index]+all_pairs$Imports2_Pre7[index]

rm(dat)



setwd("~/Downloads/BACI_HS92_V202401b")

dat<-read.csv("BACI_HS92_Y2012_V202401b.csv",stringsAsFactors=F)

setwd("~/Dropbox/WCNaturalExperiment/Data_and_Code")

source("ChangeNamesUN.R")

all_pairs$Imports1_Pre6[index]<-NA
all_pairs$Imports2_Pre6[index]<-NA
all_pairs$Bi_Trade_M6[index]<-NA


for(i in index){
	p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
	if(nrow(p)>0) all_pairs$Imports2_Pre6[i]<-sum(p$v)/1000
	p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
	if(nrow(p)>0) all_pairs$Imports1_Pre6[i]<-sum(p$v)/1000

}

all_pairs$Bi_Trade_M6[index]<-all_pairs$Imports1_Pre6[index]+all_pairs$Imports2_Pre6[index]

rm(dat)




setwd("~/Downloads/BACI_HS92_V202401b")

dat<-read.csv("BACI_HS92_Y2013_V202401b.csv",stringsAsFactors=F)

setwd("~/Dropbox/WCNaturalExperiment/Data_and_Code")

source("ChangeNamesUN.R")

all_pairs$Imports1_Pre5[index]<-NA
all_pairs$Imports2_Pre5[index]<-NA
all_pairs$Bi_Trade_M5[index]<-NA


for(i in index){
	p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
	if(nrow(p)>0) all_pairs$Imports2_Pre5[i]<-sum(p$v)/1000
	p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
	if(nrow(p)>0) all_pairs$Imports1_Pre5[i]<-sum(p$v)/1000

}

all_pairs$Bi_Trade_M5[index]<-all_pairs$Imports1_Pre5[index]+all_pairs$Imports2_Pre5[index]

rm(dat)







setwd("~/Downloads/BACI_HS92_V202401b")

dat<-read.csv("BACI_HS92_Y2014_V202401b.csv",stringsAsFactors=F)

setwd("~/Dropbox/WCNaturalExperiment/Data_and_Code")

source("ChangeNamesUN.R")

all_pairs$Imports1_Pre4[index]<-NA
all_pairs$Imports2_Pre4[index]<-NA
all_pairs$Bi_Trade_M4[index]<-NA


for(i in index){
	p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
	if(nrow(p)>0) all_pairs$Imports2_Pre4[i]<-sum(p$v)/1000
	p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
	if(nrow(p)>0) all_pairs$Imports1_Pre4[i]<-sum(p$v)/1000

}

all_pairs$Bi_Trade_M4[index]<-all_pairs$Imports1_Pre4[index]+all_pairs$Imports2_Pre4[index]

rm(dat)





setwd("~/Downloads/BACI_HS92_V202401b")

dat<-read.csv("BACI_HS92_Y2015_V202401b.csv",stringsAsFactors=F)

setwd("/Dropbox/WCNaturalExperiment/Data_and_Code")


source("ChangeNamesUN.R")

all_pairs$Imports1_Pre3[index]<-NA
all_pairs$Imports2_Pre3[index]<-NA
all_pairs$Bi_Trade_M3[index]<-NA


for(i in index){
	p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
	if(nrow(p)>0) all_pairs$Imports2_Pre3[i]<-sum(p$v)/1000
	p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
	if(nrow(p)>0) all_pairs$Imports1_Pre3[i]<-sum(p$v)/1000

}

all_pairs$Bi_Trade_M3[index]<-all_pairs$Imports1_Pre3[index]+all_pairs$Imports2_Pre3[index]

rm(dat)



setwd("~/Downloads/BACI_HS92_V202401b")

dat<-read.csv("BACI_HS92_Y2016_V202401b.csv",stringsAsFactors=F)

setwd("~/Dropbox/WCNaturalExperiment/Data_and_Code")

source("ChangeNamesUN.R")

all_pairs$Imports1_Pre2[index]<-NA
all_pairs$Imports2_Pre2[index]<-NA
all_pairs$Bi_Trade_M2[index]<-NA


for(i in index){
	p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
	if(nrow(p)>0) all_pairs$Imports2_Pre2[i]<-sum(p$v)/1000
	p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
	if(nrow(p)>0) all_pairs$Imports1_Pre2[i]<-sum(p$v)/1000

}

all_pairs$Bi_Trade_M2[index]<-all_pairs$Imports1_Pre2[index]+all_pairs$Imports2_Pre2[index]

rm(dat)



setwd("~/Downloads/BACI_HS92_V202401b")

dat<-read.csv("BACI_HS92_Y2017_V202401b.csv",stringsAsFactors=F)

setwd("~/Dropbox/WCNaturalExperiment/Data_and_Code")

source("ChangeNamesUN.R")

all_pairs$Imports1_Pre[index]<-NA
all_pairs$Imports2_Pre[index]<-NA
all_pairs$Bi_Trade_Pre[index]<-NA


for(i in index){
	p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
	if(nrow(p)>0) all_pairs$Imports2_Pre[i]<-sum(p$v)/1000
	p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
	if(nrow(p)>0) all_pairs$Imports1_Pre[i]<-sum(p$v)/1000

}

all_pairs$Bi_Trade_Pre[index]<-all_pairs$Imports1_Pre[index]+all_pairs$Imports2_Pre[index]

rm(dat)



setwd("~/Downloads/BACI_HS92_V202401b")

dat<-read.csv("BACI_HS92_Y2018_V202401b.csv",stringsAsFactors=F)

setwd("~/Dropbox/WCNaturalExperiment/Data_and_Code")

source("ChangeNamesUN.R")

all_pairs$Imports1[index]<-NA
all_pairs$Imports2[index]<-NA
all_pairs$Bi_Trade_Post[index]<-NA


for(i in index){
	p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
	if(nrow(p)>0) all_pairs$Imports2[i]<-sum(p$v)/1000
	p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
	if(nrow(p)>0) all_pairs$Imports1[i]<-sum(p$v)/1000

}

all_pairs$Bi_Trade_Post[index]<-all_pairs$Imports1[index]+all_pairs$Imports2[index]

rm(dat)



setwd("~/Downloads/BACI_HS92_V202401b")

dat<-read.csv("BACI_HS92_Y2019_V202401b.csv",stringsAsFactors=F)

setwd("~/Dropbox/WCNaturalExperiment/Data_and_Code")

source("ChangeNamesUN.R")

all_pairs$Imports1_Post1[index]<-NA
all_pairs$Imports2_Post1[index]<-NA
all_pairs$Bi_Trade_P1[index]<-NA


for(i in index){
	p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
	if(nrow(p)>0) all_pairs$Imports2_Post1[i]<-sum(p$v)/1000
	p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
	if(nrow(p)>0) all_pairs$Imports1_Post1[i]<-sum(p$v)/1000

}

all_pairs$Bi_Trade_P1[index]<-all_pairs$Imports1_Post1[index]+all_pairs$Imports2_Post1[index]

rm(dat)





setwd("~/Downloads/BACI_HS92_V202401b")

dat<-read.csv("BACI_HS92_Y2020_V202401b.csv",stringsAsFactors=F)

setwd("~/Dropbox/WCNaturalExperiment/Data_and_Code")

source("ChangeNamesUN.R")

all_pairs$Imports1_Post2[index]<-NA
all_pairs$Imports2_Post2[index]<-NA
all_pairs$Bi_Trade_P2[index]<-NA


for(i in index){
  p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
  if(nrow(p)>0) all_pairs$Imports2_Post2[i]<-sum(p$v)/1000
  p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
  if(nrow(p)>0) all_pairs$Imports1_Post2[i]<-sum(p$v)/1000
  
}

all_pairs$Bi_Trade_P2[index]<-all_pairs$Imports1_Post2[index]+all_pairs$Imports2_Post2[index]

rm(dat)







setwd("~/Downloads/BACI_HS92_V202401b")

dat<-read.csv("BACI_HS92_Y2021_V202401b.csv",stringsAsFactors=F)

setwd("~/Dropbox/WCNaturalExperiment/Data_and_Code")

source("ChangeNamesUN.R")

all_pairs$Imports1_Post3[index]<-NA
all_pairs$Imports2_Post3[index]<-NA
all_pairs$Bi_Trade_P3[index]<-NA


for(i in index){
  p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
  if(nrow(p)>0) all_pairs$Imports2_Post3[i]<-sum(p$v)/1000
  p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
  if(nrow(p)>0) all_pairs$Imports1_Post3[i]<-sum(p$v)/1000
  
}

all_pairs$Bi_Trade_P3[index]<-all_pairs$Imports1_Post3[index]+all_pairs$Imports2_Post3[index]

rm(dat)






setwd("~/Downloads/BACI_HS92_V202401b")

dat<-read.csv("BACI_HS92_Y2022_V202401b.csv",stringsAsFactors=F)

setwd("~/Dropbox/WCNaturalExperiment/Data_and_Code")

source("ChangeNamesUN.R")

all_pairs$Imports1_Post4[index]<-NA
all_pairs$Imports2_Post4[index]<-NA
all_pairs$Bi_Trade_P4[index]<-NA


for(i in index){
  p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
  if(nrow(p)>0) all_pairs$Imports2_Post4[i]<-sum(p$v)/1000
  p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
  if(nrow(p)>0) all_pairs$Imports1_Post4[i]<-sum(p$v)/1000
  
}

all_pairs$Bi_Trade_P4[index]<-all_pairs$Imports1_Post4[index]+all_pairs$Imports2_Post4[index]

rm(dat)





all_pairs$ImportDrop1_Pre9<-as.numeric(all_pairs$Imports1_Pre10>all_pairs$Imports1_Pre9)
all_pairs$ImportDrop2_Pre9<-as.numeric(all_pairs$Imports2_Pre10>all_pairs$Imports2_Pre9)

all_pairs$ImportDrop1_Pre8<-as.numeric(all_pairs$Imports1_Pre9>all_pairs$Imports1_Pre8)
all_pairs$ImportDrop2_Pre8<-as.numeric(all_pairs$Imports2_Pre9>all_pairs$Imports2_Pre8)

all_pairs$ImportDrop1_Pre7<-as.numeric(all_pairs$Imports1_Pre8>all_pairs$Imports1_Pre7)
all_pairs$ImportDrop2_Pre7<-as.numeric(all_pairs$Imports2_Pre8>all_pairs$Imports2_Pre7)

all_pairs$ImportDrop1_Pre6<-as.numeric(all_pairs$Imports1_Pre7>all_pairs$Imports1_Pre6)
all_pairs$ImportDrop2_Pre6<-as.numeric(all_pairs$Imports2_Pre7>all_pairs$Imports2_Pre6)

all_pairs$ImportDrop1_Pre5<-as.numeric(all_pairs$Imports1_Pre6>all_pairs$Imports1_Pre5)
all_pairs$ImportDrop2_Pre5<-as.numeric(all_pairs$Imports2_Pre6>all_pairs$Imports2_Pre5)

all_pairs$ImportDrop1_Pre4<-as.numeric(all_pairs$Imports1_Pre5>all_pairs$Imports1_Pre4)
all_pairs$ImportDrop2_Pre4<-as.numeric(all_pairs$Imports2_Pre5>all_pairs$Imports2_Pre4)

all_pairs$ImportDrop1_Pre3<-as.numeric(all_pairs$Imports1_Pre4>all_pairs$Imports1_Pre3)
all_pairs$ImportDrop2_Pre3<-as.numeric(all_pairs$Imports2_Pre4>all_pairs$Imports2_Pre3)

all_pairs$ImportDrop1_Pre2<-as.numeric(all_pairs$Imports1_Pre3>all_pairs$Imports1_Pre2)
all_pairs$ImportDrop2_Pre2<-as.numeric(all_pairs$Imports2_Pre3>all_pairs$Imports2_Pre2)

all_pairs$ImportDrop1_Pre<-as.numeric(all_pairs$Imports1_Pre2>all_pairs$Imports1_Pre)
all_pairs$ImportDrop2_Pre<-as.numeric(all_pairs$Imports2_Pre2>all_pairs$Imports2_Pre)

all_pairs$ImportDrop1<-as.numeric(all_pairs$Imports1_Pre>all_pairs$Imports1)
all_pairs$ImportDrop2<-as.numeric(all_pairs$Imports2_Pre>all_pairs$Imports2)

all_pairs$ImportDrop1_Post1<-as.numeric(all_pairs$Imports1>all_pairs$Imports1_Post1)
all_pairs$ImportDrop2_Post1<-as.numeric(all_pairs$Imports2>all_pairs$Imports2_Post1)

all_pairs$ImportDrop1_Post2<-as.numeric(all_pairs$Imports1_Post1>all_pairs$Imports1_Post2)
all_pairs$ImportDrop2_Post2<-as.numeric(all_pairs$Imports2_Post1>all_pairs$Imports2_Post2)

all_pairs$ImportDrop1_Post3<-as.numeric(all_pairs$Imports1_Post2>all_pairs$Imports1_Post3)
all_pairs$ImportDrop2_Post3<-as.numeric(all_pairs$Imports2_Post2>all_pairs$Imports2_Post3)

all_pairs$ImportDrop1_Post4<-as.numeric(all_pairs$Imports1_Post3>all_pairs$Imports1_Post4)
all_pairs$ImportDrop2_Post4<-as.numeric(all_pairs$Imports2_Post3>all_pairs$Imports2_Post4)

all_pairs$ImportDrop1_Post5<-as.numeric(all_pairs$Imports1_Post4>all_pairs$Imports1_Post5)
all_pairs$ImportDrop2_Post5<-as.numeric(all_pairs$Imports2_Post4>all_pairs$Imports2_Post5)

all_pairs$ImportDrop1_Post6<-as.numeric(all_pairs$Imports1_Post5>all_pairs$Imports1_Post6)
all_pairs$ImportDrop2_Post6<-as.numeric(all_pairs$Imports2_Post5>all_pairs$Imports2_Post6)

all_pairs$ImportDrop1_Post7<-as.numeric(all_pairs$Imports1_Post6>all_pairs$Imports1_Post7)
all_pairs$ImportDrop2_Post7<-as.numeric(all_pairs$Imports2_Post6>all_pairs$Imports2_Post7)

all_pairs$ImportDrop1_Post8<-as.numeric(all_pairs$Imports1_Post7>all_pairs$Imports1_Post8)
all_pairs$ImportDrop2_Post8<-as.numeric(all_pairs$Imports2_Post7>all_pairs$Imports2_Post8)

all_pairs$ImportDrop1_Post9<-as.numeric(all_pairs$Imports1_Post8>all_pairs$Imports1_Post9)
all_pairs$ImportDrop2_Post9<-as.numeric(all_pairs$Imports2_Post8>all_pairs$Imports2_Post9)



all_pairs$Dist<-NA
all_pairs$Country1_GDP<-NA
all_pairs$Country2_GDP<-NA
all_pairs$Country1_GDP_Cap<-NA
all_pairs$Country2_GDP_Cap<-NA
all_pairs$Country1_GATT<-NA
all_pairs$Country2_GATT<-NA
all_pairs$Country1_EU<-NA
all_pairs$Country2_EU<-NA
all_pairs$Common_Religion<-NA
all_pairs$Colony<-NA
# all_pairs$Country1_Colony<-NA
# all_pairs$Country2_Colony<-NA
all_pairs$Sibling<-NA


# The data for the gravity model can be downloaded from this website;
# http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=8

setwd("~/Downloads/Gravity_csv_V202211")

dat<-read.csv("Gravity_V202211.csv",stringsAsFactors = F)

dat<-dat[,c("year","country_id_o","country_id_d", "iso3_o", 
            "iso3_d","dist", "gdp_o", "gdp_d", "gdpcap_o", "gdpcap_d", 
            "gatt_o", "gatt_d", "eu_o", "eu_d", "comrelig", 
            "col_dep_ever", "sibling")]

dat$iso3_o[which(dat$country_id_o=="DEU.1"&dat$year<1991)]<-"West Germany"
dat$iso3_o[which(dat$country_id_o=="DDR")]<-"East Germany"
dat$iso3_o[which(dat$country_id_o=="CZE")]<-"Czech Republic"
dat$iso3_o[which(dat$country_id_o=="CSK")]<-"Czechoslovakia"
dat$iso3_o[which(dat$country_id_o=="SUN")]<-"Soviet Union"
dat$iso3_o[which(dat$country_id_o=="PRK")]<-"North Korea"
dat$iso3_o[which(dat$country_id_o=="COD")]<-"Zaire"
dat$iso3_o[which(dat$country_id_o=="YUG")]<-"Yugoslavia"
dat$iso3_o[which(dat$country_id_o=="SRB"&dat$year==2018)]<-"Yugoslavia"


dat$iso3_d[which(dat$country_id_d=="DEU.1"&dat$year<1991)]<-"West Germany"
dat$iso3_d[which(dat$country_id_d=="DDR")]<-"East Germany"
dat$iso3_d[which(dat$country_id_d=="CZE")]<-"Czech Republic"
dat$iso3_d[which(dat$country_id_d=="CSK")]<-"Czechoslovakia"
dat$iso3_d[which(dat$country_id_d=="SUN")]<-"Soviet Union"
dat$iso3_d[which(dat$country_id_d=="PRK")]<-"North Korea"
dat$iso3_d[which(dat$country_id_d=="COD")]<-"Zaire"
dat$iso3_d[which(dat$country_id_d=="YUG")]<-"Yugoslavia"
dat$iso3_d[which(dat$country_id_d=="SRB"&dat$year==2018)]<-"Yugoslavia"

dat<-dat[-which(dat$year==2018&dat$country_id_o=="YUG"),]
dat<-dat[-which(dat$year==2018&dat$country_id_d=="YUG"),]

dat$iso3_o<-adapt_names_iso(dat$iso3_o)
dat<-dat[-which(nchar(dat$iso3_o)==3&dat$iso3_o!="UAE"),]

dat$iso3_d<-adapt_names_iso(dat$iso3_d)
dat<-dat[-which(nchar(dat$iso3_d)==3&dat$iso3_d!="UAE"),]

for(i in 1:nrow(all_pairs)){
v<-all_pairs[i,]
v1<-v[1]
v2<-v[2]

if(v1=="Russia"&all_pairs$Year[i]<1991) v1<-"Soviet Union"
if(v2=="Russia"&all_pairs$Year[i]<1991) v2<-"Soviet Union"	

a<-which(dat$iso3_o==as.character(v1)&dat$year==all_pairs$Year[i])
b<-which(dat$iso3_d==as.character(v2)&dat$year==all_pairs$Year[i])
a<-intersect(a,b)
if(length(a)==2) a<-a[2]
if(length(a)==0){t<-NA}
if(length(a)==1){ 
all_pairs[i,c("Dist","Country1_GDP","Country2_GDP",
              "Country1_GDP_Cap","Country2_GDP_Cap",
              "Country1_GATT","Country2_GATT","Country1_EU",
              "Country2_EU","Common_Religion",
              "Colony","Sibling")]<-
  dat[a,c("dist", "gdp_o", "gdp_d", "gdpcap_o", "gdpcap_d", 
          "gatt_o", "gatt_d", "eu_o", "eu_d", "comrelig", 
          "col_dep_ever", "sibling")]}
a<-which(dat$iso3_o==as.character(v2)&dat$year==all_pairs$Year[i])
b<-which(dat$iso3_d==as.character(v1)&dat$year==all_pairs$Year[i])
a<-intersect(a,b)
if(length(a)==1){ 
all_pairs[i,c("Dist","Country1_GDP","Country2_GDP",
              "Country1_GDP_Cap","Country2_GDP_Cap",
              "Country1_GATT","Country2_GATT","Country1_EU",
              "Country2_EU","Common_Religion",
              "Colony","Sibling")]<-
  dat[a,c("dist", "gdp_d", "gdp_o", "gdpcap_d", "gdpcap_o", 
          "gatt_d", "gatt_o", "eu_d", "eu_o", "comrelig", 
          "col_dep_ever", "sibling")]}
}





missing_dist<-which(is.na(all_pairs$Dist)==T&all_pairs$Year<1940)

for(i in missing_dist){
v<-all_pairs[i,]
v1<-v[1]
v2<-v[2]

if(v1=="Russia"&all_pairs$Year[i]<1991) v1<-"Soviet Union"
if(v2=="Russia"&all_pairs$Year[i]<1991) v2<-"Soviet Union"

a<-which(dat$iso3_o==as.character(v1))
b<-which(dat$iso3_d==as.character(v2))
a<-intersect(a,b)
if(length(a)==0){t<-NA}
if(length(a)>=1){ 
a<-a[1]	
all_pairs[i,c("Dist")]<-dat[a,c("dist")]}
a<-which(dat$iso3_o==as.character(v2))
b<-which(dat$iso3_d==as.character(v1))
a<-intersect(a,b)
if(length(a)>=1){ 
a<-a[1]	
all_pairs[i,c("Dist")]<-dat[a,c("dist")]}
}




missing_religion<-which(is.na(all_pairs$Common_Religion)==T)

for(i in missing_religion){
v<-all_pairs[i,]
v1<-v[1]
v2<-v[2]

if(v1=="Russia"&all_pairs$Year[i]<1991) v1<-"Soviet Union"
if(v2=="Russia"&all_pairs$Year[i]<1991) v2<-"Soviet Union"

a<-which(dat$iso3_o==as.character(v1))
b<-which(dat$iso3_d==as.character(v2))
a<-intersect(a,b)
if(length(a)==0){t<-NA}
if(length(a)>=1){ 
a<-a[1]	
all_pairs[i,c("Common_Religion")]<-dat[a,c("comrelig")]}
a<-which(dat$iso3_o==as.character(v2))
b<-which(dat$iso3_d==as.character(v1))
a<-intersect(a,b)
if(length(a)>=1){ 
a<-a[1]	
all_pairs[i,c("Common_Religion")]<-dat[a,c("comrelig")]}
}




missing_colony<-which(is.na(all_pairs$Colony)==T)

for(i in missing_colony){
v<-all_pairs[i,]
v1<-v[1]
v2<-v[2]

if(v1=="Russia"&all_pairs$Year[i]<1991) v1<-"Soviet Union"
if(v2=="Russia"&all_pairs$Year[i]<1991) v2<-"Soviet Union"

a<-which(dat$iso3_o==as.character(v1))
b<-which(dat$iso3_d==as.character(v2))
a<-intersect(a,b)
if(length(a)==0){t=NA}
if(length(a)>=1){ 
a<-a[1]	
all_pairs[i,c("Colony")]<-dat[a,c("col_dep_ever")]}
a<-which(dat$iso3_o==as.character(v2))
b<-which(dat$iso3_d==as.character(v1))
a<-intersect(a,b)
if(length(a)>=1){ 
a<-a[1]	
all_pairs[i,c("Colony")]<-dat[a,c("col_dep_ever")]}
}

all_pairs$Colony[all_pairs$Country1=="Germany"&all_pairs$Year<1940]<-0
all_pairs$Colony[all_pairs$Country2=="Germany"&all_pairs$Year<1940]<-0




missing_sibling<-which(is.na(all_pairs$Sibling)==T)

for(i in missing_sibling){
v<-all_pairs[i,]
v1<-v[1]
v2<-v[2]

if(v1=="Russia"&all_pairs$Year[i]<1991) v1<-"Soviet Union"
if(v2=="Russia"&all_pairs$Year[i]<1991) v2<-"Soviet Union"

a<-which(dat$iso3_o==as.character(v1))
b<-which(dat$iso3_d==as.character(v2))
a<-intersect(a,b)
if(length(a)==0){t=NA}
if(length(a)>=1){ 
a<-a[1]	
all_pairs[i,c("Sibling")]<-dat[a,c("sibling")]}
a<-which(dat$iso3_o==as.character(v2))
b<-which(dat$iso3_d==as.character(v1))
a<-intersect(a,b)
if(length(a)>=1){ 
a<-a[1]	
all_pairs[i,c("Sibling")]<-dat[a,c("sibling")]}
}

all_pairs$Sibling[all_pairs$Country1=="Germany"]<-0
all_pairs$Sibling[all_pairs$Country2=="Germany"]<-0



all_pairs$Country1_GATT[all_pairs$Year<1940]<-0
all_pairs$Country2_GATT[all_pairs$Year<1940]<-0

all_pairs$Country1_EU[all_pairs$Year<1940]<-0
all_pairs$Country2_EU[all_pairs$Year<1940]<-0

all_pairs<-all_pairs[-1,]

all_pairs$Could_Play_GS<-NA
n<-nrow(all_pairs)
for(i in 1:n){
  group<-real_groups[real_groups$Year==all_pairs$Year[i],]
  pot1<-which(group==as.character(all_pairs$Country1[i]),arr.ind=T)[2]	
  pot2<-which(group==as.character(all_pairs$Country2[i]),arr.ind=T)[2]		
  if(pot1==pot2){all_pairs$Could_Play_GS[i]<-0}
  if(pot1!=pot2){all_pairs$Could_Play_GS[i]<-1}
}

seeded<-c("Austria","Brazil","United Kingdom","France","Hungary","Italy","Turkey","Uruguay")
unseeded<-c("Yugoslavia","Mexico","South Korea","West Germany","Czechoslovakia","Scotland","Belgium","Switzerland")

index<-which(all_pairs$Year==1954)

all_pairs$Could_Play_GS[index]<-0

for(i in index){
  if((all_pairs$Country1[i]%in%seeded)+
     (all_pairs$Country2[i]%in%seeded)==1) 
    all_pairs$Could_Play_GS[i]<-1}

all_pairs$Could_Play_GS[all_pairs$Year==1930]<-1
all_pairs$Could_Play_GS[all_pairs$Year==1930&
                          all_pairs$Country1==all_pairs$Country2]<-0

seeded<-c("Argentina","Brazil","United States","Uruguay")

all_pairs$Could_Play_GS[all_pairs$Year==1930& 
                          all_pairs$Country1%in%seeded& 
                          all_pairs$Country2%in%seeded]<-0







seeded<-c("United Kingdom","Brazil","Italy","Uruguay")

all_pairs$Could_Play_GS[all_pairs$Year==1962 & 
                          all_pairs$Country1%in%seeded & 
                          all_pairs$Country2%in%seeded]<-0


seeded<-c("United Kingdom","Brazil","Italy","West Germany")

all_pairs$Could_Play_GS[all_pairs$Year==1966 & 
                          all_pairs$Country1%in%seeded & 
                          all_pairs$Country2%in%seeded]<-0




seeded<-c("Uruguay","Brazil","Italy","West Germany")

all_pairs$Could_Play_GS[all_pairs$Year==1974 & 
                          all_pairs$Country1%in%seeded & 
                          all_pairs$Country2%in%seeded]<-0







Pot_2_1998<-c(real_groups[real_groups$Year==1998,2],
              real_groups[real_groups$Year==1998,][2,3])

all_pairs$Could_Play_GS[all_pairs$Year==1998 & 
                          all_pairs$Country1%in%Pot_2_1998 & 
                          all_pairs$Country2%in%Pot_2_1998]<-1



Pot_2_2002<-c(real_groups[real_groups$Year==2002,2],
              real_groups[real_groups$Year==2002,][1:3,3])

all_pairs$Could_Play_GS[all_pairs$Year==2002 & 
                          all_pairs$Country1%in%Pot_2_2002 & 
                          all_pairs$Country2%in%Pot_2_2002]<-1


Pot_1_Europe<-real_groups[real_groups$Year==2006,1][c(3:6,8)]

all_pairs$Could_Play_GS[all_pairs$Year==2006 & 
                          all_pairs$Country1%in%Pot_1_Europe & 
                          all_pairs$Country2=="Serbia-Montenegro"]<-0
all_pairs$Could_Play_GS[all_pairs$Year==2006 & 
                          all_pairs$Country1=="Serbia-Montenegro" & 
                          all_pairs$Country2%in%Pot_1_Europe]<-0



Pot_1_SA_2014<-real_groups[real_groups$Year==2014,][c(1,3,4,6),1]
Pot_2_SA_2014<-real_groups[real_groups$Year==2014,][c(2,5),2]

all_pairs$Could_Play_GS[all_pairs$Year==2014 & 
                          all_pairs$Country1%in%Pot_1_SA_2014 & 
                          all_pairs$Country2%in%Pot_2_SA_2014]<-0
all_pairs$Could_Play_GS[all_pairs$Year==2014 & 
                          all_pairs$Country1%in%Pot_2_SA_2014 & 
                          all_pairs$Country2%in%Pot_1_SA_2014]<-0


all_pairs$Could_Play_GS[all_pairs$Year==2018&
                        all_pairs$Country1=="Argentina"&
                        all_pairs$Country2=="Uruguay"]<-0

all_pairs$Could_Play_GS[all_pairs$Year==2018&
                          all_pairs$Country1=="Argentina"&
                          all_pairs$Country2=="Peru"]<-0

all_pairs$Could_Play_GS[all_pairs$Year==2018&
                          all_pairs$Country1=="Argentina"&
                          all_pairs$Country2=="Colombia"]<-0

all_pairs$Could_Play_GS[all_pairs$Year==2018&
                          all_pairs$Country1=="Brazil"&
                          all_pairs$Country2=="Uruguay"]<-0

all_pairs$Could_Play_GS[all_pairs$Year==2018&
                          all_pairs$Country1=="Brazil"&
                          all_pairs$Country2=="Peru"]<-0

all_pairs$Could_Play_GS[all_pairs$Year==2018&
                          all_pairs$Country1=="Brazil"&
                          all_pairs$Country2=="Colombia"]<-0

all_pairs$Could_Play_GS[all_pairs$Year==2018&
                          all_pairs$Country1=="Costa Rica"&
                          all_pairs$Country2=="Mexico"]<-0





all_pairs$Both_GATT<-as.numeric(all_pairs$Country1_GATT==1 &
                                all_pairs$Country2_GATT==1)

all_pairs$Both_NonGATT<-as.numeric(all_pairs$Country1_GATT==0 &
                                   all_pairs$Country2_GATT==0)

all_pairs$One_GATT<-as.numeric(all_pairs$Country1_GATT!=
                               all_pairs$Country2_GATT)

all_pairs$Both_EU<-as.numeric(all_pairs$Country1_EU==1 &
                              all_pairs$Country2_EU==1)

all_pairs$Both_NonEU<-as.numeric(all_pairs$Country1_EU==0 &
                                 all_pairs$Country2_EU==0)

all_pairs$One_EU<-as.numeric(all_pairs$Country1_EU!=
                             all_pairs$Country2_EU)

all_pairs$Combined_GDP<-as.numeric(all_pairs$Country1_GDP+
                                   all_pairs$Country2_GDP)

all_pairs$Combined_GDP_Cap<-as.numeric(all_pairs$Country1_GDP_Cap+
                                       all_pairs$Country2_GDP_Cap)

all_pairs$Dist[is.na(all_pairs$Dist)==T]<-
all_pairs$Dist1[is.na(all_pairs$Dist)==T]

all_pairs$ln_Dist<-log(all_pairs$Dist)



all_pairs$Contiguous<-as.numeric(all_pairs$Contiguous)


all_pairs$Both_EU[all_pairs$Year<1950]<-0
all_pairs$One_EU[all_pairs$Year<1950]<-0
all_pairs$Both_NonEU[all_pairs$Year<1950]<-0

all_pairs$Both_GATT[all_pairs$Year<1950]<-0
all_pairs$One_GATT[all_pairs$Year<1950]<-0
all_pairs$Both_NonGATT[all_pairs$Year<1950]<-0

rm(dat)
rm(covs1)
rm(covs2)



countries<-c("Brazil","Romania", "Chile", "Paraguay", 
             "Bolivia", "Peru","France","Mexico", 
             "Yugoslavia","Uruguay", "Belgium", "United States",
             "Argentina", "Spain", "Switzerland", "Egypt",
             "Sweden","Germany", "Italy", "Netherlands",
             "Czechoslovakia","Hungary", "Norway","Poland", 
             "Dutch East Indies", "Cuba","United Kingdom","Austria",
             "Scotland","South Korea", "West Germany","Turkey", 
             "Soviet Union","Northern Ireland","Wales", "Colombia", 
             "Bulgaria","Portugal","North Korea", "El Salvador",
             "Morocco", "Israel","Australia", "Zaire",
             "East Germany","Haiti", "Tunisia", "Iran", 
             "Honduras","New Zealand", "Cameroon","Kuwait", 
             "Algeria", "Iraq","Canada","Denmark",
             "Ireland", "UAE", "Costa Rica","Russia", 
             "Greece","Saudi Arabia","Nigeria", "Croatia",
             "Jamaica", "Japan", "South Africa","Slovenia", 
             "China", "Ecuador", "Senegal", "Ivory Coast",
             "Togo","Ghana", "Czech Republic","Ukraine",
             "Trinidad-Tobago","Angola","Slovakia",
             "Bosnia-Herzegovina", "Iceland", "Panama") 


continents<-c("South America","Europe","South America","South America",
              "South America","South America","Europe","North America",
              "Europe","South America","Europe","North America",
              "South America","Europe","Europe","Africa",
              "Europe","Europe","Europe","Europe",
              "Europe","Europe","Europe","Europe",
              "Pacific","North America","Europe","Europe",
              "Europe","Asia","Europe","Europe",
              "Europe","Europe","Europe","South America",
              "Europe","Europe","Asia","North America",
              "Africa","Asia","Pacific","Africa",
              "Europe","North America","Africa","A",
              "North America","Pacific","Africa","Asia",
              "Africa","Asia","North America","Europe",
              "Europe","Asia","North America","Europe",
              "Europe","Asia","Africa", "Europe",
              "North America", "Asia", "Africa","Europe", 
              "Asia", "South America", "Africa", "Africa",
              "Africa","Africa", "Europe","Europe",
              "North America","Africa","Europe","Europe", 
              "Europe", "North America")

all_pairs$Continent1<-NA
all_pairs$Continent2<-NA

for(i in 1:nrow(all_pairs)){
  index1<-which(countries==all_pairs$Country1[i])
  if(length(index1)==1) all_pairs$Continent1[i]<-continents[index1]
  index2<-which(countries==all_pairs$Country2[i])
  if(length(index2)==1) all_pairs$Continent2[i]<-continents[index2]	
}


all_pairs$Same_Continent<-as.numeric(all_pairs$Continent1==all_pairs$Continent2)





all_pairs$Country1[all_pairs$Country1=="Russia"&
                     all_pairs$Year<1991]<-"Soviet Union"

all_pairs$Country2[all_pairs$Country2=="Russia"&
                   all_pairs$Year<1991]<-"Soviet Union"

all_pairs$Country1[all_pairs$Country1=="Russia"&
                     all_pairs$Year<1991]<-"Soviet Union"

all_pairs$Country1[all_pairs$Country1=="Yugoslavia"&
                     all_pairs$Year==2006]<-"Serbia-Montenegro"

all_pairs$Country2[all_pairs$Country2=="Yugoslavia"&
                     all_pairs$Year==2006]<-"Serbia-Montenegro"

all_pairs$Country1[all_pairs$Country1=="Yugoslavia"&
                     all_pairs$Year>2006]<-"Serbia"

all_pairs$Country2[all_pairs$Country2=="Yugoslavia"&
                     all_pairs$Year>2006]<-"Serbia"



all_pairs$ln_Country1_GDP <- log(all_pairs$Country1_GDP)
all_pairs$ln_Country2_GDP <- log(all_pairs$Country2_GDP)



# Played Before

all_pairs$Country_Index<-NA

for(i in 1:nrow(all_pairs)){
  all_pairs$Country_Index[i]<-paste(sort(c(all_pairs$Country1[i],
                                           all_pairs$Country2[i])),
                                    collapse = " ")
  
}

all_pairs$Country_Index2<-"0"

for(i in 1:nrow(all_pairs)){
  if(all_pairs$Group_Stage[i]==1|all_pairs$Knockout_Stage[i]==1){
    all_pairs$Country_Index2[i]<-paste(sort(c(all_pairs$Country1[i],
                                              all_pairs$Country2[i])),
                                       collapse = " ")}
  
}



all_pairs$Played_Before_Ever<-0

for(i in 2:nrow(all_pairs)){
  if(all_pairs$Country_Index[i]%in%(all_pairs$Country_Index2[1:(i-1)])){
    all_pairs$Played_Before_Ever[i]<-1}
}





all_pairs$Played_Before_Last_10_Years<-0



for(i in 2:nrow(all_pairs)){
  if(all_pairs$Country_Index[i]%in%(all_pairs$Country_Index2[1:(i-1)])){
    rows<-all_pairs[all_pairs$Country_Index2==all_pairs$Country_Index[i],]
    max_year<-max(rows$Year)
    if(all_pairs$Year[i]<=(max_year+10)) all_pairs$Played_Before_Last_10_Years[i]<-1}
}


all_pairs$TradeDrop<-as.numeric(all_pairs$Bi_Trade_Post<all_pairs$Bi_Trade_Pre)




all_pairs$Prediction<-NA

all_pairs$y<-NA

xs<--5:-1
for(i in 1:nrow(all_pairs)){
  ys<-all_pairs[i,c("Bi_Trade_M5","Bi_Trade_M4","Bi_Trade_M3","Bi_Trade_M2","Bi_Trade_Pre")]	
  if(sum(is.na(ys))<4){reg<-lm(as.numeric(ys)~xs); all_pairs$Prediction[i]<-as.numeric(reg$coefficients[1])
  all_pairs$y[i]<-as.numeric(all_pairs$Bi_Trade_Post[i]<all_pairs$Prediction[i])
  }}




all_pairs$Prediction_Prev<-NA

all_pairs$ym1<-NA

xs<--5:-1
for(i in 1:nrow(all_pairs)){
  ys<-all_pairs[i,c("Bi_Trade_M6","Bi_Trade_M5","Bi_Trade_M4","Bi_Trade_M3","Bi_Trade_M2")]	
  if(sum(is.na(ys))<4){reg<-lm(as.numeric(ys)~xs); all_pairs$Prediction[i]<-as.numeric(reg$coefficients[1])
  all_pairs$ym1[i]<-as.numeric(all_pairs$Bi_Trade_Pre[i]<all_pairs$Prediction[i])
  }}



all_pairs$Prediction1<-NA

all_pairs$y1<-NA

xs<--5:-1
for(i in 1:nrow(all_pairs)){
  ys<-all_pairs[i,c("Imports1_Pre5","Imports1_Pre4","Imports1_Pre3","Imports1_Pre2","Imports1_Pre")]	
  if(sum(is.na(ys))<4){reg<-lm(as.numeric(ys)~xs); all_pairs$Prediction1[i]<-as.numeric(reg$coefficients[1])
  all_pairs$y1[i]<-as.numeric(all_pairs$Imports1[i]<all_pairs$Prediction1[i])
  }}




all_pairs$Prediction2<-NA

all_pairs$y2<-NA

xs<--5:-1
for(i in 1:nrow(all_pairs)){
  ys<-all_pairs[i,c("Imports2_Pre5","Imports2_Pre4","Imports2_Pre3","Imports2_Pre2","Imports2_Pre")]	
  if(sum(is.na(ys))<4){ reg<-lm(as.numeric(ys)~xs); all_pairs$Prediction2[i]<-as.numeric(reg$coefficients[1])
  all_pairs$y2[i]<-as.numeric(all_pairs$Imports2[i]<all_pairs$Prediction2[i])
  }}





all_pairs$Prediction1M1<-NA

all_pairs$y1m1<-NA

xs<--5:-1
for(i in 1:nrow(all_pairs)){
  ys<-all_pairs[i,c("Imports1_Pre6","Imports1_Pre5","Imports1_Pre4","Imports1_Pre3","Imports1_Pre2")]	
  if(sum(is.na(ys))<4){reg<-lm(as.numeric(ys)~xs); all_pairs$Prediction1M1[i]<-as.numeric(reg$coefficients[1])
  all_pairs$y1m1[i]<-as.numeric(all_pairs$Imports1_Pre[i]<all_pairs$Prediction1M1[i])
  }}





all_pairs$Prediction2M1<-NA

all_pairs$y2m1<-NA

xs<--5:-1
for(i in 1:nrow(all_pairs)){
  ys<-all_pairs[i,c("Imports2_Pre6","Imports2_Pre5","Imports2_Pre4","Imports2_Pre3","Imports2_Pre2")]	
  if(sum(is.na(ys))<4){reg<-lm(as.numeric(ys)~xs); all_pairs$Prediction2M1[i]<-as.numeric(reg$coefficients[1])
  all_pairs$y2m1[i]<-as.numeric(all_pairs$Imports2_Pre[i]<all_pairs$Prediction2M1[i])
  }}







max<-0.2

all_pairs$Percent_Change<-(all_pairs$Bi_Trade_Post-
                           all_pairs$Bi_Trade_Pre)/
                           all_pairs$Bi_Trade_Pre

all_pairs$Percent_Change_Adjusted<-all_pairs$Percent_Change

all_pairs$Percent_Change_Adjusted[all_pairs$Percent_Change_Adjusted>max&
                                  is.na(all_pairs$Percent_Change_Adjusted)==F]<-max

all_pairs$Percent_Change_Adjusted[all_pairs$Percent_Change_Adjusted<(-max)&
                                  is.na(all_pairs$Percent_Change_Adjusted)==F]<- -max

all_pairs$Percent_Change_Adjusted[all_pairs$Bi_Trade_Pre==0 &
                                  all_pairs$Bi_Trade_Post==0 &
                                  is.na(all_pairs$Bi_Trade_Pre)==F & 
                                  is.na(all_pairs$Bi_Trade_Post)==F]<- 0

all_pairs$Percent_Change_Adjusted[all_pairs$Bi_Trade_Pre==0 &
                                    all_pairs$Bi_Trade_Post>0 &
                                    is.na(all_pairs$Bi_Trade_Pre)==F & 
                                    is.na(all_pairs$Bi_Trade_Post)==F]<- max


all_pairs$Percent_Change_Prev<-(all_pairs$Bi_Trade_Pre-all_pairs$Bi_Trade_M2)/all_pairs$Bi_Trade_M2

all_pairs$Percent_Change_Prev_Adjusted<-all_pairs$Percent_Change_Prev

all_pairs$Percent_Change_Prev_Adjusted[all_pairs$Percent_Change_Prev_Adjusted>max&is.na(all_pairs$Percent_Change_Prev_Adjusted)==F]<-max

all_pairs$Percent_Change_Prev_Adjusted[all_pairs$Percent_Change_Prev_Adjusted<(-max)&is.na(all_pairs$Percent_Change_Prev_Adjusted)==F]<- -max


all_pairs$Percent_Change_Prev_Adjusted[all_pairs$Bi_Trade_M2==0 &
                                    all_pairs$Bi_Trade_Pre==0 &
                                    is.na(all_pairs$Bi_Trade_M2)==F & 
                                    is.na(all_pairs$Bi_Trade_Pre)==F]<- 0

all_pairs$Percent_Change_Prev_Adjusted[all_pairs$Bi_Trade_M2==0 &
                                    all_pairs$Bi_Trade_Pre>0 &
                                    is.na(all_pairs$Bi_Trade_M2)==F & 
                                    is.na(all_pairs$Bi_Trade_Pre)==F]<- max


all_pairs$Adjusted_Trade<-(all_pairs$Percent_Change_Adjusted+1)*
                          all_pairs$Bi_Trade_Pre

all_pairs$ln_Bi_Trade_Pre<-log(all_pairs$Bi_Trade_Pre+1)

all_pairs$ln_Adjusted_Trade<-log(all_pairs$Adjusted_Trade+1)



all_pairs$ln_Bi_Trade_M2<-log(all_pairs$Bi_Trade_M2+1)

all_pairs$Adjusted_Trade_Prev<-(all_pairs$Percent_Change_Prev_Adjusted+1)*
  all_pairs$Bi_Trade_M2

all_pairs$ln_Adjusted_Trade_Prev<-log(all_pairs$Adjusted_Trade_Prev+1)

all_pairs$y[all_pairs$Could_Play_GS==1&
            is.na(all_pairs$y)==T&
            is.na(all_pairs$Bi_Trade_Pre)==F&
            is.na(all_pairs$Bi_Trade_Post)==F]<-
as.numeric(all_pairs$Bi_Trade_Post[all_pairs$Could_Play_GS==1&
                         is.na(all_pairs$y)==T&
                         is.na(all_pairs$Bi_Trade_Pre)==F&
                         is.na(all_pairs$Bi_Trade_Post)==F]<
             all_pairs$Bi_Trade_Pre[all_pairs$Could_Play_GS==1&
                                       is.na(all_pairs$y)==T&
                                       is.na(all_pairs$Bi_Trade_Pre)==F&
                                       is.na(all_pairs$Bi_Trade_Post)==F])  

setwd("~/Dropbox/WCNaturalExperiment/Data_and_Code")

write.csv(all_pairs,"All_Pairs_July24.csv",row.names = F)



real_data_full_all<-all_pairs[all_pairs$Group_Stage==1,]

real_data_full<-real_data_full_all[unlist(is.na(real_data_full_all$Bi_Trade_Pre)==FALSE&is.na(real_data_full_all$Bi_Trade_Post)==FALSE),]

real_data_full_all<-all_pairs[all_pairs$Group_Stage==1,]

real_data_full<-real_data_full_all[is.na(real_data_full_all$y)==F,]


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





write.csv(real_data_full,"RealDataFull_July24.csv",row.names=F)

write.csv(real_data,"RealData_July24.csv",row.names=F)
