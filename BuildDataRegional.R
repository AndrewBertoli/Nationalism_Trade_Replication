# We will start by loading the following package.
require("readxl")

# We will next set the working directory to the folder with the replication
# materials from the "Nationalism_Trade_Replication" repository from Github
setwd("~/Downloads/Nationalism_Trade_Replication-main")

# We can read in the file with the World Cup groups.
real_groups<-read.csv("FIFAGroups_Expanded.csv",stringsAsFactors=FALSE,sep=";")

# To make the data merging easier, we will convert the Soviet Union
# to Russia and both Serbia and Serbia-Montenegro to Yugoslavia (and 
# change them back later)

real_groups$Country1[real_groups$Country1=="Soviet Union"]<-"Russia"
real_groups$Country2[real_groups$Country2=="Soviet Union"]<-"Russia"

real_groups$Country4[real_groups$Country4=="Serbia"]<-"Yugoslavia"
real_groups$Country2[real_groups$Country2=="Serbia-Montenegro"]<-"Yugoslavia"
real_groups$Country4[real_groups$Country4=="Serbia-Montenegro"]<-"Yugoslavia"

real_groups$Country1[real_groups$Country1=="DRC"]<-"Zaire"
real_groups$Country2[real_groups$Country2=="DRC"]<-"Zaire"
real_groups$Country3[real_groups$Country3=="DRC"]<-"Zaire"
real_groups$Country4[real_groups$Country4=="DRC"]<-"Zaire"

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
            asia1976,asia1980,asia1984,asia1988,asia1992,
            asia1996,asia2000,asia2004,asia2007,
            africa1968,africa1970,africa1972,africa1974,
            africa1976,africa1978,africa1980,africa1982,
            africa1984,africa1986,africa1988,africa1990,
            africa1992,africa1994,africa1996,africa1998,
            africa2000,africa2002,africa2004,africa2006,
            africa2008,africa2010,africa2012,euro2012,
            africa2013,africa2015,africa2017,africa2019,
            africa2022,euro2016,euro2021)
            
# We will also make a vector with corresponding years for the list.
years<-c(1980,1984,1988,1992,1996,2000,
        2004,2008,1976,1980,1984,1988,1992,1996,2000,
        2004,2007,1968,1970,1972,1974,1976,1978,1980,
        1982,1984,1986,1988,1990,1992,1994,1996,1998,
        2000,2002,2004,2006,2008,2010,2012,2012,2013,
        2015,2017,2019,2022,2016,2021)

# We can now make a vector with the corresponding continents.
event<-c("Europe","Europe","Europe","Europe",
        "Europe","Europe","Europe","Europe",
        "Asia","Asia","Asia","Asia","Asia","Asia","Asia",
        "Asia","Asia","Africa","Africa","Africa","Africa",
        "Africa","Africa","Africa","Africa","Africa","Africa",
        "Africa","Africa","Africa","Africa","Africa","Africa",
        "Africa","Africa","Africa","Africa","Africa","Africa",
        "Africa","Europe","Africa","Africa","Africa","Africa",
        "Africa","Europe","Europe")

# We will next read in a data frame that includes all the countries that went
# to the World Cup, Euro, or African Cup of Nations. The data file contains
# the country names and years, but no other information.
covariates<-read.csv("Covariates_Regional.csv",stringsAsFactors=FALSE, sep=";")

# We can next read in a data frame with information about the countries that
# played each other in the tournaments.
rc_games<-read.csv("RCGames.csv",stringsAsFactors=F,sep=";")

rc_games$Score2[rc_games$Penalities==1]<-
  rc_games$Score1[rc_games$Penalities==1]-1

# We will again convert the Soviet Union to Russia and both Serbia and 
# Serbia-Montenegro to Yugoslavia to make the data merging easier
covariates$Country[covariates$Country=="Soviet Union"]<-"Russia"
covariates$Country[covariates$Country=="Serbia"]<-"Yugoslavia"
covariates$Country[covariates$Country=="Serbia-Montenegro"]<-"Yugoslavia"


rc_games$Country1[rc_games$Country1=="Soviet Union"]<-"Russia"
rc_games$Country1[rc_games$Country1=="Serbia"]<-"Yugoslavia"
rc_games$Country1[rc_games$Country1=="Serbia-Montenegro"]<-"Yugoslavia"


rc_games$Country2[rc_games$Country2=="Soviet Union"]<-"Russia"
rc_games$Country2[rc_games$Country2=="Serbia"]<-"Yugoslavia"
rc_games$Country2[rc_games$Country2=="Serbia-Montenegro"]<-"Yugoslavia"







# We will now load the function that will adapt the country names in the
# datasets that we load so that they match the country names in Covariates.
source("AdaptNames.R")


# You can download the National Material Capabilities (v6.0)
# data from this website:
# https://correlatesofwar.org/data-sets/national-material-capabilities/

setwd("~/Downloads/NMC_Documentation-6/NMC-60-abridged")
nmc<-read.csv("NMC-60-abridged.csv")



# Input country names into the data set that match the country names in
# our data frame covariates.
nmc$stateabb<-adapt_names_cow(nmc$stateabb)

# For each row of covariates, extract the information for that country 
# year from nmc. Note that some rows of covariates do not have corresponding
# rows in nmc.
for(i in 1:nrow(covariates)){
index<-which(nmc$stateabb==covariates$Country[i]&
               nmc$year==min(2012,covariates$Year[i]))
if(length(index)==1) covariates[i,3:7]<-nmc[index,c(6,4,5,8,9)]
}



# You can download the Polity V data from this website:
# https://www.systemicpeace.org/inscrdata.html
# We will use the Excel Series of the Polity V Annual Time-Series, 1946-2018. 

setwd("~/Downloads")
polity<-read_excel("p5v2018.xls")

# Convert the object into a data frame.
polity<-data.frame(polity)

# Input country names into the data set that match the country names in
# our data frame covariates.
polity$country<-adapt_names_pol(polity$country)

# Create a new column of covariates called regime that will record whether
# countries were or were not democracies.
covariates$regime<-NA

# Fill in the column regime with 1 if the country was a democracy (had a
# Polity V score of 6 or higher) and 0 if not.
for(i in 1:nrow(covariates)){
  index<-which(polity$country==covariates$Country[i]&
               polity$year==covariates$Year[i])
  if(length(index)==1){
  covariates$regime[i]<-as.numeric(polity$polity[index]>5)}
}




# We can now remove the data from polity from the workspace.
rm(polity)

# We will code Iceland as a democracy (it appears only once -- in 2018)
covariates$regime[covariates$Country=="Iceland"]<-1

# We will also code Yugoslavia (Serbia) as a democracy in 2006.
covariates$regime[covariates$Country=="Yugoslavia"&covariates$Year==2006]<-1

# We can next read in the data set on state independence years from the 
# Correlates of War website.
ind<-read.csv("https://correlatesofwar.org/wp-content/uploads/states2016.csv")

# Input country names into the data set that match the country names in
# our data frame covariates.
ind$stateabb<-adapt_names_cow(ind$stateabb)

# Create a new column of covariates called IndYear that will record the
# independence years for each country.
covariates$IndYear<-NA

# Fill in the column IndYear with 1 if the country was a democracy that year (had a
# Polity V score of 6 or higher) and 0 if not.
for(i in 1:nrow(covariates)){
  covariates$IndYear[i]<-
  ind$styear[ind$stateabb==covariates$Country[i]][1]
}

# Create a column of 1s called SoccerMostPopular, since soccer was the most popular
# sport for all countries.
covariates$SoccerMostPopular<-1

# You can download the data on contiguity from this website:
# https://correlatesofwar.org/data-sets/direct-contiguity/

setwd("~/Downloads/DirectContiguity320")
contiguous<-read.csv("contdird.csv")


# Input country names into the data set that match the country names in
# our data frame all_pairs.

contiguous$state1ab<-adapt_names_cow(contiguous$state1ab)

contiguous$state2ab<-adapt_names_cow(contiguous$state2ab)


# You can download the data on Militarized Interstate
# Disputes from this website:
# https://correlatesofwar.org/data-sets/mids/

setwd("~/Downloads/MID-5-Data-and-Supporting-Materials")
data<-read.csv("MIDB 5.0.csv", stringsAsFactors=F)

# Input country names into the data set that match the country names in
# our data frame all_pairs.

data$stabb<-adapt_names_cow(data$stabb)

# You can download the COW trade data from this website:
# https://correlatesofwar.org/data-sets/bilateral-trade/

setwd("~/Downloads/COW_Trade_4.0")
trade<-read.csv("Dyadic_Cow_4.0.csv",stringsAsFactors=FALSE)

# To reduce the size of the data frame, we will remove all the rows before 
# 1913.
trade<-trade[-which(trade$year<1945),]

# Convert the -9s into NAs
trade$flow1[trade$flow1==-9]<-NA
trade$flow2[trade$flow2==-9]<-NA




# Input country names into the data set that match the country names in
# our data frame all_pairs.

trade$importer1<-adapt_names_trade(trade$importer1)
trade$importer2<-adapt_names_trade(trade$importer2)

# Reset the working directory to the folder with the replication materials
# from the repository.
setwd("~/Downloads/Nationalism_Trade_Replication-main")

# Set the months as June for all cases.
month<-rep(6,27)

# Create a new data frame called all_pairs that has pairs of countries for
# each tournament.
all_pairs<-matrix(0,nrow=1,ncol=5)
for(k in 1:length(groups)){
for(j in 1:length(as.matrix(groups[[k]]))){
new_pairs<-cbind(rep(as.matrix(groups[[k]])[j],length(as.matrix(groups[[k]]))-1),
				 c(as.matrix(groups[[k]])[-j]),years[k],month[k],event[k])
all_pairs<-rbind(all_pairs,new_pairs)	
}}







# Remove the rows of all_pairs where one or both of the country names are "NONE".
all_pairs<-all_pairs[-unique(c(which(all_pairs[,1]=="NONE"),
                               which(all_pairs[,2]=="NONE"))),]


# Remove the rows of all_pairs where one or both of the country names are years.
all_pairs<-all_pairs[-unique(c(which(all_pairs[,1]%in%as.character(years) ),
                               which(all_pairs[,2]%in%as.character(years)))),]   
                               
# Remove the rows of all_pairs where one or both of the country names are "Africa",
# "Europe", or "Asia".
all_pairs<-all_pairs[-unique(c(which(all_pairs[,1]%in%c("Europe","Asia","Africa")),
                               which(all_pairs[,2]%in%c("Europe","Asia","Africa")))),]    
                               

# all_pairs[,1][all_pairs[,1]=="DRC"]<-"Zaire"                               
# all_pairs[,2][all_pairs[,2]=="DRC"]<-"Zaire"                                
                                                                                         
                              
# Put the country names in alphabetical order in each row.
for(i in 1:length(all_pairs[,1])){all_pairs[i,1:2]<-
  sort(all_pairs[i,1:2])}

# Remove any rows that are duplicates.
all_pairs<-unique(all_pairs,rows=TRUE)

# Turn the object all_pairs into a data frame.
all_pairs<-data.frame(all_pairs,stringsAsFactors=FALSE)

# Assign column names to all_pairs.
colnames(all_pairs)<-c("Country1","Country2","Year","Month","Event")



# Make the year and month columns numeric.
all_pairs$Year<-as.numeric(all_pairs$Year)
all_pairs$Month<-as.numeric(all_pairs$Month)

# Add European pairs that played each other form 1960-1976.
all_pairs<-rbind(all_pairs,rc_games[rc_games$Event=="Europe"&rc_games$Year<1980,c("Country1","Country2","Year","Month","Event")])

all_pairs<-rbind(all_pairs,rc_games[rc_games$Event=="Africa"&rc_games$Year<1968,c("Country1","Country2","Year","Month","Event")])

# Add columns to all_pairs to store the group stage scores of
# each country, for countries that played in the group stage.
all_pairs$GS_Score1<-NA
all_pairs$GS_Score2<-NA

# Add columns to all_pairs to store the knockout stage scores of
# each country, for countries that played in the knockout stage.
all_pairs$KS_Score1<-NA
all_pairs$KS_Score2<-NA


# The code below adds the group stage scores to all_pairs.
for(i in 1:nrow(all_pairs)){
	index<-which(rc_games$Country1==all_pairs$Country1[i]&
	               rc_games$Country2==all_pairs$Country2[i]&
	               rc_games$Year==all_pairs$Year[i]&
	               rc_games$GroupStage==1)
	if(length(index)==1) {all_pairs$GS_Score1[i]<-rc_games$Score1[index] 
	all_pairs$GS_Score2[i]<-rc_games$Score2[index]}
	index<-which(rc_games$Country1==all_pairs$Country2[i]&
	              rc_games$Country2==all_pairs$Country1[i]&
	              rc_games$Year==all_pairs$Year[i]&
	              rc_games$GroupStage==1)
	if(length(index)==1) {all_pairs$GS_Score1[i]<-rc_games$Score2[index] 
	all_pairs$GS_Score2[i]<-rc_games$Score1[index]}}
	
	
	
# The code below adds the knockout stage scores to all_pairs.	
for(i in 1:nrow(all_pairs)){
	index<-which(rc_games$Country1==all_pairs$Country1[i]&
	               rc_games$Country2==all_pairs$Country2[i]&
	               rc_games$Year==all_pairs$Year[i]&
	               rc_games$GroupStage==0)
	if(length(index)==1) {all_pairs$KS_Score1[i]<-rc_games$Score1[index] 
	all_pairs$KS_Score2[i]<-rc_games$Score2[index]}
	index<-which(rc_games$Country1==all_pairs$Country2[i]&
	              rc_games$Country2==all_pairs$Country1[i]&
	              rc_games$Year==all_pairs$Year[i]&
	              rc_games$GroupStage==0)
	if(length(index)==1) {all_pairs$KS_Score1[i]<-rc_games$Score2[index] 
	all_pairs$KS_Score2[i]<-rc_games$Score1[index]}}
	


	
	
# The code below switches the order of the countries in each row of all_pairs
# so that if they played each other and their was a winner, the country that
# won will be set as Country 1 and the country that lost will be set as Country 2.
# For the rare situation where two countries played in both the group stage and
# knockout stage, we will consider the winner of the knockout stage game to be the
# winner.

for(i in 1:nrow(all_pairs)){
	if(is.na(all_pairs$GS_Score1[i])==F){
		if(all_pairs$GS_Score1[i]<all_pairs$GS_Score2[i]){
		all_pairs[i,c("Country1","Country2")]<-all_pairs[i,c("Country2","Country1")]
		all_pairs[i,c("GS_Score1","GS_Score2")]<-all_pairs[i,c("GS_Score2","GS_Score1")]}}
	if(is.na(all_pairs$KS_Score1[i])==F){
		if(all_pairs$KS_Score1[i]<all_pairs$KS_Score2[i]){
		all_pairs[i,c("Country1","Country2")]<-all_pairs[i,c("Country2","Country1")]
		all_pairs[i,c("KS_Score1","KS_Score2")]<-all_pairs[i,c("KS_Score2","KS_Score1")]}}}	






# Create a function to extract the trade data for a pair of countries
# from the data frame trade.
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

# Extract the trade data for all pairs of countries in all_pairs.
t<-apply(as.matrix(all_pairs),1,trade_flow)

# Add the extracted trade data as a new column of all_pairs.
trade1<-rep(0, length(t))
all_pairs<-cbind(all_pairs,t(t))

# Create a function to extract the covariate information for each country
# in the Country 1 column of all_pairs.
gest_covs1<-function(v){
covs<-covariates[covariates$Country==v[1]&
                   covariates$Year==v[3],]	
return(covs[1,3:10])	
}

# Create a function to extract the covariate information for each country
# in the Country 2 column of all_pairs.
gest_covs2<-function(v){
covs<-covariates[covariates$Country==v[2]&
                covariates$Year==v[3],]	
return(covs[1,3:10])	
}

# Extract the covariate information for each country in the Country 1 column 
# of all_pairs.
covs1<-do.call(rbind,apply(all_pairs,1,gest_covs1))

# Extract the covariate information for each country in the Country 2 column 
# of all_pairs.
covs2<-do.call(rbind,apply(all_pairs,1,gest_covs2))

# Merge the data frame all_pairs with the extracted covariate information.
all_pairs<-data.frame(all_pairs,covs1,covs2,stringsAsFactors=F)

# Remove the data frame covariates.
rm(covariates)




# Create a column of all_pairs called Both_Dem that is 1 if both countries were 
# democracies and 0 otherwise. 
all_pairs$Both_Dem<-as.numeric(all_pairs$regime==1&all_pairs$regime.1==1)

# Create a column of all_pairs called Both_NonDem that is 1 if both countries were 
# non-democracies and 0 otherwise. 
all_pairs$Both_NonDem<-as.numeric(all_pairs$regime==0&all_pairs$regime.1==0)

# Create a column of all_pairs called Diff_Regime that is 1 if exactly one of the 
# two countries was a democracies and 0 otherwise. 
all_pairs$Diff_Regime<-as.numeric(all_pairs$regime!=all_pairs$regime.1)

# Create a column of all_pairs called Both_Soccer that is 1 if soccer was the most 
# popular sport for both countries and 0 otherwise. 
all_pairs$Both_Soccer<-as.numeric(all_pairs$SoccerMostPopular==1&all_pairs$SoccerMostPopular.1==1)

# Create a column of all_pairs called Both_NonSoccer that is 1 if soccer was not the most 
# popular sport for either country and 0 otherwise. 
all_pairs$Both_NonSoccer<-as.numeric(all_pairs$SoccerMostPopular==0&all_pairs$SoccerMostPopular.1==0)

# Create a column of all_pairs called One_Soccer that is 1 if soccer was the most 
# popular sport for exactly one of the two countries and 0 otherwise. 
all_pairs$One_Soccer<-as.numeric(all_pairs$SoccerMostPopular!=all_pairs$SoccerMostPopular.1)

# Create a new column of all_pairs to store whether each pair of countries was continguous.
all_pairs$Contiguous<-NA

# Code pairs of countries as 1 if they were continguous and 0 if they were not.
for(i in 1:nrow(all_pairs)){
a<-contiguous[c(which(contiguous$state1ab==as.character(all_pairs[i,1])),
                which(contiguous$state2ab==as.character(all_pairs[i,1]))),]
a<-a[c(which(a$state1ab==as.character(all_pairs[i,2])),
       which(a$state2ab==as.character(all_pairs[i,2]))),]
a<-a[which(a$year==min(2016,as.numeric(all_pairs[i,3]))),]
if(dim(a)[1]==0){all_pairs$Contiguous[i]<-0}
if(dim(a)[1]==2){all_pairs$Contiguous[i]<-1}
}

# Remove the data frame contiguous from the workspace.
rm(contiguous)


# Create a new column of all_pairs to store the number of military disputes that each 
# pair of countries had against each other the year before the World Cup.
all_pairs$Total_Disputes_Before<-NA


# Extract the number of military disputes that each pair of countries had the year
# before the World Cup and store that information in all_pairs.
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

# Remove the data frame containing the information on militarized interstate
# disputes from the workspace.
rm(data)


# Create a new column of all_pairs that records whether countries had a military dispute 
# the year before the World Cup (yes=1, no=0).
all_pairs$Any_Disputes_Before<-as.numeric(all_pairs$Total_Disputes_Before>0)


# You can download the alliance data from this website:
# https://correlatesofwar.org/data-sets/formal-alliances/
# Download the .csv version.

setwd("~/Downloads/version4.1_csv")
alliances<-read.csv("alliance_v4.1_by_dyad_yearly.csv",stringsAsFactors = F)

# To reduce the size of the data frame, we will subset to cases after 1925.
alliances<-alliances[alliances$year>1925,]

# Reset the working directory to the folder with the replication materials
# from the repository.
setwd("~/Downloads/Nationalism_Trade_Replication-main")

# Input country names into the data set that match the country names in
# our data frame all_pairs.
alliances$state_name1<-adapt_names_trade(alliances$state_name1)
alliances$state_name2<-adapt_names_trade(alliances$state_name2)

# Create a function to check whether any pair of countries had an alliance
# the year before the World Cup (yes=1, no=0).
getalliances<-function(v){
alliances_year<-alliances[alliances$year==(as.numeric(v[3])-1),]
a<-which(alliances_year$state_name1==as.character(v[1])&alliances_year$state_name2==as.character(v[2]))
b<-which(alliances_year$state_name1==as.character(v[2])&alliances_year$state_name2==as.character(v[1]))
d<-c(a,b)
return(as.numeric(length(d)>0))}

# Extract the information on which pairs of coountries had alliances and 
# merge it with all_pairs.
all_pairs<-cbind(all_pairs,apply(all_pairs,1,getalliances))


# Remove the data frame containing the information on alliances from the workspace.
rm(alliances)


# The data on distances between countries is available at this website:
# https://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=6
# Download the dist_cepii.xls dataset. 

setwd("~/Downloads")
distances<-read_xls("dist_cepii.xls")

# Convert the object distances into a data frame.
distances<-data.frame(distances)

# Input country names into the data set that match the country names in
# our data frame all_pairs.

distances$iso_o<-as.character(adapt_names_iso(distances$iso_o))
distances$iso_d<-as.character(adapt_names_iso(distances$iso_d))

# Create a function to extract the distance between countries.
# For more information on how the distances are calculated, see
# https://www.cepii.fr/CEPII/en/publications/wp/abstract.asp?NoDoc=3877
# Note that we are first focusing on the dist column of the data frame dist.

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

# Extract the distances between countries and store them
# as a new column of all_pairs.
all_pairs$dist1=apply(all_pairs,1,dist1)

# We can now create a function to extract the distance between 
# the capital cities of each pair of countries.
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

# Extract the distances between the capital cities of the country pairs
# and store this information as a new column of all_pairs.
all_pairs$capdist=apply(all_pairs,1,cap_dist)

# Remove the data frame distances.
rm(distances)


# Create a column that records whether countries played in 
# the group stage (yes=1, no=0).
all_pairs$Group_Stage<-as.numeric(is.na(all_pairs$GS_Score1)==F)

# Create a column that records whether countries played in 
# the knockout stage (yes=1, no=0).
all_pairs$Knockout_Stage<-as.numeric(is.na(all_pairs$KS_Score1)==F)


# Assign these column names to the data frame all_pairs.
colnames(all_pairs)<-c("Country1","Country2","Year","Month","Event",
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


# Reorder the columns of all_pairs.
all_pairs<-all_pairs[,c("Country1","Country2","Year","Month","Event",
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





# Create columns for the imports for each country in every row of all_pairs.
# all_pairs$Imports1_Pre15 will be Country 1's value of imports from Country 2
# 15 years before the World Cup year. Likewise, all_pairs$Imports2_Pre15 
# will be Country 2's value of imports from Country 1 15 years before the 
# World Cup year.

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

all_pairs$Imports1_Pre<-NA # Year before the tournament
all_pairs$Imports2_Pre<-NA

all_pairs$Imports1<-NA # Tournament year
all_pairs$Imports2<-NA

all_pairs$Imports1_Post1<-NA # First year after the tournament
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


# The code below fills in the import data for each row of all_pairs, if available.
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

# Remove the trade data frame from the workspace.
rm(trade)







# You can download the trade data for 2018 here:
# http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37
# The file is HS 92

setwd("~/Downloads/BACI_HS92_V202501")

years<-c(2018, 2015, 2017, 2019, 2022, 2016, 2021)

years<-c(2018, 2017, 2016, 2015)

i1<-c("Imports1_Pre15","Imports1_Pre14","Imports1_Pre13","Imports1_Pre12")
i2<-c("Imports2_Pre15","Imports2_Pre14","Imports2_Pre13","Imports2_Pre12")
b<-c("Bi_Trade_M15","Bi_Trade_M14","Bi_Trade_M13","Bi_Trade_M12")

dat<-read.csv("BACI_HS92_Y2003_V202501.csv",stringsAsFactors=F)

setwd("~/Downloads/Nationalism_Trade_Replication-main")

source("ChangeNamesUN.R")

for(j in 1:length(years)){
  
  index<-which(all_pairs$Year==years[j])

all_pairs[index,i1[j]]<-NA
all_pairs[index,i2[j]]<-NA
all_pairs[index,b[j]]<-NA


for(i in index){
  p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
  if(nrow(p)>0) all_pairs[i,i2[j]]<-sum(p$v)/1000
  p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
  if(nrow(p)>0) all_pairs[i,i1[j]]<-sum(p$v)/1000
  
}

all_pairs[index,b[j]]<-all_pairs[index,i1[j]]+all_pairs[index,i2[j]]

print(years[j])

}

rm(dat)



setwd("~/Downloads/BACI_HS92_V202501")

years<-c(2018, 2015, 2017, 2019, 2022, 2016, 2021)

years<-c(2019, 2018, 2017, 2016, 2015)

i1<-c("Imports1_Pre15","Imports1_Pre14","Imports1_Pre13","Imports1_Pre12","Imports1_Pre11")
i2<-c("Imports2_Pre15","Imports2_Pre14","Imports2_Pre13","Imports2_Pre12","Imports2_Pre11")
b<-c("Bi_Trade_M15","Bi_Trade_M14","Bi_Trade_M13","Bi_Trade_M12","Bi_Trade_M11")

dat<-read.csv("BACI_HS92_Y2004_V202501.csv",stringsAsFactors=F)

setwd("~/Downloads/Nationalism_Trade_Replication-main")

source("ChangeNamesUN.R")

for(j in 1:length(years)){
  
  index<-which(all_pairs$Year==years[j])
  
  all_pairs[index,i1[j]]<-NA
  all_pairs[index,i2[j]]<-NA
  all_pairs[index,b[j]]<-NA
  
  
  for(i in index){
    p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
    if(nrow(p)>0) all_pairs[i,i2[j]]<-sum(p$v)/1000
    p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
    if(nrow(p)>0) all_pairs[i,i1[j]]<-sum(p$v)/1000
    
  }
  
  all_pairs[index,b[j]]<-all_pairs[index,i1[j]]+all_pairs[index,i2[j]]
  
  print(years[j])
  
}

rm(dat)








setwd("~/Downloads/BACI_HS92_V202501")

years<-c(2018, 2015, 2017, 2019, 2022, 2016, 2021)

years<-c(2019, 2018, 2017, 2016, 2015)

i1<-c("Imports1_Pre14","Imports1_Pre13","Imports1_Pre12","Imports1_Pre11","Imports1_Pre10")
i2<-c("Imports2_Pre14","Imports2_Pre13","Imports2_Pre12","Imports2_Pre11","Imports2_Pre10")
b<-c("Bi_Trade_M14","Bi_Trade_M13","Bi_Trade_M12","Bi_Trade_M11","Bi_Trade_M10")

dat<-read.csv("BACI_HS92_Y2005_V202501.csv",stringsAsFactors=F)

setwd("~/Downloads/Nationalism_Trade_Replication-main")

source("ChangeNamesUN.R")

for(j in 1:length(years)){
  
  index<-which(all_pairs$Year==years[j])
  
  all_pairs[index,i1[j]]<-NA
  all_pairs[index,i2[j]]<-NA
  all_pairs[index,b[j]]<-NA
  
  
  for(i in index){
    p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
    if(nrow(p)>0) all_pairs[i,i2[j]]<-sum(p$v)/1000
    p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
    if(nrow(p)>0) all_pairs[i,i1[j]]<-sum(p$v)/1000
    
  }
  
  all_pairs[index,b[j]]<-all_pairs[index,i1[j]]+all_pairs[index,i2[j]]
  
  print(years[j])
  
}

rm(dat)







setwd("~/Downloads/BACI_HS92_V202501")

years<-c(2018, 2015, 2017, 2019, 2022, 2016, 2021)

years<-c(2019, 2018, 2017, 2016, 2015)

i1<-c("Imports1_Pre14","Imports1_Pre13","Imports1_Pre12","Imports1_Pre11","Imports1_Pre10")
i2<-c("Imports2_Pre14","Imports2_Pre13","Imports2_Pre12","Imports2_Pre11","Imports2_Pre10")
b<-c("Bi_Trade_M14","Bi_Trade_M13","Bi_Trade_M12","Bi_Trade_M11","Bi_Trade_M10")

dat<-read.csv("BACI_HS92_Y2005_V202501.csv",stringsAsFactors=F)

setwd("~/Downloads/Nationalism_Trade_Replication-main")

source("ChangeNamesUN.R")

for(j in 1:length(years)){
  
  index<-which(all_pairs$Year==years[j])
  
  all_pairs[index,i1[j]]<-NA
  all_pairs[index,i2[j]]<-NA
  all_pairs[index,b[j]]<-NA
  
  
  for(i in index){
    p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
    if(nrow(p)>0) all_pairs[i,i2[j]]<-sum(p$v)/1000
    p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
    if(nrow(p)>0) all_pairs[i,i1[j]]<-sum(p$v)/1000
    
  }
  
  all_pairs[index,b[j]]<-all_pairs[index,i1[j]]+all_pairs[index,i2[j]]
  
  print(years[j])
  
}

rm(dat)





setwd("~/Downloads/BACI_HS92_V202501")

years<-c(2018, 2015, 2017, 2019, 2022, 2016, 2021)

years<-c(2021,2019, 2018, 2017, 2016, 2015)

i1<-c("Imports1_Pre15","Imports1_Pre13","Imports1_Pre12","Imports1_Pre11","Imports1_Pre10","Imports1_Pre9")
i2<-c("Imports2_Pre15","Imports2_Pre13","Imports2_Pre12","Imports2_Pre11","Imports2_Pre10","Imports2_Pre9")
b<-c("Bi_Trade_M15","Bi_Trade_M13","Bi_Trade_M12","Bi_Trade_M11","Bi_Trade_M10","Bi_Trade_M9")

dat<-read.csv("BACI_HS92_Y2006_V202501.csv",stringsAsFactors=F)

setwd("~/Downloads/Nationalism_Trade_Replication-main")

source("ChangeNamesUN.R")

for(j in 1:length(years)){
  
  index<-which(all_pairs$Year==years[j])
  
  all_pairs[index,i1[j]]<-NA
  all_pairs[index,i2[j]]<-NA
  all_pairs[index,b[j]]<-NA
  
  
  for(i in index){
    p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
    if(nrow(p)>0) all_pairs[i,i2[j]]<-sum(p$v)/1000
    p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
    if(nrow(p)>0) all_pairs[i,i1[j]]<-sum(p$v)/1000
    
  }
  
  all_pairs[index,b[j]]<-all_pairs[index,i1[j]]+all_pairs[index,i2[j]]
  
  print(years[j])
  
}

rm(dat)







setwd("~/Downloads/BACI_HS92_V202501")

years<-c(2018, 2015, 2017, 2019, 2022, 2016, 2021)

years<-c(2022, 2021, 2019, 2018, 2017, 2016, 2015)

i1<-c("Imports1_Pre15","Imports1_Pre14",
      "Imports1_Pre12","Imports1_Pre11",
      "Imports1_Pre10","Imports1_Pre9",
      "Imports1_Pre8")
i2<-c("Imports2_Pre15","Imports2_Pre14",
      "Imports2_Pre12","Imports2_Pre11",
      "Imports2_Pre10","Imports2_Pre9",
      "Imports2_Pre8")
b<-c("Bi_Trade_M15","Bi_Trade_M14",
     "Bi_Trade_M12","Bi_Trade_M11",
     "Bi_Trade_M10","Bi_Trade_M9",
     "Bi_Trade_M8")

dat<-read.csv("BACI_HS92_Y2007_V202501.csv",stringsAsFactors=F)

setwd("~/Downloads/Nationalism_Trade_Replication-main")

source("ChangeNamesUN.R")

for(j in 1:length(years)){
  
  index<-which(all_pairs$Year==years[j])
  
  all_pairs[index,i1[j]]<-NA
  all_pairs[index,i2[j]]<-NA
  all_pairs[index,b[j]]<-NA
  
  
  for(i in index){
    p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
    if(nrow(p)>0) all_pairs[i,i2[j]]<-sum(p$v)/1000
    p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
    if(nrow(p)>0) all_pairs[i,i1[j]]<-sum(p$v)/1000
    
  }
  
  all_pairs[index,b[j]]<-all_pairs[index,i1[j]]+all_pairs[index,i2[j]]
  
  print(years[j])
  
}

rm(dat)





setwd("~/Downloads/BACI_HS92_V202501")

years<-c(2018, 2015, 2017, 2019, 2022, 2016, 2021)

years<-c(2022, 2021, 2019, 2018, 2017, 2016, 2015)

i1<-c("Imports1_Pre14","Imports1_Pre13",
      "Imports1_Pre11","Imports1_Pre10",
      "Imports1_Pre9","Imports1_Pre8",
      "Imports1_Pre7")
i2<-c("Imports2_Pre14","Imports2_Pre13",
      "Imports2_Pre11","Imports2_Pre10",
      "Imports2_Pre9","Imports2_Pre8",
      "Imports2_Pre7")
b<-c("Bi_Trade_M14","Bi_Trade_M13",
     "Bi_Trade_M11","Bi_Trade_M10",
     "Bi_Trade_M9","Bi_Trade_M8",
     "Bi_Trade_M7")

dat<-read.csv("BACI_HS92_Y2008_V202501.csv",stringsAsFactors=F)

setwd("~/Downloads/Nationalism_Trade_Replication-main")

source("ChangeNamesUN.R")

for(j in 1:length(years)){
  
  index<-which(all_pairs$Year==years[j])
  
  all_pairs[index,i1[j]]<-NA
  all_pairs[index,i2[j]]<-NA
  all_pairs[index,b[j]]<-NA
  
  
  for(i in index){
    p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
    if(nrow(p)>0) all_pairs[i,i2[j]]<-sum(p$v)/1000
    p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
    if(nrow(p)>0) all_pairs[i,i1[j]]<-sum(p$v)/1000
    
  }
  
  all_pairs[index,b[j]]<-all_pairs[index,i1[j]]+all_pairs[index,i2[j]]
  
  print(years[j])
  
}

rm(dat)







setwd("~/Downloads/BACI_HS92_V202501")

years<-c(2018, 2015, 2017, 2019, 2022, 2016, 2021)

years<-c(2022, 2021, 2019, 2018, 2017, 2016, 2015)

i1<-c("Imports1_Pre13","Imports1_Pre12",
      "Imports1_Pre10","Imports1_Pre9",
      "Imports1_Pre8","Imports1_Pre7",
      "Imports1_Pre6")
i2<-c("Imports2_Pre13","Imports2_Pre12",
      "Imports2_Pre10","Imports2_Pre9",
      "Imports2_Pre8","Imports2_Pre7",
      "Imports2_Pre6")
b<-c("Bi_Trade_M13","Bi_Trade_M12",
     "Bi_Trade_M10","Bi_Trade_M9",
     "Bi_Trade_M8","Bi_Trade_M7",
     "Bi_Trade_M6")

dat<-read.csv("BACI_HS92_Y2009_V202501.csv",stringsAsFactors=F)

setwd("~/Downloads/Nationalism_Trade_Replication-main")

source("ChangeNamesUN.R")

for(j in 1:length(years)){
  
  index<-which(all_pairs$Year==years[j])
  
  all_pairs[index,i1[j]]<-NA
  all_pairs[index,i2[j]]<-NA
  all_pairs[index,b[j]]<-NA
  
  
  for(i in index){
    p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
    if(nrow(p)>0) all_pairs[i,i2[j]]<-sum(p$v)/1000
    p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
    if(nrow(p)>0) all_pairs[i,i1[j]]<-sum(p$v)/1000
    
  }
  
  all_pairs[index,b[j]]<-all_pairs[index,i1[j]]+all_pairs[index,i2[j]]
  
  print(years[j])
  
}

rm(dat)





setwd("~/Downloads/BACI_HS92_V202501")

years<-c(2018, 2015, 2017, 2019, 2022, 2016, 2021)

years<-c(2022, 2021, 2019, 2018, 2017, 2016, 2015)

i1<-c("Imports1_Pre12","Imports1_Pre11",
      "Imports1_Pre9","Imports1_Pre8",
      "Imports1_Pre7","Imports1_Pre6",
      "Imports1_Pre5")
i2<-c("Imports2_Pre12","Imports2_Pre11",
      "Imports2_Pre9","Imports2_Pre8",
      "Imports2_Pre7","Imports2_Pre6",
      "Imports2_Pre5")
b<-c("Bi_Trade_M12","Bi_Trade_M11",
     "Bi_Trade_M9","Bi_Trade_M8",
     "Bi_Trade_M7","Bi_Trade_M6",
     "Bi_Trade_M5")

dat<-read.csv("BACI_HS92_Y2010_V202501.csv",stringsAsFactors=F)

setwd("~/Downloads/Nationalism_Trade_Replication-main")

source("ChangeNamesUN.R")

for(j in 1:length(years)){
  
  index<-which(all_pairs$Year==years[j])
  
  all_pairs[index,i1[j]]<-NA
  all_pairs[index,i2[j]]<-NA
  all_pairs[index,b[j]]<-NA
  
  
  for(i in index){
    p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
    if(nrow(p)>0) all_pairs[i,i2[j]]<-sum(p$v)/1000
    p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
    if(nrow(p)>0) all_pairs[i,i1[j]]<-sum(p$v)/1000
    
  }
  
  all_pairs[index,b[j]]<-all_pairs[index,i1[j]]+all_pairs[index,i2[j]]
  
  print(years[j])
  
}

rm(dat)







setwd("~/Downloads/BACI_HS92_V202501")

years<-c(2018, 2015, 2017, 2019, 2022, 2016, 2021)

years<-c(2022, 2021, 2019, 2018, 2017, 2016, 2015)

i1<-c("Imports1_Pre12","Imports1_Pre11",
      "Imports1_Pre9","Imports1_Pre8",
      "Imports1_Pre7","Imports1_Pre6",
      "Imports1_Pre5")
i2<-c("Imports2_Pre12","Imports2_Pre11",
      "Imports2_Pre9","Imports2_Pre8",
      "Imports2_Pre7","Imports2_Pre6",
      "Imports2_Pre5")
b<-c("Bi_Trade_M12","Bi_Trade_M11",
     "Bi_Trade_M9","Bi_Trade_M8",
     "Bi_Trade_M7","Bi_Trade_M6",
     "Bi_Trade_M5")

dat<-read.csv("BACI_HS92_Y2010_V202501.csv",stringsAsFactors=F)

setwd("~/Downloads/Nationalism_Trade_Replication-main")

source("ChangeNamesUN.R")

for(j in 1:length(years)){
  
  index<-which(all_pairs$Year==years[j])
  
  all_pairs[index,i1[j]]<-NA
  all_pairs[index,i2[j]]<-NA
  all_pairs[index,b[j]]<-NA
  
  
  for(i in index){
    p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
    if(nrow(p)>0) all_pairs[i,i2[j]]<-sum(p$v)/1000
    p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
    if(nrow(p)>0) all_pairs[i,i1[j]]<-sum(p$v)/1000
    
  }
  
  all_pairs[index,b[j]]<-all_pairs[index,i1[j]]+all_pairs[index,i2[j]]
  
  print(years[j])
  
}

rm(dat)








setwd("~/Downloads/BACI_HS92_V202501")

years<-c(2018, 2015, 2017, 2019, 2022, 2016, 2021)

years<-c(2022, 2021, 2019, 2018, 2017, 2016, 2015)

i1<-c("Imports1_Pre11","Imports1_Pre10",
      "Imports1_Pre8","Imports1_Pre7",
      "Imports1_Pre6","Imports1_Pre5",
      "Imports1_Pre4")
i2<-c("Imports2_Pre11","Imports2_Pre10",
      "Imports2_Pre8","Imports2_Pre7",
      "Imports2_Pre6","Imports2_Pre5",
      "Imports2_Pre4")
b<-c("Bi_Trade_M11","Bi_Trade_M10",
     "Bi_Trade_M8","Bi_Trade_M7",
     "Bi_Trade_M6","Bi_Trade_M5",
     "Bi_Trade_M4")

dat<-read.csv("BACI_HS92_Y2011_V202501.csv",stringsAsFactors=F)

setwd("~/Downloads/Nationalism_Trade_Replication-main")

source("ChangeNamesUN.R")

for(j in 1:length(years)){
  
  index<-which(all_pairs$Year==years[j])
  
  all_pairs[index,i1[j]]<-NA
  all_pairs[index,i2[j]]<-NA
  all_pairs[index,b[j]]<-NA
  
  
  for(i in index){
    p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
    if(nrow(p)>0) all_pairs[i,i2[j]]<-sum(p$v)/1000
    p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
    if(nrow(p)>0) all_pairs[i,i1[j]]<-sum(p$v)/1000
    
  }
  
  all_pairs[index,b[j]]<-all_pairs[index,i1[j]]+all_pairs[index,i2[j]]
  
  print(years[j])
  
}

rm(dat)










setwd("~/Downloads/BACI_HS92_V202501")

years<-c(2018, 2015, 2017, 2019, 2022, 2016, 2021)

years<-c(2022, 2021, 2019, 2018, 2017, 2016, 2015)

i1<-c("Imports1_Pre10","Imports1_Pre9",
      "Imports1_Pre7","Imports1_Pre6",
      "Imports1_Pre5","Imports1_Pre4",
      "Imports1_Pre3")
i2<-c("Imports2_Pre10","Imports2_Pre9",
      "Imports2_Pre7","Imports2_Pre6",
      "Imports2_Pre5","Imports2_Pre4",
      "Imports2_Pre3")
b<-c("Bi_Trade_M10","Bi_Trade_M9",
     "Bi_Trade_M7","Bi_Trade_M6",
     "Bi_Trade_M5","Bi_Trade_M4",
     "Bi_Trade_M3")

dat<-read.csv("BACI_HS92_Y2012_V202501.csv",stringsAsFactors=F)

setwd("~/Downloads/Nationalism_Trade_Replication-main")

source("ChangeNamesUN.R")

for(j in 1:length(years)){
  
  index<-which(all_pairs$Year==years[j])
  
  all_pairs[index,i1[j]]<-NA
  all_pairs[index,i2[j]]<-NA
  all_pairs[index,b[j]]<-NA
  
  
  for(i in index){
    p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
    if(nrow(p)>0) all_pairs[i,i2[j]]<-sum(p$v)/1000
    p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
    if(nrow(p)>0) all_pairs[i,i1[j]]<-sum(p$v)/1000
    
  }
  
  all_pairs[index,b[j]]<-all_pairs[index,i1[j]]+all_pairs[index,i2[j]]
  
  print(years[j])
  
}

rm(dat)










setwd("~/Downloads/BACI_HS92_V202501")

years<-c(2018, 2015, 2017, 2019, 2022, 2016, 2021)

years<-c(2022, 2021, 2019, 2018, 2017, 2016, 2015)

i1<-c("Imports1_Pre9","Imports1_Pre8",
      "Imports1_Pre6","Imports1_Pre5",
      "Imports1_Pre4","Imports1_Pre3",
      "Imports1_Pre2")
i2<-c("Imports2_Pre9","Imports2_Pre8",
      "Imports2_Pre6","Imports2_Pre5",
      "Imports2_Pre4","Imports2_Pre3",
      "Imports2_Pre2")
b<-c("Bi_Trade_M9","Bi_Trade_M8",
     "Bi_Trade_M6","Bi_Trade_M5",
     "Bi_Trade_M4","Bi_Trade_M3",
     "Bi_Trade_M2")

dat<-read.csv("BACI_HS92_Y2013_V202501.csv",stringsAsFactors=F)

setwd("~/Downloads/Nationalism_Trade_Replication-main")

source("ChangeNamesUN.R")

for(j in 1:length(years)){
  
  index<-which(all_pairs$Year==years[j])
  
  all_pairs[index,i1[j]]<-NA
  all_pairs[index,i2[j]]<-NA
  all_pairs[index,b[j]]<-NA
  
  
  for(i in index){
    p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
    if(nrow(p)>0) all_pairs[i,i2[j]]<-sum(p$v)/1000
    p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
    if(nrow(p)>0) all_pairs[i,i1[j]]<-sum(p$v)/1000
    
  }
  
  all_pairs[index,b[j]]<-all_pairs[index,i1[j]]+all_pairs[index,i2[j]]
  
  print(years[j])
  
}

rm(dat)








setwd("~/Downloads/BACI_HS92_V202501")

years<-c(2018, 2015, 2017, 2019, 2022, 2016, 2021)

years<-c(2022, 2021, 2019, 2018, 2017, 2016, 2015)

i1<-c("Imports1_Pre8","Imports1_Pre7",
      "Imports1_Pre5","Imports1_Pre4",
      "Imports1_Pre3","Imports1_Pre2",
      "Imports1_Pre")
i2<-c("Imports2_Pre8","Imports2_Pre7",
      "Imports2_Pre5","Imports2_Pre4",
      "Imports2_Pre3","Imports2_Pre2",
      "Imports2_Pre")
b<-c("Bi_Trade_M8","Bi_Trade_M7",
     "Bi_Trade_M5","Bi_Trade_M4",
     "Bi_Trade_M3","Bi_Trade_M2",
     "Bi_Trade_Pre")

dat<-read.csv("BACI_HS92_Y2014_V202501.csv",stringsAsFactors=F)

setwd("~/Downloads/Nationalism_Trade_Replication-main")

source("ChangeNamesUN.R")

for(j in 1:length(years)){
  
  index<-which(all_pairs$Year==years[j])
  
  all_pairs[index,i1[j]]<-NA
  all_pairs[index,i2[j]]<-NA
  all_pairs[index,b[j]]<-NA
  
  
  for(i in index){
    p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
    if(nrow(p)>0) all_pairs[i,i2[j]]<-sum(p$v)/1000
    p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
    if(nrow(p)>0) all_pairs[i,i1[j]]<-sum(p$v)/1000
    
  }
  
  all_pairs[index,b[j]]<-all_pairs[index,i1[j]]+all_pairs[index,i2[j]]
  
  print(years[j])
  
}

rm(dat)






setwd("~/Downloads/BACI_HS92_V202501")

years<-c(2018, 2015, 2017, 2019, 2022, 2016, 2021)

years<-c(2022, 2021, 2019, 2018, 2017, 2016, 2015)

i1<-c("Imports1_Pre7","Imports1_Pre6",
      "Imports1_Pre4","Imports1_Pre3",
      "Imports1_Pre2","Imports1_Pre",
      "Imports1")
i2<-c("Imports2_Pre7","Imports2_Pre6",
      "Imports2_Pre4","Imports2_Pre3",
      "Imports2_Pre2","Imports2_Pre",
      "Imports2")
b<-c("Bi_Trade_M7","Bi_Trade_M6",
     "Bi_Trade_M4","Bi_Trade_M3",
     "Bi_Trade_M2","Bi_Trade_Pre",
     "Bi_Trade_Post")

dat<-read.csv("BACI_HS92_Y2015_V202501.csv",stringsAsFactors=F)

setwd("~/Downloads/Nationalism_Trade_Replication-main")

source("ChangeNamesUN.R")

for(j in 1:length(years)){
  
  index<-which(all_pairs$Year==years[j])
  
  all_pairs[index,i1[j]]<-NA
  all_pairs[index,i2[j]]<-NA
  all_pairs[index,b[j]]<-NA
  
  
  for(i in index){
    p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
    if(nrow(p)>0) all_pairs[i,i2[j]]<-sum(p$v)/1000
    p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
    if(nrow(p)>0) all_pairs[i,i1[j]]<-sum(p$v)/1000
    
  }
  
  all_pairs[index,b[j]]<-all_pairs[index,i1[j]]+all_pairs[index,i2[j]]
  
  print(years[j])
  
}

rm(dat)





setwd("~/Downloads/BACI_HS92_V202501")

years<-c(2018, 2015, 2017, 2019, 2022, 2016, 2021)

years<-c(2022, 2021, 2019, 2018, 2017, 2016, 2015)

i1<-c("Imports1_Pre6","Imports1_Pre5",
      "Imports1_Pre3","Imports1_Pre2",
      "Imports1_Pre","Imports1",
      "Imports1_Post1")
i2<-c("Imports2_Pre6","Imports2_Pre5",
      "Imports2_Pre3","Imports2_Pre2",
      "Imports2_Pre","Imports2",
      "Imports2_Post1")
b<-c("Bi_Trade_M6","Bi_Trade_M5",
     "Bi_Trade_M3","Bi_Trade_M2",
     "Bi_Trade_Pre","Bi_Trade_Post",
     "Bi_Trade_P1")

dat<-read.csv("BACI_HS92_Y2016_V202501.csv",stringsAsFactors=F)

setwd("~/Downloads/Nationalism_Trade_Replication-main")

source("ChangeNamesUN.R")

for(j in 1:length(years)){
  
  index<-which(all_pairs$Year==years[j])
  
  all_pairs[index,i1[j]]<-NA
  all_pairs[index,i2[j]]<-NA
  all_pairs[index,b[j]]<-NA
  
  
  for(i in index){
    p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
    if(nrow(p)>0) all_pairs[i,i2[j]]<-sum(p$v)/1000
    p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
    if(nrow(p)>0) all_pairs[i,i1[j]]<-sum(p$v)/1000
    
  }
  
  all_pairs[index,b[j]]<-all_pairs[index,i1[j]]+all_pairs[index,i2[j]]
  
  print(years[j])
  
}

rm(dat)






setwd("~/Downloads/BACI_HS92_V202501")

years<-c(2018, 2015, 2017, 2019, 2022, 2016, 2021)

years<-c(2022, 2021, 2019, 2018, 2017, 2016, 2015)

i1<-c("Imports1_Pre5","Imports1_Pre4",
      "Imports1_Pre2","Imports1_Pre",
      "Imports1","Imports1_Post1",
      "Imports1_Post2")
i2<-c("Imports2_Pre5","Imports2_Pre4",
      "Imports2_Pre2","Imports2_Pre",
      "Imports2","Imports2_Post1",
      "Imports2_Post2")
b<-c("Bi_Trade_M5","Bi_Trade_M4",
     "Bi_Trade_M2","Bi_Trade_Pre",
     "Bi_Trade_Post","Bi_Trade_P1",
     "Bi_Trade_P2")

dat<-read.csv("BACI_HS92_Y2017_V202501.csv",stringsAsFactors=F)

setwd("~/Downloads/Nationalism_Trade_Replication-main")

source("ChangeNamesUN.R")

for(j in 1:length(years)){
  
  index<-which(all_pairs$Year==years[j])
  
  all_pairs[index,i1[j]]<-NA
  all_pairs[index,i2[j]]<-NA
  all_pairs[index,b[j]]<-NA
  
  
  for(i in index){
    p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
    if(nrow(p)>0) all_pairs[i,i2[j]]<-sum(p$v)/1000
    p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
    if(nrow(p)>0) all_pairs[i,i1[j]]<-sum(p$v)/1000
    
  }
  
  all_pairs[index,b[j]]<-all_pairs[index,i1[j]]+all_pairs[index,i2[j]]
  
  print(years[j])
  
}

rm(dat)







setwd("~/Downloads/BACI_HS92_V202501")

years<-c(2018, 2015, 2017, 2019, 2022, 2016, 2021)

years<-c(2022, 2021, 2019, 2018, 2017, 2016, 2015)

i1<-c("Imports1_Pre4","Imports1_Pre3",
      "Imports1_Pre","Imports1",
      "Imports1_Post1","Imports1_Post2",
      "Imports1_Post3")
i2<-c("Imports2_Pre4","Imports2_Pre3",
      "Imports2_Pre","Imports2",
      "Imports2_Post1","Imports2_Post2",
      "Imports2_Post3")
b<-c("Bi_Trade_M4","Bi_Trade_M3",
     "Bi_Trade_Pre","Bi_Trade_Post",
     "Bi_Trade_P1","Bi_Trade_P2",
     "Bi_Trade_P3")

dat<-read.csv("BACI_HS92_Y2018_V202501.csv",stringsAsFactors=F)

setwd("~/Downloads/Nationalism_Trade_Replication-main")

source("ChangeNamesUN.R")

for(j in 1:length(years)){
  
  index<-which(all_pairs$Year==years[j])
  
  all_pairs[index,i1[j]]<-NA
  all_pairs[index,i2[j]]<-NA
  all_pairs[index,b[j]]<-NA
  
  
  for(i in index){
    p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
    if(nrow(p)>0) all_pairs[i,i2[j]]<-sum(p$v)/1000
    p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
    if(nrow(p)>0) all_pairs[i,i1[j]]<-sum(p$v)/1000
    
  }
  
  all_pairs[index,b[j]]<-all_pairs[index,i1[j]]+all_pairs[index,i2[j]]
  
  print(years[j])
  
}

rm(dat)








setwd("~/Downloads/BACI_HS92_V202501")

years<-c(2018, 2015, 2017, 2019, 2022, 2016, 2021)

years<-c(2022, 2021, 2019, 2018, 2017, 2016, 2015)

i1<-c("Imports1_Pre3","Imports1_Pre2",
      "Imports1","Imports1_Post1",
      "Imports1_Post2","Imports1_Post3",
      "Imports1_Post4")
i2<-c("Imports2_Pre3","Imports2_Pre2",
      "Imports2","Imports2_Post1",
      "Imports2_Post2","Imports2_Post3",
      "Imports2_Post4")
b<-c("Bi_Trade_M3","Bi_Trade_M2",
     "Bi_Trade_Post","Bi_Trade_P1",
     "Bi_Trade_P2","Bi_Trade_P3",
     "Bi_Trade_P4")

dat<-read.csv("BACI_HS92_Y2019_V202501.csv",stringsAsFactors=F)

setwd("~/Downloads/Nationalism_Trade_Replication-main")

source("ChangeNamesUN.R")

for(j in 1:length(years)){
  
  index<-which(all_pairs$Year==years[j])
  
  all_pairs[index,i1[j]]<-NA
  all_pairs[index,i2[j]]<-NA
  all_pairs[index,b[j]]<-NA
  
  
  for(i in index){
    p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
    if(nrow(p)>0) all_pairs[i,i2[j]]<-sum(p$v)/1000
    p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
    if(nrow(p)>0) all_pairs[i,i1[j]]<-sum(p$v)/1000
    
  }
  
  all_pairs[index,b[j]]<-all_pairs[index,i1[j]]+all_pairs[index,i2[j]]
  
  print(years[j])
  
}

rm(dat)











setwd("~/Downloads/BACI_HS92_V202501")

years<-c(2018, 2015, 2017, 2019, 2022, 2016, 2021)

years<-c(2022, 2021, 2019, 2018, 2017, 2016, 2015)

i1<-c("Imports1_Pre2","Imports1_Pre",
      "Imports1_Post1","Imports1_Post2",
      "Imports1_Post3","Imports1_Post4",
      "Imports1_Post5")
i2<-c("Imports2_Pre2","Imports2_Pre",
      "Imports2_Post1","Imports2_Post2",
      "Imports2_Post3","Imports2_Post4",
      "Imports2_Post5")
b<-c("Bi_Trade_M2","Bi_Trade_Pre",
     "Bi_Trade_P1","Bi_Trade_P2",
     "Bi_Trade_P3","Bi_Trade_P4",
     "Bi_Trade_P5")

dat<-read.csv("BACI_HS92_Y2020_V202501.csv",stringsAsFactors=F)

setwd("~/Downloads/Nationalism_Trade_Replication-main")

source("ChangeNamesUN.R")

for(j in 1:length(years)){
  
  index<-which(all_pairs$Year==years[j])
  
  all_pairs[index,i1[j]]<-NA
  all_pairs[index,i2[j]]<-NA
  all_pairs[index,b[j]]<-NA
  
  
  for(i in index){
    p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
    if(nrow(p)>0) all_pairs[i,i2[j]]<-sum(p$v)/1000
    p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
    if(nrow(p)>0) all_pairs[i,i1[j]]<-sum(p$v)/1000
    
  }
  
  all_pairs[index,b[j]]<-all_pairs[index,i1[j]]+all_pairs[index,i2[j]]
  
  print(years[j])
  
}

rm(dat)









setwd("~/Downloads/BACI_HS92_V202501")

years<-c(2018, 2015, 2017, 2019, 2022, 2016, 2021)

years<-c(2022, 2021, 2019, 2018, 2017, 2016, 2015)

i1<-c("Imports1_Pre","Imports1",
      "Imports1_Post2","Imports1_Post3",
      "Imports1_Post4","Imports1_Post5",
      "Imports1_Post6")
i2<-c("Imports2_Pre","Imports2",
      "Imports2_Post2","Imports2_Post3",
      "Imports2_Post4","Imports2_Post5",
      "Imports2_Post6")
b<-c("Bi_Trade_Pre","Bi_Trade_Post",
     "Bi_Trade_P2","Bi_Trade_P3",
     "Bi_Trade_P4","Bi_Trade_P5",
     "Bi_Trade_P6")

dat<-read.csv("BACI_HS92_Y2021_V202501.csv",stringsAsFactors=F)

setwd("~/Downloads/Nationalism_Trade_Replication-main")

source("ChangeNamesUN.R")

for(j in 1:length(years)){
  
  index<-which(all_pairs$Year==years[j])
  
  all_pairs[index,i1[j]]<-NA
  all_pairs[index,i2[j]]<-NA
  all_pairs[index,b[j]]<-NA
  
  
  for(i in index){
    p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
    if(nrow(p)>0) all_pairs[i,i2[j]]<-sum(p$v)/1000
    p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
    if(nrow(p)>0) all_pairs[i,i1[j]]<-sum(p$v)/1000
    
  }
  
  all_pairs[index,b[j]]<-all_pairs[index,i1[j]]+all_pairs[index,i2[j]]
  
  print(years[j])
  
}

rm(dat)






setwd("~/Downloads/BACI_HS92_V202501")

years<-c(2018, 2015, 2017, 2019, 2022, 2016, 2021)

years<-c(2022, 2021, 2019, 2018, 2017, 2016, 2015)

i1<-c("Imports1","Imports1_Post1",
      "Imports1_Post3","Imports1_Post4",
      "Imports1_Post5","Imports1_Post6",
      "Imports1_Post7")
i2<-c("Imports2","Imports2_Post1",
      "Imports2_Post3","Imports2_Post4",
      "Imports2_Post5","Imports2_Post6",
      "Imports2_Post7")
b<-c("Bi_Trade_Post","Bi_Trade_P1",
     "Bi_Trade_P3","Bi_Trade_P4",
     "Bi_Trade_P5","Bi_Trade_P6",
     "Bi_Trade_P7")

dat<-read.csv("BACI_HS92_Y2022_V202501.csv",stringsAsFactors=F)

setwd("~/Downloads/Nationalism_Trade_Replication-main")

source("ChangeNamesUN.R")

for(j in 1:length(years)){
  
  index<-which(all_pairs$Year==years[j])
  
  all_pairs[index,i1[j]]<-NA
  all_pairs[index,i2[j]]<-NA
  all_pairs[index,b[j]]<-NA
  
  
  for(i in index){
    p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
    if(nrow(p)>0) all_pairs[i,i2[j]]<-sum(p$v)/1000
    p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
    if(nrow(p)>0) all_pairs[i,i1[j]]<-sum(p$v)/1000
    
  }
  
  all_pairs[index,b[j]]<-all_pairs[index,i1[j]]+all_pairs[index,i2[j]]
  
  print(years[j])
  
}

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
dat$iso3_o[which(dat$country_id_o=="COD")]<-"DRC"
dat$iso3_o[which(dat$country_id_o=="YUG")]<-"Yugoslavia"
dat$iso3_o[which(dat$country_id_o=="SRB"&dat$year==2018)]<-"Yugoslavia"


dat$iso3_d[which(dat$country_id_d=="DEU.1"&dat$year<1991)]<-"West Germany"
dat$iso3_d[which(dat$country_id_d=="DDR")]<-"East Germany"
dat$iso3_d[which(dat$country_id_d=="CZE")]<-"Czech Republic"
dat$iso3_d[which(dat$country_id_d=="CSK")]<-"Czechoslovakia"
dat$iso3_d[which(dat$country_id_d=="SUN")]<-"Soviet Union"
dat$iso3_d[which(dat$country_id_d=="PRK")]<-"North Korea"
dat$iso3_d[which(dat$country_id_d=="COD")]<-"DRC"
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






all_pairs<-all_pairs[-1,]

all_pairs$Could_Play_GS<-NA
n<-nrow(all_pairs)-sum(all_pairs$Event=="Europe"&all_pairs$Year<1980)
for(i in 1:n){
  group<-real_groups[real_groups$Year==all_pairs$Year[i]&
                     real_groups$Event==all_pairs$Event[i],]
  pot1<-which(group==as.character(all_pairs$Country1[i]),arr.ind=T)[2]	
  pot2<-which(group==as.character(all_pairs$Country2[i]),arr.ind=T)[2]		
  if(pot1==pot2){all_pairs$Could_Play_GS[i]<-0}
  if(pot1!=pot2){all_pairs$Could_Play_GS[i]<-1}
}

all_pairs$Could_Play_GS[all_pairs$Event=="Europe"&all_pairs$Year<1980]<-0

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


rm(dat)
rm(covs1)
rm(covs2)



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

setwd("~/Downloads/Nationalism_Trade_Replication-main")

write.csv(all_pairs,"All_Pairs_Regional_9_25.csv",row.names = F)

all_pairs<-read.csv("All_Pairs_Regional_9_25.csv",stringsAsFactors = F)

real_data_full_all<-all_pairs[all_pairs$Group_Stage==1&all_pairs$Event=="Europe",]

real_data_full<-real_data_full_all[unlist(is.na(real_data_full_all$Bi_Trade_Pre)==FALSE&is.na(real_data_full_all$Bi_Trade_Post)==FALSE),]

real_data_full_all<-all_pairs[all_pairs$Group_Stage==1&all_pairs$Event=="Europe",]

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





write.csv(real_data_full,"RealDataFull_Euro_Sept25.csv",row.names=F)

write.csv(real_data,"RealData_Euro_Sept25.csv",row.names=F)









real_data_full_all<-all_pairs[all_pairs$Group_Stage==1&all_pairs$Event=="Africa",]

real_data_full<-real_data_full_all[unlist(is.na(real_data_full_all$Bi_Trade_Pre)==FALSE&is.na(real_data_full_all$Bi_Trade_Post)==FALSE),]

real_data_full_all<-all_pairs[all_pairs$Group_Stage==1&all_pairs$Event=="Africa",]

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





write.csv(real_data_full,"RealDataFull_Africa_Sept25.csv",row.names=F)

write.csv(real_data,"RealData_Africa_Sept25.csv",row.names=F)



