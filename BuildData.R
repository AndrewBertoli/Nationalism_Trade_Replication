# We will start by loading the following package.
require("readxl")

# We will next set the working directory to the folder with the replication
# materials from the "Nationalism_Trade_Replication" repository from Github
setwd("~/Downloads/Nationalism_Trade_Replication-main")

# We can read in the file with the World Cup groups.
real_groups<-read.csv("WC_Groups.csv",stringsAsFactors=FALSE,sep=";")

# To make the data merging easier, we will convert the Soviet Union
# to Russia and both Serbia and Serbia-Montenegro to Yugoslavia (and 
# change them back later)

real_groups$Country1[real_groups$Country1=="Soviet Union"]<-"Russia"
real_groups$Country2[real_groups$Country2=="Soviet Union"]<-"Russia"

real_groups$Country4[real_groups$Country4=="Serbia"]<-"Yugoslavia"
real_groups$Country2[real_groups$Country2=="Serbia-Montenegro"]<-"Yugoslavia"
real_groups$Country4[real_groups$Country4=="Serbia-Montenegro"]<-"Yugoslavia"


# We will now subset to the World Cup groups for each year.
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

# Next, we can put the groups for each year into a list.
groups<-list(groups1930,groups1934,groups1938,groups1950,
             groups1954,groups1958,groups1962,groups1966,
             groups1970,groups1974,groups1978,groups1982,
             groups1986,groups1990,groups1994,groups1998,
             groups2002,groups2006,groups2010,groups2014,
             groups2018)

# We will also make a vector with corresponding years for the list.
years<-c(1930,1934,1938,1950,1954,1958,1962,1966,
         1970,1974,1978,1982,1986,1990,1994,1998,
         2002,2006,2010,2014,2018)

# We will next read in a data frame that includes all the countries that went
# to the World Cup, Euro, or African Cup of Nations. The data file contains
# the country names and years, but no other information.
covariates<-read.csv("Covariates.csv",stringsAsFactors=FALSE,sep=";")

# We can next read in a data frame with information about the countries that
# played each other in the World Cup. 
games<-read.csv("WorldCupGames.csv",stringsAsFactors=F,sep=";")

# We will again convert the Soviet Union to Russia and both Serbia and 
# Serbia-Montenegro to Yugoslavia to make the data merging easier
covariates$Country[covariates$Country=="Soviet Union"]<-"Russia"
covariates$Country[covariates$Country=="Serbia"]<-"Yugoslavia"
covariates$Country[covariates$Country=="Serbia-Montenegro"]<-"Yugoslavia"

games$Country1[games$Country1=="Soviet Union"]<-"Russia"
games$Country1[games$Country1=="Serbia"]<-"Yugoslavia"
games$Country1[games$Country1=="Serbia-Montenegro"]<-"Yugoslavia"

games$Country2[games$Country2=="Soviet Union"]<-"Russia"
games$Country2[games$Country2=="Serbia"]<-"Yugoslavia"
games$Country2[games$Country2=="Serbia-Montenegro"]<-"Yugoslavia"





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

# Create a vector of countries where soccer is not the most popular sport.
countries<-c("United States", "Australia", 
             "New Zealand", "Canada", 
             "Jamaica","Japan", "Cuba", 
             "Trinidad-Tobago")

# Create a column of 1s called SoccerMostPopular
covariates$SoccerMostPopular<-1

# Switch the 1s to 0s for countries where soccer is not the most popular sport.
covariates$SoccerMostPopular[covariates$Country%in%countries]<-0

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
trade<-trade[-which(trade$year<1913),]

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
all_pairs<-matrix(0,nrow=1,ncol=4)
for(k in 1:length(groups)){
for(j in 1:length(as.matrix(groups[[k]]))){
new_pairs<-cbind(rep(as.matrix(groups[[k]])[j],length(as.matrix(groups[[k]]))-1),
				 c(as.matrix(groups[[k]])[-j]),years[k],month[k])
all_pairs<-rbind(all_pairs,new_pairs)	
}}


# Remove the rows of all_pairs where one or both of the country names are "NONE"
all_pairs<-all_pairs[-unique(c(which(all_pairs[,1]=="NONE"),
                               which(all_pairs[,2]=="NONE"))),]

# Put the country names in alphabetical order in each row.
for(i in 1:length(all_pairs[,1])){all_pairs[i,1:2]<-
  sort(all_pairs[i,1:2])}

# Remove any rows that are duplicates.
all_pairs<-unique(all_pairs,rows=TRUE)

# Turn the object all_pairs into a data frame.
all_pairs<-data.frame(all_pairs,stringsAsFactors=FALSE)

# Assign column names to all_pairs.
colnames(all_pairs)<-c("Country1","Country2","Year","Month")

# Make the year and month columns numeric.
all_pairs$Year<-as.numeric(all_pairs$Year)
all_pairs$Month<-as.numeric(all_pairs$Month)


# Turning to the data frame games, we will set the the GroupStage values 
# to 1 for pairs of countries that played in Round 1 of the 1934 and
# 1938 World Cup.
games$GroupStage[games$Year%in%c(1934,1938)&games$Round1==1]<-1

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
	
	
	
# The code below adds the knockout stage scores to all_pairs.	
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
		
# Change the 2014 row for Costa Rica and the Netherlands so that the Netherlands is Country 1
# and Costa Rica is Country 2 (the Netherlands won on penalties).
all_pairs[all_pairs$Country1=="Costa Rica"&
            all_pairs$Country2=="Netherlands"&
            all_pairs$Year==2014,c("Country1","Country2")]<-
  c("Netherlands","Costa Rica")

# Change the 1982 row for the United Kingdom and West Germany so that West Germany is 
# Country 1 and the United Kingdom is Country 2 (West Germany won on penalties).
all_pairs[all_pairs$Country1=="United Kingdom"&
            all_pairs$Country2=="West Germany"&
            all_pairs$Year==1982,c("Country1","Country2")]<-
  c("West Germany","United Kingdom")

# Change the 1982 row for Spain and the United Kingdom so that the United Kingdom is 
# Country 1 and Spain is Country 2 (the United Kingdom won on penalties).
all_pairs[all_pairs$Country1=="Spain"&
            all_pairs$Country2=="United Kingdom"&
            all_pairs$Year==1982,c("Country1","Country2")]<-
  c("United Kingdom","Spain")

# Change the 1974 row for Argentina and East Germany so that East Germany is 
# Country 1 and Argentina is Country 2 (East Germany won on penalties).
all_pairs[all_pairs$Country1=="Argentina"&
            all_pairs$Country2=="East Germany"&
            all_pairs$Year==1974,c("Country1","Country2")]<-
  c("East Germany","Argentina")

# Change the 1950 row for Spain and Uruguay so that Uruguay is Country 1
# and Spain is Country 2 (Uruguay won on penalties).
all_pairs[all_pairs$Country1=="Spain"&
            all_pairs$Country2=="Uruguay"&
            all_pairs$Year==1950,c("Country1","Country2")]<-
  c("Uruguay","Spain")



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


# Reorder the columns of all_pairs.
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

# Since the trade dataset we used ends in 2014, we will use
# another trade dataset for the 2018 cases. 
# You can download the trade data for 2018 here:
# https://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37
# The file is HS 92 of the 202401b version.

setwd("~/Downloads/BACI_HS92_V202401b")

# We set the year as 2018, because we will use this new dataset for
# the 2018 cases.
year<-2018

# We will get the row numbers for the 2018 World Cup cases.
index<-which(all_pairs$Year%in%(year))

# We will start by reading in the trade data for 2003.
dat<-read.csv("BACI_HS92_Y2003_V202401b.csv",stringsAsFactors=F)

# Reset the working directory to the folder with the replication materials
# from the repository.
setwd("~/Downloads/Nationalism_Trade_Replication-main")

# Input country names into the data set that match the country names in
# our data frame all_pairs.
source("ChangeNamesUN.R")

# Reset the import values and bilateral trade information for 
# the 2018 World Cup countries 15 years before the tournament.
all_pairs$Imports1_Pre15[index]<-NA
all_pairs$Imports2_Pre15[index]<-NA
all_pairs$Bi_Trade_M15[index]<-NA

# Fill in the import data for this year.
for(i in index){
	p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
	if(nrow(p)>0) all_pairs$Imports2_Pre15[i]<-sum(p$v)/1000
	p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
	if(nrow(p)>0) all_pairs$Imports1_Pre15[i]<-sum(p$v)/1000

}

# Fill in the level of bilateral trade for this year.
all_pairs$Bi_Trade_M15[index]<-all_pairs$Imports1_Pre15[index]+all_pairs$Imports2_Pre15[index]

# Remove the 2003 trade data.
rm(dat)


# Set the working directory to the folder with the trade data.
setwd("~/Downloads/BACI_HS92_V202401b")

# We will next read in the trade data for 2004.
dat<-read.csv("BACI_HS92_Y2004_V202401b.csv",stringsAsFactors=F)

# Reset the working directory to the folder with the replication materials
# from the repository.
setwd("~/Downloads/Nationalism_Trade_Replication-main")

# Input country names into the data set that match the country names in
# our data frame all_pairs.
source("ChangeNamesUN.R")

# Reset the import values and bilateral trade information for 
# the 2018 World Cup countries 14 years before the tournament.
all_pairs$Imports1_Pre14[index]<-NA
all_pairs$Imports2_Pre14[index]<-NA
all_pairs$Bi_Trade_M14[index]<-NA

# Fill in the import data for this year.
for(i in index){
	p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
	if(nrow(p)>0) all_pairs$Imports2_Pre14[i]<-sum(p$v)/1000
	p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
	if(nrow(p)>0) all_pairs$Imports1_Pre14[i]<-sum(p$v)/1000

}

# Fill in the level of bilateral trade for this year.
all_pairs$Bi_Trade_M14[index]<-all_pairs$Imports1_Pre14[index]+all_pairs$Imports2_Pre14[index]

# Remove the 2004 trade data.
rm(dat)



# Set the working directory to the folder with the trade data.
setwd("~/Downloads/BACI_HS92_V202401b")

# We will next read in the trade data for 2005.
dat<-read.csv("BACI_HS92_Y2005_V202401b.csv",stringsAsFactors=F)

# Reset the working directory to the folder with the replication materials
# from the repository.
setwd("~/Downloads/Nationalism_Trade_Replication-main")

# Input country names into the data set that match the country names in
# our data frame all_pairs.
source("ChangeNamesUN.R")

# Reset the import values and bilateral trade information for 
# the 2018 World Cup countries 13 years before the tournament.
all_pairs$Imports1_Pre13[index]<-NA
all_pairs$Imports2_Pre13[index]<-NA
all_pairs$Bi_Trade_M13[index]<-NA

# Fill in the import data for this year.
for(i in index){
	p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
	if(nrow(p)>0) all_pairs$Imports2_Pre13[i]<-sum(p$v)/1000
	p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
	if(nrow(p)>0) all_pairs$Imports1_Pre13[i]<-sum(p$v)/1000

}

# Fill in the level of bilateral trade for this year.
all_pairs$Bi_Trade_M13[index]<-all_pairs$Imports1_Pre13[index]+all_pairs$Imports2_Pre13[index]

# Remove the 2005 trade data.
rm(dat)




# Set the working directory to the folder with the trade data.
setwd("~/Downloads/BACI_HS92_V202401b")

# We will next read in the trade data for 2006.
dat<-read.csv("BACI_HS92_Y2006_V202401b.csv",stringsAsFactors=F)

# Reset the working directory to the folder with the replication materials
# from the repository.
setwd("~/Downloads/Nationalism_Trade_Replication-main")

# Input country names into the data set that match the country names in
# our data frame all_pairs.
source("ChangeNamesUN.R")

# Reset the import values and bilateral trade information for 
# the 2018 World Cup countries 12 years before the tournament.
all_pairs$Imports1_Pre12[index]<-NA
all_pairs$Imports2_Pre12[index]<-NA
all_pairs$Bi_Trade_M12[index]<-NA

# Fill in the import data for this year.
for(i in index){
	p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
	if(nrow(p)>0) all_pairs$Imports2_Pre12[i]<-sum(p$v)/1000
	p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
	if(nrow(p)>0) all_pairs$Imports1_Pre12[i]<-sum(p$v)/1000

}

# Fill in the level of bilateral trade for this year.
all_pairs$Bi_Trade_M12[index]<-all_pairs$Imports1_Pre12[index]+all_pairs$Imports2_Pre12[index]

# Remove the 2006 trade data.
rm(dat)



# Set the working directory to the folder with the trade data.
setwd("~/Downloads/BACI_HS92_V202401b")

# We will next read in the trade data for 2007.
dat<-read.csv("BACI_HS92_Y2007_V202401b.csv",stringsAsFactors=F)

# Reset the working directory to the folder with the replication materials
# from the repository.
setwd("~/Downloads/Nationalism_Trade_Replication-main")

# Input country names into the data set that match the country names in
# our data frame all_pairs.
source("ChangeNamesUN.R")

# Reset the import values and bilateral trade information for 
# the 2018 World Cup countries 11 years before the tournament.
all_pairs$Imports1_Pre11[index]<-NA
all_pairs$Imports2_Pre11[index]<-NA
all_pairs$Bi_Trade_M11[index]<-NA

# Fill in the import data for this year.
for(i in index){
	p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
	if(nrow(p)>0) all_pairs$Imports2_Pre11[i]<-sum(p$v)/1000
	p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
	if(nrow(p)>0) all_pairs$Imports1_Pre11[i]<-sum(p$v)/1000

}

# Fill in the level of bilateral trade for this year.
all_pairs$Bi_Trade_M11[index]<-all_pairs$Imports1_Pre11[index]+all_pairs$Imports2_Pre11[index]

# Remove the 2007 trade data.
rm(dat)



# Set the working directory to the folder with the trade data.
setwd("~/Downloads/BACI_HS92_V202401b")

# We will next read in the trade data for 2008.
dat<-read.csv("BACI_HS92_Y2008_V202401b.csv",stringsAsFactors=F)

# Reset the working directory to the folder with the replication materials
# from the repository.
setwd("~/Downloads/Nationalism_Trade_Replication-main")

# Input country names into the data set that match the country names in
# our data frame all_pairs.
source("ChangeNamesUN.R")

# Reset the import values and bilateral trade information for 
# the 2018 World Cup countries 10 years before the tournament.
all_pairs$Imports1_Pre10[index]<-NA
all_pairs$Imports2_Pre10[index]<-NA
all_pairs$Bi_Trade_M10[index]<-NA

# Fill in the import data for this year.
for(i in index){
	p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
	if(nrow(p)>0) all_pairs$Imports2_Pre10[i]<-sum(p$v)/1000
	p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
	if(nrow(p)>0) all_pairs$Imports1_Pre10[i]<-sum(p$v)/1000

}

# Fill in the level of bilateral trade for this year.
all_pairs$Bi_Trade_M10[index]<-all_pairs$Imports1_Pre10[index]+all_pairs$Imports2_Pre10[index]

# Remove the 2008 trade data.
rm(dat)





# Set the working directory to the folder with the trade data.
setwd("~/Downloads/BACI_HS92_V202401b")

# We will next read in the trade data for 2009.
dat<-read.csv("BACI_HS92_Y2009_V202401b.csv",stringsAsFactors=F)

# Reset the working directory to the folder with the replication materials
# from the repository.
setwd("~/Downloads/Nationalism_Trade_Replication-main")

# Input country names into the data set that match the country names in
# our data frame all_pairs.
source("ChangeNamesUN.R")

# Reset the import values and bilateral trade information for 
# the 2018 World Cup countries 9 years before the tournament.
all_pairs$Imports1_Pre9[index]<-NA
all_pairs$Imports2_Pre9[index]<-NA
all_pairs$Bi_Trade_M9[index]<-NA

# Fill in the import data for this year.
for(i in index){
	p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
	if(nrow(p)>0) all_pairs$Imports2_Pre9[i]<-sum(p$v)/1000
	p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
	if(nrow(p)>0) all_pairs$Imports1_Pre9[i]<-sum(p$v)/1000

}

# Fill in the level of bilateral trade for this year.
all_pairs$Bi_Trade_M9[index]<-all_pairs$Imports1_Pre9[index]+all_pairs$Imports2_Pre9[index]

# Remove the 2009 trade data.
rm(dat)




# Set the working directory to the folder with the trade data.
setwd("~/Downloads/BACI_HS92_V202401b")

# We will next read in the trade data for 2010.
dat<-read.csv("BACI_HS92_Y2010_V202401b.csv",stringsAsFactors=F)

# Reset the working directory to the folder with the replication materials
# from the repository.
setwd("~/Downloads/Nationalism_Trade_Replication-main")

# Input country names into the data set that match the country names in
# our data frame all_pairs.
source("ChangeNamesUN.R")

# Reset the import values and bilateral trade information for 
# the 2018 World Cup countries 8 years before the tournament.
all_pairs$Imports1_Pre8[index]<-NA
all_pairs$Imports2_Pre8[index]<-NA
all_pairs$Bi_Trade_M8[index]<-NA

# Fill in the import data for this year.
for(i in index){
	p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
	if(nrow(p)>0) all_pairs$Imports2_Pre8[i]<-sum(p$v)/1000
	p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
	if(nrow(p)>0) all_pairs$Imports1_Pre8[i]<-sum(p$v)/1000

}

# Fill in the level of bilateral trade for this year.
all_pairs$Bi_Trade_M8[index]<-all_pairs$Imports1_Pre8[index]+all_pairs$Imports2_Pre8[index]

# Remove the 2010 trade data.
rm(dat)





# Set the working directory to the folder with the trade data.
setwd("~/Downloads/BACI_HS92_V202401b")

# We will next read in the trade data for 2011.
dat<-read.csv("BACI_HS92_Y2011_V202401b.csv",stringsAsFactors=F)

# Reset the working directory to the folder with the replication materials
# from the repository.
setwd("~/Downloads/Nationalism_Trade_Replication-main")

# Input country names into the data set that match the country names in
# our data frame all_pairs.
source("ChangeNamesUN.R")

# Reset the import values and bilateral trade information for 
# the 2018 World Cup countries 7 years before the tournament.
all_pairs$Imports1_Pre7[index]<-NA
all_pairs$Imports2_Pre7[index]<-NA
all_pairs$Bi_Trade_M7[index]<-NA

# Fill in the import data for this year.
for(i in index){
	p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
	if(nrow(p)>0) all_pairs$Imports2_Pre7[i]<-sum(p$v)/1000
	p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
	if(nrow(p)>0) all_pairs$Imports1_Pre7[i]<-sum(p$v)/1000

}

# Fill in the level of bilateral trade for this year.
all_pairs$Bi_Trade_M7[index]<-all_pairs$Imports1_Pre7[index]+all_pairs$Imports2_Pre7[index]

# Remove the 2011 trade data.
rm(dat)



# Set the working directory to the folder with the trade data.
setwd("~/Downloads/BACI_HS92_V202401b")

# We will next read in the trade data for 2012.
dat<-read.csv("BACI_HS92_Y2012_V202401b.csv",stringsAsFactors=F)

# Reset the working directory to the folder with the replication materials
# from the repository.
setwd("~/Downloads/Nationalism_Trade_Replication-main")

# Input country names into the data set that match the country names in
# our data frame all_pairs.
source("ChangeNamesUN.R")

# Reset the import values and bilateral trade information for 
# the 2018 World Cup countries 6 years before the tournament.
all_pairs$Imports1_Pre6[index]<-NA
all_pairs$Imports2_Pre6[index]<-NA
all_pairs$Bi_Trade_M6[index]<-NA

# Fill in the import data for this year.
for(i in index){
	p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
	if(nrow(p)>0) all_pairs$Imports2_Pre6[i]<-sum(p$v)/1000
	p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
	if(nrow(p)>0) all_pairs$Imports1_Pre6[i]<-sum(p$v)/1000

}

# Fill in the level of bilateral trade for this year.
all_pairs$Bi_Trade_M6[index]<-all_pairs$Imports1_Pre6[index]+all_pairs$Imports2_Pre6[index]

# Remove the 2012 trade data.
rm(dat)




# Set the working directory to the folder with the trade data.
setwd("~/Downloads/BACI_HS92_V202401b")

# We will next read in the trade data for 2013.
dat<-read.csv("BACI_HS92_Y2013_V202401b.csv",stringsAsFactors=F)

# Reset the working directory to the folder with the replication materials
# from the repository.
setwd("~/Downloads/Nationalism_Trade_Replication-main")

# Input country names into the data set that match the country names in
# our data frame all_pairs.
source("ChangeNamesUN.R")

# Reset the import values and bilateral trade information for 
# the 2018 World Cup countries 5 years before the tournament.
all_pairs$Imports1_Pre5[index]<-NA
all_pairs$Imports2_Pre5[index]<-NA
all_pairs$Bi_Trade_M5[index]<-NA

# Fill in the import data for this year.
for(i in index){
	p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
	if(nrow(p)>0) all_pairs$Imports2_Pre5[i]<-sum(p$v)/1000
	p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
	if(nrow(p)>0) all_pairs$Imports1_Pre5[i]<-sum(p$v)/1000

}

# Fill in the level of bilateral trade for this year.
all_pairs$Bi_Trade_M5[index]<-all_pairs$Imports1_Pre5[index]+all_pairs$Imports2_Pre5[index]

# Remove the 2013 trade data.
rm(dat)







# Set the working directory to the folder with the trade data.
setwd("~/Downloads/BACI_HS92_V202401b")

# We will next read in the trade data for 2014.
dat<-read.csv("BACI_HS92_Y2014_V202401b.csv",stringsAsFactors=F)

# Reset the working directory to the folder with the replication materials
# from the repository.
setwd("~/Downloads/Nationalism_Trade_Replication-main")

# Input country names into the data set that match the country names in
# our data frame all_pairs.
source("ChangeNamesUN.R")

# Reset the import values and bilateral trade information for 
# the 2018 World Cup countries 4 years before the tournament.
all_pairs$Imports1_Pre4[index]<-NA
all_pairs$Imports2_Pre4[index]<-NA
all_pairs$Bi_Trade_M4[index]<-NA

# Fill in the import data for this year.
for(i in index){
	p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
	if(nrow(p)>0) all_pairs$Imports2_Pre4[i]<-sum(p$v)/1000
	p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
	if(nrow(p)>0) all_pairs$Imports1_Pre4[i]<-sum(p$v)/1000

}

# Fill in the level of bilateral trade for this year.
all_pairs$Bi_Trade_M4[index]<-all_pairs$Imports1_Pre4[index]+all_pairs$Imports2_Pre4[index]

# Remove the 2014 trade data.
rm(dat)





# Set the working directory to the folder with the trade data.
setwd("~/Downloads/BACI_HS92_V202401b")

# We will next read in the trade data for 2015.
dat<-read.csv("BACI_HS92_Y2015_V202401b.csv",stringsAsFactors=F)

# Reset the working directory to the folder with the replication materials
# from the repository.
setwd("~/Downloads/Nationalism_Trade_Replication-main")

# Input country names into the data set that match the country names in
# our data frame all_pairs.
source("ChangeNamesUN.R")

# Reset the import values and bilateral trade information for 
# the 2018 World Cup countries 3 years before the tournament.
all_pairs$Imports1_Pre3[index]<-NA
all_pairs$Imports2_Pre3[index]<-NA
all_pairs$Bi_Trade_M3[index]<-NA

# Fill in the import data for this year.
for(i in index){
	p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
	if(nrow(p)>0) all_pairs$Imports2_Pre3[i]<-sum(p$v)/1000
	p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
	if(nrow(p)>0) all_pairs$Imports1_Pre3[i]<-sum(p$v)/1000

}

# Fill in the level of bilateral trade for this year.
all_pairs$Bi_Trade_M3[index]<-all_pairs$Imports1_Pre3[index]+all_pairs$Imports2_Pre3[index]

# Remove the 2015 trade data.
rm(dat)



# Set the working directory to the folder with the trade data.
setwd("~/Downloads/BACI_HS92_V202401b")

# We will next read in the trade data for 2016.
dat<-read.csv("BACI_HS92_Y2016_V202401b.csv",stringsAsFactors=F)

# Reset the working directory to the folder with the replication materials
# from the repository.
setwd("~/Downloads/Nationalism_Trade_Replication-main")

# Input country names into the data set that match the country names in
# our data frame all_pairs.
source("ChangeNamesUN.R")

# Reset the import values and bilateral trade information for 
# the 2018 World Cup countries 2 years before the tournament.
all_pairs$Imports1_Pre2[index]<-NA
all_pairs$Imports2_Pre2[index]<-NA
all_pairs$Bi_Trade_M2[index]<-NA

# Fill in the import data for this year.
for(i in index){
	p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
	if(nrow(p)>0) all_pairs$Imports2_Pre2[i]<-sum(p$v)/1000
	p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
	if(nrow(p)>0) all_pairs$Imports1_Pre2[i]<-sum(p$v)/1000

}

# Fill in the level of bilateral trade for this year.
all_pairs$Bi_Trade_M2[index]<-all_pairs$Imports1_Pre2[index]+all_pairs$Imports2_Pre2[index]

# Remove the 2016 trade data.
rm(dat)



# Set the working directory to the folder with the trade data.
setwd("~/Downloads/BACI_HS92_V202401b")

# We will next read in the trade data for 2017.
dat<-read.csv("BACI_HS92_Y2017_V202401b.csv",stringsAsFactors=F)

# Reset the working directory to the folder with the replication materials
# from the repository.
setwd("~/Downloads/Nationalism_Trade_Replication-main")

# Input country names into the data set that match the country names in
# our data frame all_pairs.
source("ChangeNamesUN.R")

# Reset the import values and bilateral trade information for 
# the 2018 World Cup countries 1 year before the tournament.
all_pairs$Imports1_Pre[index]<-NA
all_pairs$Imports2_Pre[index]<-NA
all_pairs$Bi_Trade_Pre[index]<-NA

# Fill in the import data for this year.
for(i in index){
	p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
	if(nrow(p)>0) all_pairs$Imports2_Pre[i]<-sum(p$v)/1000
	p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
	if(nrow(p)>0) all_pairs$Imports1_Pre[i]<-sum(p$v)/1000

}

# Fill in the level of bilateral trade for this year.
all_pairs$Bi_Trade_Pre[index]<-all_pairs$Imports1_Pre[index]+all_pairs$Imports2_Pre[index]

# Remove the 2017 trade data.
rm(dat)



# Set the working directory to the folder with the trade data.
setwd("~/Downloads/BACI_HS92_V202401b")

# We will next read in the trade data for 2018.
dat<-read.csv("BACI_HS92_Y2018_V202401b.csv",stringsAsFactors=F)

# Reset the working directory to the folder with the replication materials
# from the repository.
setwd("~/Downloads/Nationalism_Trade_Replication-main")

# Input country names into the data set that match the country names in
# our data frame all_pairs.
source("ChangeNamesUN.R")

# Reset the import values and bilateral trade information for 
# the 2018 World Cup countries the year of the tournament.
all_pairs$Imports1[index]<-NA
all_pairs$Imports2[index]<-NA
all_pairs$Bi_Trade_Post[index]<-NA

# Fill in the import data for this year.
for(i in index){
	p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
	if(nrow(p)>0) all_pairs$Imports2[i]<-sum(p$v)/1000
	p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
	if(nrow(p)>0) all_pairs$Imports1[i]<-sum(p$v)/1000

}

# Fill in the level of bilateral trade for this year.
all_pairs$Bi_Trade_Post[index]<-all_pairs$Imports1[index]+all_pairs$Imports2[index]

# Remove the 2018 trade data.
rm(dat)



# Set the working directory to the folder with the trade data.
setwd("~/Downloads/BACI_HS92_V202401b")

# We will next read in the trade data for 2019.
dat<-read.csv("BACI_HS92_Y2019_V202401b.csv",stringsAsFactors=F)

# Reset the working directory to the folder with the replication materials
# from the repository.
setwd("~/Downloads/Nationalism_Trade_Replication-main")

# Input country names into the data set that match the country names in
# our data frame all_pairs.
source("ChangeNamesUN.R")

# Reset the import values and bilateral trade information for 
# the 2018 World Cup countries the year after the tournament.
all_pairs$Imports1_Post1[index]<-NA
all_pairs$Imports2_Post1[index]<-NA
all_pairs$Bi_Trade_P1[index]<-NA

# Fill in the import data for this year.
for(i in index){
	p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
	if(nrow(p)>0) all_pairs$Imports2_Post1[i]<-sum(p$v)/1000
	p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
	if(nrow(p)>0) all_pairs$Imports1_Post1[i]<-sum(p$v)/1000

}

# Fill in the level of bilateral trade for this year.
all_pairs$Bi_Trade_P1[index]<-all_pairs$Imports1_Post1[index]+all_pairs$Imports2_Post1[index]

# Remove the 2019 trade data.
rm(dat)





# Set the working directory to the folder with the trade data.
setwd("~/Downloads/BACI_HS92_V202401b")

# We will next read in the trade data for 2020.
dat<-read.csv("BACI_HS92_Y2020_V202401b.csv",stringsAsFactors=F)

# Reset the working directory to the folder with the replication materials
# from the repository.
setwd("~/Downloads/Nationalism_Trade_Replication-main")

# Input country names into the data set that match the country names in
# our data frame all_pairs.
source("ChangeNamesUN.R")

# Reset the import values and bilateral trade information for 
# the 2018 World Cup countries 2 years after the tournament.
all_pairs$Imports1_Post2[index]<-NA
all_pairs$Imports2_Post2[index]<-NA
all_pairs$Bi_Trade_P2[index]<-NA

# Fill in the import data for this year.
for(i in index){
  p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
  if(nrow(p)>0) all_pairs$Imports2_Post2[i]<-sum(p$v)/1000
  p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
  if(nrow(p)>0) all_pairs$Imports1_Post2[i]<-sum(p$v)/1000
  
}

# Fill in the level of bilateral trade for this year.
all_pairs$Bi_Trade_P2[index]<-all_pairs$Imports1_Post2[index]+all_pairs$Imports2_Post2[index]

# Remove the 2020 trade data.
rm(dat)







# Set the working directory to the folder with the trade data.
setwd("~/Downloads/BACI_HS92_V202401b")

# We will next read in the trade data for 2021.
dat<-read.csv("BACI_HS92_Y2021_V202401b.csv",stringsAsFactors=F)

# Reset the working directory to the folder with the replication materials
# from the repository.
setwd("~/Downloads/Nationalism_Trade_Replication-main")

# Input country names into the data set that match the country names in
# our data frame all_pairs.
source("ChangeNamesUN.R")

# Reset the import values and bilateral trade information for 
# the 2018 World Cup countries 3 years after the tournament.
all_pairs$Imports1_Post3[index]<-NA
all_pairs$Imports2_Post3[index]<-NA
all_pairs$Bi_Trade_P3[index]<-NA

# Fill in the import data for this year.
for(i in index){
  p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
  if(nrow(p)>0) all_pairs$Imports2_Post3[i]<-sum(p$v)/1000
  p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
  if(nrow(p)>0) all_pairs$Imports1_Post3[i]<-sum(p$v)/1000
  
}

# Fill in the level of bilateral trade for this year.
all_pairs$Bi_Trade_P3[index]<-all_pairs$Imports1_Post3[index]+all_pairs$Imports2_Post3[index]

# Remove the 2021 trade data.
rm(dat)






# Set the working directory to the folder with the trade data.
setwd("~/Downloads/BACI_HS92_V202401b")

# We will next read in the trade data for 2022.
dat<-read.csv("BACI_HS92_Y2022_V202401b.csv",stringsAsFactors=F)

# Reset the working directory to the folder with the replication materials
# from the repository.
setwd("~/Downloads/Nationalism_Trade_Replication-main")

# Input country names into the data set that match the country names in
# our data frame all_pairs.
source("ChangeNamesUN.R")

# Reset the import values and bilateral trade information for 
# the 2018 World Cup countries 4 years after the tournament.
all_pairs$Imports1_Post4[index]<-NA
all_pairs$Imports2_Post4[index]<-NA
all_pairs$Bi_Trade_P4[index]<-NA

# Fill in the import data for this year.
for(i in index){
  p<-dat[which(dat$i==all_pairs$Country1[i]&dat$j==all_pairs$Country2[i]),]
  if(nrow(p)>0) all_pairs$Imports2_Post4[i]<-sum(p$v)/1000
  p<-dat[which(dat$i==all_pairs$Country2[i]&dat$j==all_pairs$Country1[i]),]
  if(nrow(p)>0) all_pairs$Imports1_Post4[i]<-sum(p$v)/1000
  
}

# Fill in the level of bilateral trade for this year.
all_pairs$Bi_Trade_P4[index]<-all_pairs$Imports1_Post4[index]+all_pairs$Imports2_Post4[index]

# Remove the 2022 trade data.
rm(dat)



# Add columns to all_pairs showing whether countries had drops in imports each year
# (yes=1, no=0).

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


# Add the following columns to the data frame all_pairs.

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
# https://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=8
# Download the CSV version.

setwd("~/Downloads/Gravity_csv_V202211")

dat<-read.csv("Gravity_V202211.csv",stringsAsFactors = F)

# To reduce the size of the data frame, we will subset to these columns.
dat<-dat[,c("year","country_id_o","country_id_d", "iso3_o", 
            "iso3_d","dist", "gdp_o", "gdp_d", "gdpcap_o", "gdpcap_d", 
            "gatt_o", "gatt_d", "eu_o", "eu_d", "comrelig", 
            "col_dep_ever", "sibling")]

# Reset the country ids as follows.
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


# Remove YUG for 2018 (we already changed Serbia to Yugoslavia).
dat<-dat[-which(dat$year==2018&dat$country_id_o=="YUG"),]
dat<-dat[-which(dat$year==2018&dat$country_id_d=="YUG"),]

# Change the ids for the other countries.
dat$iso3_o<-adapt_names_iso(dat$iso3_o)
dat$iso3_d<-adapt_names_iso(dat$iso3_d)

# Remove all rows where the country id is 3 characters (except the UAE).
dat<-dat[-which(nchar(dat$iso3_d)==3&dat$iso3_d!="UAE"),]
dat<-dat[-which(nchar(dat$iso3_o)==3&dat$iso3_o!="UAE"),]

# The below for loop extracts the gravity information for the rows of all_pairs.
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





# Find the row numbers where Dist is missing and where 
# the year is less than 1940.
missing_dist<-which(is.na(all_pairs$Dist)==T&all_pairs$Year<1940)

# For these rows, fill in Dist with the earliest available 
# information from the dataset dat.
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



# Find the row numbers where Common_Religion is missing.
missing_religion<-which(is.na(all_pairs$Common_Religion)==T)

# For these rows, fill in Common_Religion with the earliest available 
# information from the dataset dat.
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



# Find the row numbers where Colony is missing.
missing_colony<-which(is.na(all_pairs$Colony)==T)

# For these rows, fill in Colony with the earliest available 
# information from the dataset dat.
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




# Set the Colony values to 0 for all pre-1940 rows with Germany.
all_pairs$Colony[all_pairs$Country1=="Germany"&all_pairs$Year<1940]<-0
all_pairs$Colony[all_pairs$Country2=="Germany"&all_pairs$Year<1940]<-0



# Find the row numbers where Sibling is missing.
missing_sibling<-which(is.na(all_pairs$Sibling)==T)

# For these rows, fill in Sibling with the earliest available 
# information from the dataset dat.
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

# Set the Sibling values to 0 for all rows with Germany.
all_pairs$Sibling[all_pairs$Country1=="Germany"]<-0
all_pairs$Sibling[all_pairs$Country2=="Germany"]<-0


# Set the GATT values to 0 for all pre-1940 rows.
all_pairs$Country1_GATT[all_pairs$Year<1940]<-0
all_pairs$Country2_GATT[all_pairs$Year<1940]<-0

# Set the EU values to 0 for all pre-1940 rows.
all_pairs$Country1_EU[all_pairs$Year<1940]<-0
all_pairs$Country2_EU[all_pairs$Year<1940]<-0

# Remove the first row of all_pairs, which does not include any countries.
all_pairs<-all_pairs[-1,]

# The code below adds a column called Could_Play_GS to all_pairs that
# reports whether the country pair could have played against each other
# in the group stage (Yes=1, No=0).
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




# Add a column to all_pairs called Both_GATT that reports whether
# both countries in the dyad were GATT or WTO members (Yes=1, No=0). 
all_pairs$Both_GATT<-as.numeric(all_pairs$Country1_GATT==1 &
                                all_pairs$Country2_GATT==1)

# Add a column to all_pairs called Both_NonGATT that reports whether
# neither country in the dyad was a GATT or WTO member (Yes=1, No=0). 
all_pairs$Both_NonGATT<-as.numeric(all_pairs$Country1_GATT==0 &
                                   all_pairs$Country2_GATT==0)

# Add a column to all_pairs called One_GATT that reports whether
# exactly one country in the dyad was a GATT or WTO member (Yes=1, No=0). 
all_pairs$One_GATT<-as.numeric(all_pairs$Country1_GATT!=
                               all_pairs$Country2_GATT)

# Add a column to all_pairs called Both_EU that reports whether
# both countries in the dyad were EU members (Yes=1, No=0). 
all_pairs$Both_EU<-as.numeric(all_pairs$Country1_EU==1 &
                              all_pairs$Country2_EU==1)

# Add a column to all_pairs called Both_NonEU that reports whether
# neither country in the dyad was an EU member (Yes=1, No=0). 
all_pairs$Both_NonEU<-as.numeric(all_pairs$Country1_EU==0 &
                                 all_pairs$Country2_EU==0)

# Add a column to all_pairs called One_EU that reports whether
# exactly one country in the dyad was an EU member (Yes=1, No=0). 
all_pairs$One_EU<-as.numeric(all_pairs$Country1_EU!=
                             all_pairs$Country2_EU)

# Add a column to all_pairs called Combined_GDP that reports the 
# combined GDP of the two countries in the dyad during the World
# Cup year. 
all_pairs$Combined_GDP<-as.numeric(all_pairs$Country1_GDP+
                                   all_pairs$Country2_GDP)

# Add a column to all_pairs called Combined_GDP_Cap that reports the 
# combined GDP per capita of the two countries in the dyad during the 
# World Cup year. 
all_pairs$Combined_GDP_Cap<-as.numeric(all_pairs$Country1_GDP_Cap+
                                       all_pairs$Country2_GDP_Cap)

# For the rows where the Dist value is missing, fill the missing 
# values in with the values from the Dist1 column that we made earlier
# (wherever that data is available)
all_pairs$Dist[is.na(all_pairs$Dist)==T]<-
all_pairs$Dist1[is.na(all_pairs$Dist)==T]

# Make a new column with the log of the Dist values.
all_pairs$ln_Dist<-log(all_pairs$Dist)


# Convert the column Continguous to a numeric column.
all_pairs$Contiguous<-as.numeric(all_pairs$Contiguous)


# Set the Both_EU values to 0 for all pre-1950 rows.
all_pairs$Both_EU[all_pairs$Year<1950]<-0

# Set the One_EU values to 0 for all pre-1950 rows.
all_pairs$One_EU[all_pairs$Year<1950]<-0

# Set the Both_NonEU values to 0 for all pre-1950 rows.
all_pairs$Both_NonEU[all_pairs$Year<1950]<-0
######### We might also set these values to 1 #############


# Set the Both_GATT values to 0 for all pre-1950 rows.
all_pairs$Both_GATT[all_pairs$Year<1950]<-0

# Set the One_GATT values to 0 for all pre-1950 rows.
all_pairs$One_GATT[all_pairs$Year<1950]<-0

# Set the Both_NonGATT values to 0 for all pre-1950 rows.
all_pairs$Both_NonGATT[all_pairs$Year<1950]<-0
######### We might also set these values to 1 #############

# Remove the following data frames:
rm(dat)
rm(covs1)
rm(covs2)


# The code below adds two columns, Continent1 and Continent2, to 
# all_pairs that show the continents of Country 1 and Country 2.

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


# We can now make a new column called Same_Continent that reports whether
# the dyad was from the same continent (Yes=1, No=0).
all_pairs$Same_Continent<-as.numeric(all_pairs$Continent1==all_pairs$Continent2)




# Change Russia back to the Soviet Union for the pre-1991 rows.
all_pairs$Country1[all_pairs$Country1=="Russia"&
                     all_pairs$Year<1991]<-"Soviet Union"

all_pairs$Country2[all_pairs$Country2=="Russia"&
                   all_pairs$Year<1991]<-"Soviet Union"
                   
# Change Yugoslavia to Serbia-Montenegro for the 2006 rows.                   

all_pairs$Country1[all_pairs$Country1=="Yugoslavia"&
                     all_pairs$Year==2006]<-"Serbia-Montenegro"

all_pairs$Country2[all_pairs$Country2=="Yugoslavia"&
                     all_pairs$Year==2006]<-"Serbia-Montenegro"

# Change Yugoslavia to Serbia for the post-2006 rows.

all_pairs$Country1[all_pairs$Country1=="Yugoslavia"&
                     all_pairs$Year>2006]<-"Serbia"

all_pairs$Country2[all_pairs$Country2=="Yugoslavia"&
                     all_pairs$Year>2006]<-"Serbia"


# Create columns that report the ln(GDP) for Country 1 
# and Country 2.
all_pairs$ln_Country1_GDP <- log(all_pairs$Country1_GDP)
all_pairs$ln_Country2_GDP <- log(all_pairs$Country2_GDP)



# The code below creates new columns called Played_Before_Ever
# and Played_Before_Last_10_Years that report whether the two
# countries had ever played before at the World Cup or had
# played in the last 10 years.

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


# We can now add a column called TradeDrop that reports whether the two countries
# traded less with each other in the World Cup year compared to the previous year.
all_pairs$TradeDrop<-as.numeric(all_pairs$Bi_Trade_Post<all_pairs$Bi_Trade_Pre)





# The below code predicts the level of bilateral trade for each dyad during 
# the World Cup year based on the dyad's bilateral trade levels in the 
# previous five years. It then codes whether the dyad had higher or lower  
# trade than predicted. This information is stored in a new column called y
# (lower than predicted=1, higher than predicted=0).

all_pairs$Prediction<-NA

all_pairs$y<-NA

xs<--5:-1
for(i in 1:nrow(all_pairs)){
  ys<-all_pairs[i,c("Bi_Trade_M5","Bi_Trade_M4","Bi_Trade_M3","Bi_Trade_M2","Bi_Trade_Pre")]	
  if(sum(is.na(ys))<4){reg<-lm(as.numeric(ys)~xs); all_pairs$Prediction[i]<-as.numeric(reg$coefficients[1])
  all_pairs$y[i]<-as.numeric(all_pairs$Bi_Trade_Post[i]<all_pairs$Prediction[i])
  }}




# The below code does the same as the code above, but focusing on the Country 1's 
# imports from Country 2 (lower than predicted=1, higher than predicted=0).

all_pairs$Prediction1<-NA

all_pairs$y1<-NA

xs<--5:-1
for(i in 1:nrow(all_pairs)){
  ys<-all_pairs[i,c("Imports1_Pre5","Imports1_Pre4","Imports1_Pre3","Imports1_Pre2","Imports1_Pre")]	
  if(sum(is.na(ys))<4){reg<-lm(as.numeric(ys)~xs); all_pairs$Prediction1[i]<-as.numeric(reg$coefficients[1])
  all_pairs$y1[i]<-as.numeric(all_pairs$Imports1[i]<all_pairs$Prediction1[i])
  }}

# The below code does the same as the code above, but focusing on the Country 2's 
# imports from Country 1 (lower than predicted=1, higher than predicted=0).

all_pairs$Prediction2<-NA

all_pairs$y2<-NA

xs<--5:-1
for(i in 1:nrow(all_pairs)){
  ys<-all_pairs[i,c("Imports2_Pre5","Imports2_Pre4","Imports2_Pre3","Imports2_Pre2","Imports2_Pre")]	
  if(sum(is.na(ys))<4){ reg<-lm(as.numeric(ys)~xs); all_pairs$Prediction2[i]<-as.numeric(reg$coefficients[1])
  all_pairs$y2[i]<-as.numeric(all_pairs$Imports2[i]<all_pairs$Prediction2[i])
  }}





# We can now create placebos corresponding to the three outcome measures 
# above (y, y1, and y2) that look at whether bilateral trade/imports
# the year before the World Cup were lower or higher than expected.
# We will call these placebos ym1, y1m1, and y2m1.

all_pairs$Prediction_Prev<-NA

all_pairs$ym1<-NA

xs<--5:-1
for(i in 1:nrow(all_pairs)){
  ys<-all_pairs[i,c("Bi_Trade_M6","Bi_Trade_M5","Bi_Trade_M4","Bi_Trade_M3","Bi_Trade_M2")]	
  if(sum(is.na(ys))<4){reg<-lm(as.numeric(ys)~xs); all_pairs$Prediction[i]<-as.numeric(reg$coefficients[1])
  all_pairs$ym1[i]<-as.numeric(all_pairs$Bi_Trade_Pre[i]<all_pairs$Prediction[i])
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





# The below code calculates the percentage change in trade 
# for dyads during the World Cup year, storing this information
# as a new column of all_pairs called Percent_Change. 
# The code also creates another column called Percent_Change_Adjusted
# that caps bilateral trade swings at +/-20%.

max<-0.2

all_pairs$Percent_Change<-(all_pairs$Bi_Trade_Post-
                           all_pairs$Bi_Trade_Pre)/
                           all_pairs$Bi_Trade_Pre

all_pairs$Percent_Change_Adjusted<-all_pairs$Percent_Change

all_pairs$Percent_Change_Adjusted[all_pairs$Percent_Change_Adjusted>max&
                                  is.na(all_pairs$Percent_Change_Adjusted)==F]<-max

all_pairs$Percent_Change_Adjusted[all_pairs$Percent_Change_Adjusted<(-max)&
                                  is.na(all_pairs$Percent_Change_Adjusted)==F]<- -max


# For dyads that had no bilateral trade during the World Cup year and
# previous year, we will code there change in trade as 0%.
all_pairs$Percent_Change_Adjusted[all_pairs$Bi_Trade_Pre==0 &
                                  all_pairs$Bi_Trade_Post==0 &
                                  is.na(all_pairs$Bi_Trade_Pre)==F & 
                                  is.na(all_pairs$Bi_Trade_Post)==F]<- 0

# For dyads that had some bilateral trade during the World Cup year and
# none reported the previous year, we will code there change in trade as 20%
# (rather than infinity).
all_pairs$Percent_Change_Adjusted[all_pairs$Bi_Trade_Pre==0 &
                                    all_pairs$Bi_Trade_Post>0 &
                                    is.na(all_pairs$Bi_Trade_Pre)==F & 
                                    is.na(all_pairs$Bi_Trade_Post)==F]<- max



# We can now make a column that records the change in trade for countries
# the year before the World Cup. We can think of this variable as another
# placebo outcome.
all_pairs$Percent_Change_Prev<-(all_pairs$Bi_Trade_Pre-all_pairs$Bi_Trade_M2)/all_pairs$Bi_Trade_M2

# The code below caps trade swings for this placebo outcome at +/-20%.
all_pairs$Percent_Change_Prev_Adjusted<-all_pairs$Percent_Change_Prev


all_pairs$Percent_Change_Prev_Adjusted[all_pairs$Percent_Change_Prev_Adjusted>max&is.na(all_pairs$Percent_Change_Prev_Adjusted)==F]<-max

all_pairs$Percent_Change_Prev_Adjusted[all_pairs$Percent_Change_Prev_Adjusted<(-max)&is.na(all_pairs$Percent_Change_Prev_Adjusted)==F]<- -max


# For dyads that had no bilateral trade the year before the World Cup year and
# the previous year, we will code there previous change in trade as 0%.
all_pairs$Percent_Change_Prev_Adjusted[all_pairs$Bi_Trade_M2==0 &
                                    all_pairs$Bi_Trade_Pre==0 &
                                    is.na(all_pairs$Bi_Trade_M2)==F & 
                                    is.na(all_pairs$Bi_Trade_Pre)==F]<- 0

# For dyads that had some bilateral trade the year before the World Cup year and
# none reported the previous year, we will code there previous change in trade as 20%.
all_pairs$Percent_Change_Prev_Adjusted[all_pairs$Bi_Trade_M2==0 &
                                    all_pairs$Bi_Trade_Pre>0 &
                                    is.na(all_pairs$Bi_Trade_M2)==F & 
                                    is.na(all_pairs$Bi_Trade_Pre)==F]<- max

# We will now convert the adjusted change in trade (capped at +/-20%)
# to a numeric value for trade during the World Cup year. At most,
# this value is 20% more than it was the year before the World Cup
# year, and at the least it is 20% less.
all_pairs$Adjusted_Trade<-(all_pairs$Percent_Change_Adjusted+1)*
                          all_pairs$Bi_Trade_Pre

# We will now calculate the ln(trade) value for each country
# the year before the World Cup year.
all_pairs$ln_Bi_Trade_Pre<-log(all_pairs$Bi_Trade_Pre+1)

# We will also calculate the ln(trade) value for each country
# the World Cup year, with trade swings capped at +/-20%.
all_pairs$ln_Adjusted_Trade<-log(all_pairs$Adjusted_Trade+1)


# We will now repeat this process for the year before the World Cup
# (and the previous year) to get another placebo outcome.

all_pairs$ln_Bi_Trade_M2<-log(all_pairs$Bi_Trade_M2+1)

all_pairs$Adjusted_Trade_Prev<-(all_pairs$Percent_Change_Prev_Adjusted+1)*
  all_pairs$Bi_Trade_M2

all_pairs$ln_Adjusted_Trade_Prev<-log(all_pairs$Adjusted_Trade_Prev+1)



# For any dyads where we are missing a y value but where we have trade 
# data for the World Cup year and previous year, we will just input
# 1 if their trade the World Cup year was less than the prior year and 
# 0 if it was more than the prior year.
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



# Reset the working directory to the folder with the replication materials
# from the repository.
setwd("~/Downloads/Nationalism_Trade_Replication-main")

# We will now save all_pairs as a data frame.
write.csv(all_pairs,"All_Pairs_July25.csv",row.names = F)


# Lastly, we will subset to the countries that really did play each other in
# the World Cup group stage and also create a data frame that summarizes some
# information about them.

# We first subset to the countries that played in the group stage.
real_data_full_all<-all_pairs[all_pairs$Group_Stage==1,]

# We then subset to the dyads that we have bilateral trade data for during
# the World Cup year and previous year.
real_data_full<-real_data_full_all[is.na(real_data_full_all$y)==F,]

# We can now calculate various statistics for these dyads.
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

# We can also calculate the statistics just for the dyads where soccer
# is the most popular sport for both sides.
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

# Turn the object real_data into a matrix.
real_data<-matrix(real_data,nrow=1)

# Turn real_data into a data frame.
real_data<-data.frame(real_data)


# Assign these column names to real_data.
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




# Save the data for the countries that played at the World Cup.
write.csv(real_data_full,"RealDataFull_July25.csv",row.names=F)

# Save the summary data for the countries that played at the World Cup.
write.csv(real_data,"RealData_July25.csv",row.names=F)
