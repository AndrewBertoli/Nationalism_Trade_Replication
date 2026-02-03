# This file reproduces the results for the regional tournaments. Before running
# this code, it is first necessary to construct the datasets by running
# BuildDataRegional.R and RegionalSimulations.R.

# We will start by loading the following packages.
require(foreign)
require(ggplot2)
require(cowplot)
require(rdrobust)

# We will next set the working directory to the folder with the replication
# materials from the "Nationalism_Trade_Replication" repository from Github
setwd("~/Downloads/Nationalism_Trade_Replication-main")


# We can first bring the following datasets into the workspace.

# The dataset with pairs of countries for the regional tournaments.
all_pairs<-read.csv("All_Pairs_Regional_9_25.csv",stringsAsFactors=F)

# The dataset with various statistics about the pairs of countries 
# that played in the regional tournament group stages.

real_data_europe<-read.csv("RealData_Europe_Sept25.csv",stringsAsFactors=F)

real_data_africa<-read.csv("RealData_Africa_Sept25.csv",stringsAsFactors=F)

# The dataset with the country pairs that played in the regional tournament group
# stages.

real_data_full<-read.csv("RealDataFull_Europe_Sept25.csv",stringsAsFactors=F)

real_data_full<-read.csv("RealDataFull_Africa_Sept25.csv",stringsAsFactors=F)

# The dataset with the results from the simulations that involved
# rerandomizing the regional tournmaents.

perm_data_europe<-read.csv("PermData_Europe_Sept25.csv",stringsAsFactors=F)

perm_data_africa<-read.csv("PermData_Africa_July25.csv",stringsAsFactors=F)

# We will begin with the Euro data. We will subset to these country pairs.
all_pairs_europe<-all_pairs[all_pairs$Event=="Europe",]

# Total number of group stage games
sum(all_pairs_europe$Group_Stage==1)

# Total number of knockout stage games
sum(all_pairs_europe$Knockout_Stage==1)

# Percentage of teams that were not randomized to play in the
# group stage but met in the knockout stage
mean(all_pairs_europe$Group_Stage==0&all_pairs_europe$Knockout_Stage==1)

# Cases where countries played in both the group and knockout stages
all_pairs_europe[all_pairs_europe$Group_Stage==1&all_pairs_europe$Knockout_Stage==1,]

# Subset to pairs of countries that could have played in the group stage.
sample<-all_pairs_europe[all_pairs_europe$Could_Play_GS==1,]

sample<-sample[sample$Year>1992,]

# Print the total number of these dyads.
dim(sample)

# Number of these pairs that we have trade data on
sum(is.na(sample$Bi_Trade_Pre)==F &
    is.na(sample$Bi_Trade_Post)==F)

# Percentage of these pairs that we have trade data on
mean(is.na(sample$Bi_Trade_Pre)==F &
    is.na(sample$Bi_Trade_Post)==F)

# Drop the cases where we are missing the bilateral trade data.
sample<-sample[is.na(sample$Bi_Trade_Pre)==F &
               is.na(sample$Bi_Trade_Post)==F,]

sample<-sample[sample$Year>=1992,]

mean(sample$Year>=2000)

#### DYAD-LEVEL ANALYSIS

# View sample size
nrow(sample)

# View number of pairs of countries that played in the group stage
sum(sample$Group_Stage==1)

# View number of pairs of countries that could have played in the 
# group stage but did not
sum(sample$Group_Stage==0)

# Number of different countries in the sample
length(unique(c(sample$Country1,sample$Country2)))

# View the different countries in alphabetical order
sort(unique(c(sample$Country1,sample$Country2)))

# View the temporal span of the data
range(sample$Year)

# View the span of time that the data covers
range(sample$Year)[2]-range(sample$Year)[1]



# One and two-sided p-value from randomization inference

# One-tailed p-value for adjusted change in ln(trade)--gravity model approach--all dyads
mean(perm_data$ln_Trade_Change_Adjusted<=real_data$ln_Trade_Change_Adjusted)

# Two-tailed p-value for adjusted change in ln(trade)--gravity model approach--all dyads
mean(abs(perm_data$ln_Trade_Change_Adjusted-
           mean(perm_data$ln_Trade_Change_Adjusted))>=abs(real_data$ln_Trade_Change_Adjusted-
                                                            mean(perm_data$ln_Trade_Change_Adjusted)))

# One-tailed p-value for adjusted change in ln(trade)--gravity model approach--soccer dyads
mean(perm_data$Soccer_ln_Trade_Change_Adjusted<=
       real_data$Soccer_ln_Trade_Change_Adjusted)

# Two-tailed p-value for adjusted change in ln(trade)--gravity model approach--soccer dyads
mean(abs(perm_data$Soccer_ln_Trade_Change_Adjusted-
           mean(perm_data$Soccer_ln_Trade_Change_Adjusted))>=
       abs(real_data$Soccer_ln_Trade_Change_Adjusted-
             mean(perm_data$Soccer_ln_Trade_Change_Adjusted)))


# One-tailed p-value for adjusted percentage change in trade--all dyads
mean(perm_data$Percent_Change_Adjusted<=real_data$Percent_Change_Adjusted)

# Two-tailed p-value for adjusted percentage change in trade--all dyads
mean(abs(perm_data$Percent_Change_Adjusted-
           mean(perm_data$Percent_Change_Adjusted))>=abs(real_data$Percent_Change_Adjusted-
                                                           mean(perm_data$Percent_Change_Adjusted)))

# One-tailed p-value for adjusted percentage change in trade--soccer dyads
mean(perm_data$Soccer_Percent_Change_Adjusted<=
       real_data$Soccer_Percent_Change_Adjusted)

# Two-tailed p-value for adjusted percentage change in trade--soccer dyads
mean(abs(perm_data$Soccer_Percent_Change_Adjusted-
           mean(perm_data$Soccer_Percent_Change_Adjusted))>=
       abs(real_data$Soccer_Percent_Change_Adjusted-
             mean(perm_data$Soccer_Percent_Change_Adjusted)))

# One-tailed p-value for probability of trade drop--all dyads
mean(perm_data$y>=real_data$y)

# Two-tailed p-value for probability of trade drop--all dyads
mean(abs(perm_data$y-
         mean(perm_data$y))>=(real_data$y-
                              mean(perm_data$y)))


# One-tailed p-value for probability of trade drop--soccer dyads
mean(perm_data$Soccer_y>=real_data$Soccer_y)

# Two-tailed p-value for probability of trade drop--soccer dyads
mean(abs(perm_data$Soccer_y-
     mean(perm_data$Soccer_y))>=(real_data$Soccer_y-
                                 mean(perm_data$Soccer_y)))







# Predict missing GDP values.
source("PredictMissingGDP.R")


# Regression models

# All country dyads--Gravity model
model_v1<-lm(ln_Adjusted_Trade-ln_Bi_Trade_Pre ~ 
               Group_Stage + ln_Dist + ln_Country1_GDP +
               ln_Country2_GDP + Both_GATT + Both_EU +
               Both_Dem  + Contiguous + Colony + Sibling +
               Alliance_Year_Before + Any_Disputes_Before+
               as.factor(Year), sample)

# View the results.
summary(model_v1)




# All country dyads--percentage change in trade

model_v3<-lm(Percent_Change_Adjusted ~ Group_Stage + ln_Dist + ln_Country1_GDP +
               ln_Country2_GDP + Both_GATT + Both_EU +
               Both_Dem +  Contiguous + Colony + Sibling +
               Alliance_Year_Before + Any_Disputes_Before+
               as.factor(Year), sample)

# View the results
summary(model_v3)






# All country dyads--probability of drop in trade

model_v5<-lm(y ~ Group_Stage + ln_Dist + ln_Country1_GDP +
               ln_Country2_GDP + Both_GATT + Both_EU +
               Both_Dem + Contiguous + Colony + Sibling +
               Alliance_Year_Before + Any_Disputes_Before+
               as.factor(Year), sample)

# View the results
summary(model_v5)






# Logit results for probability of drop in trade

# All dyads

model_v1<-glm(y~Group_Stage + ln_Dist + ln_Country1_GDP +
             ln_Country2_GDP + Both_GATT + Both_EU +
             Both_Dem + Contiguous + Colony + Sibling +
             Alliance_Year_Before + Any_Disputes_Before+
             as.factor(Year),family="binomial",sample)

# View the results
summary(model_v1)

# Calculate the one-tailed p-value.
0.036322/2





# Probit for probability of drop in trade

# All dyads

model_v1<-glm(y~Group_Stage + ln_Dist + ln_Country1_GDP +
              ln_Country2_GDP + Both_GATT + Both_EU +
              Both_Dem + Contiguous + Colony + Sibling +
              Alliance_Year_Before + Any_Disputes_Before+
              as.factor(Year),
            family=binomial(link="probit"),sample)


# View the results
summary(model_v1)

# Calculate the one-tailed p-value.
0.040683/2











# Changing the minimum/maximum percentage change in trade.

# The cap in the paper is set at +/-20%. We will try different values,
# ranging from +/-3% to +/-100%.

# Create a vector with these values.
caps<-seq(0.03, 1, by=0.01)

# We will start with the percentage change in trade outcome for all dyads.

# Start a for loop that runs through these values.
for(i in 1:length(caps)){
  
# Set the cap as the ith element of the vector caps.
  max<-caps[i]
  
# Calculate the unadjusted percerntage change in trade.
  sample$Percent_Change_Shift<-(sample$Bi_Trade_Post-
                                  sample$Bi_Trade_Pre)/sample$Bi_Trade_Pre

# Set the maximum of this new variable as max (defined above--e.g., 3%).
  sample$Percent_Change_Shift[sample$Percent_Change_Shift>max&
                                is.na(sample$Percent_Change_Shift)==F]<-max

# Set the minimum of this new variable as -max (e.g., -3%).
  sample$Percent_Change_Shift[sample$Percent_Change_Shift<(-max)&
                                is.na(sample$Percent_Change_Shift)==F]<- -max
                                
# For dyads that had no bilateral trade during the World Cup year and
# previous year, we will code there change in trade as 0%.
sample$Percent_Change_Shift[sample$Bi_Trade_Pre==0 &
                            sample$Bi_Trade_Post==0 &
                            is.na(sample$Bi_Trade_Pre)==F & 
                            is.na(sample$Bi_Trade_Post)==F]<- 0

# For dyads that had some bilateral trade during the World Cup year and
# none reported the previous year, we will code there change in trade as max
# (rather than infinity).
sample$Percent_Change_Shift[sample$Bi_Trade_Pre==0 &
                            sample$Bi_Trade_Post>0 &
                            is.na(sample$Bi_Trade_Pre)==F & 
                            is.na(sample$Bi_Trade_Post)==F]<- max                                

# Create a model with this new variable as the outcome.
  model<-lm(Percent_Change_Shift~Group_Stage + ln_Dist + 
  			ln_Country1_GDP + ln_Country2_GDP +
            Both_GATT + Both_EU + Both_Dem  + Contiguous +
            Colony + Sibling + Alliance_Year_Before +
            Any_Disputes_Before + as.factor(Year), sample)
              
# Print max for this round of the for loop.
  print(max)
  
# Print the one-tailed p-value for all dyads this round of the for loop.
  print(summary(model)$coefficients[2,4]/2)

# End the for loop.
}






# Above we focused on the percentage change in trade outcome. We will now
# look at the change in ln(trade) outcome.

# Create a vector with these values that will let us run from  +/-3% to +/-100%.
caps<-seq(0.03, 1, by=0.01)

# Start a for loop that runs through these values.
for(i in 1:length(caps)){

# Set the cap as the ith element of the vector caps.
  max<-caps[i]

# Calculate the unadjusted percerntage change in trade.
  sample$Percent_Change_Shift<-(sample$Bi_Trade_Post-
                                  sample$Bi_Trade_Pre)/sample$Bi_Trade_Pre

# Set the maximum of this new variable as max (defined above--e.g., 3%).
  sample$Percent_Change_Shift[sample$Percent_Change_Shift>max&
                                is.na(sample$Percent_Change_Shift)==F]<-max

# Set the minimum of this new variable as -max (e.g., -3%).
  sample$Percent_Change_Shift[sample$Percent_Change_Shift<(-max)&
                                is.na(sample$Percent_Change_Shift)==F]<- -max

# For dyads that had no bilateral trade during the World Cup year and
# previous year, we will code there change in trade as 0%.
sample$Percent_Change_Shift[sample$Bi_Trade_Pre==0 &
                            sample$Bi_Trade_Post==0 &
                            is.na(sample$Bi_Trade_Pre)==F & 
                            is.na(sample$Bi_Trade_Post)==F]<- 0

# For dyads that had some bilateral trade during the World Cup year and
# none reported the previous year, we will code there change in trade as max
# (rather than infinity).
sample$Percent_Change_Shift[sample$Bi_Trade_Pre==0 &
                            sample$Bi_Trade_Post>0 &
                            is.na(sample$Bi_Trade_Pre)==F & 
                            is.na(sample$Bi_Trade_Post)==F]<- max  
                                                                    
  sample$Adjusted_Trade_Post<-(1+sample$Percent_Change_Shift)*sample$Bi_Trade_Pre

# Calculate the ln(trade) value given the adjustment.
  sample$ln_Adjusted_Trade_Post<-log(sample$Adjusted_Trade_Post+1)

# Create a model with this new variable as the outcome.
  model<-lm(ln_Adjusted_Trade_Post-ln_Bi_Trade_Pre~Group_Stage + ln_Dist + 
  			ln_Country1_GDP + ln_Country2_GDP +
            Both_GATT + Both_EU + Both_Dem  + Contiguous +
            Colony + Sibling + Alliance_Year_Before +
            Any_Disputes_Before + as.factor(Year), sample)
                          
# Print max for this round of the for loop.
  print(max)
  
# Print the one-tailed p-value for all dyads this round of the for loop.
  print(summary(model)$coefficients[2,4]/2)

# End the for loop.  
}






# Winners vs. Losers

# Create a data set with just the countries that played each other
# in the group stage or knockout stage.
games<-all_pairs[(all_pairs$Group_Stage==1|
                 all_pairs$Knockout_Stage==1)&
                 all_pairs$Event=="Europe",]

games[games$Country1=="Czech Republic"&games$Country2=="Latvia"& games$Year==2004,]$Both_EU<-1

games[games$Country1=="Netherlands"&games$Country2=="Latvia"&
games$Year==2004,]$Both_EU<-1

games[games$Country1=="Belgium"&games$Country2=="Finland"&
games$Year==2021,]$Both_EU<-1

games[games$Country1=="Ukraine"&games$Country2=="North Macedonia"&
games$Year==2021,]$Both_EU<-0

games[games$Country1=="Russia"&games$Country2=="Finland"&
games$Year==2021,]$Both_EU<-0

games[games$Country1=="Netherlands"&games$Country2=="North Macedonia"&
games$Year==2021,]$Both_EU<-0

games[games$Country1=="Finland"&games$Country2=="Denmark"&
games$Year==2021,]$Both_EU<-1

games[games$Country1=="Austria"&games$Country2=="North Macedonia"&
games$Year==2021,]$Both_EU<-0

# Calculate goal difference in each game (knockout stage only).
games$Z<-games$KS_Score1-games$KS_Score2

# For all dyads that did not play in the knockout stage, we will
# use the score from their group stage game.

# Calculate goal difference for countries that played
# in the group stage and not in the knockout stage and set
# this value as Z for these dyads.
games$Z[is.na(games$Z)==T]<-(games$GS_Score1-games$GS_Score2)[is.na(games$Z)==T]

# Create a data frame called d1 that primarily has the information for Country 1 in each row.
d1<-games[,c("Country1","Country2","Year","Irst1", "Milex1", "Milper1","Tpop1","Upop1",
             "Democracy1","Country1_GATT","Country1_EU","IndependenceYear1","ln_Country1_GDP",
             "ln_Country2_GDP","Both_Dem","Both_NonDem", "Diff_Regime", "Contiguous",
             "Total_Disputes_Before","Any_Disputes_Before","Alliance_Year_Before","Dist",
             "ln_Dist","Group_Stage","Knockout_Stage","Imports1_Pre11","Imports1_Pre10",
             "Imports1_Pre9","Imports1_Pre8","Imports1_Pre7","Imports1_Pre6","Imports1_Pre5",
             "Imports1_Pre4","Imports1_Pre3","Imports1_Pre2","Imports1_Pre","Imports1",
             "Imports1_Post1","Imports1_Post2","Imports1_Post3","Imports1_Post4","Imports1_Post5",
             "Imports1_Post6","Imports1_Post7","Imports1_Post8","Imports1_Post9","Imports1_Post10",
             "ln_Dist","Common_Religion","Colony","Sibling","Both_GATT","One_GATT","Both_EU",
             "One_EU", "Could_Play_GS","y1","Z","Prediction1","y","y1m1",
             "Played_Before_Last_10_Years")]

# Create a column called win that is all NAs.
d1$Win<-NA

# Set the win column value to 1 for rows where the score is not 0 (since
# Country 1 won these games).
d1$Win[d1$Z!=0]<-1             

# Create a data frame called d2 that primarily has the information for Country 2 in each row.
d2<-games[,c("Country2","Country1","Year","Irst2", "Milex2", "Milper2","Tpop2","Upop2",
             "Democracy2","Country2_GATT","Country2_EU","IndependenceYear2","ln_Country2_GDP",
             "ln_Country1_GDP","Both_Dem","Both_NonDem", "Diff_Regime","Contiguous",
             "Total_Disputes_Before", "Any_Disputes_Before","Alliance_Year_Before","Dist",
             "ln_Dist","Group_Stage","Knockout_Stage","Imports2_Pre11","Imports2_Pre10",
             "Imports2_Pre9","Imports2_Pre8","Imports2_Pre7","Imports2_Pre6","Imports2_Pre5",
             "Imports2_Pre4","Imports2_Pre3","Imports2_Pre2","Imports2_Pre","Imports2",
             "Imports2_Post1","Imports2_Post2","Imports2_Post3","Imports2_Post4","Imports2_Post5",
             "Imports2_Post6","Imports2_Post7","Imports2_Post8","Imports2_Post9","Imports2_Post10",
             "ln_Dist","Common_Religion","Colony","Sibling","Both_GATT","One_GATT","Both_EU",
             "One_EU","Could_Play_GS","y2","Z","Prediction2","y","y2m1",
             "Played_Before_Last_10_Years")]

# Create a column called win that is all NAs.
d2$Win<-NA

# Set the win column value to 0 for rows where the score is not 0 (since
# Country 2 lost these games).
d2$Win[d1$Z!=0]<-0                

# Make the score differential negative for d2 (since Country 2
# lost these games unless the goal difference was 0, in which
# case the goal difference will remain 0).
d2$Z<- -d2$Z

# Set the d2 column names to be the same as the d1 column names so that
# we can merge the data sets (note that this will switch Country 2 to 
# "Country 1" in d2).
colnames(d2)<-colnames(d1)

# Merge d1 and d2 into a single data frame.
combined<-rbind(d1,d2)

# Create a column that records whether the main country in each row
# (now called "Country 1" for all rows) lost. 
combined$Loss<- 1-combined$Win

# Set the max/min for a change in imports at +/-20%.
max<-0.2

# Create a column for the percent change in trade.
combined$Percent_Change_Adjusted<-(combined$Imports1-
                                   combined$Imports1_Pre)/combined$Imports1_Pre

# Set this value at 0 for cases where Imports1 and Imports1_Pre are both 0 
# (meaning that Country 1 had no imports from Country 2 in the World Cup year
# or the previous year).
combined$Percent_Change_Adjusted[combined$Imports1==0 &
								 combined$Imports1_Pre==0 &
								 is.na(combined$Imports1)==F &
								 is.na(combined$Imports1_Pre)==F]<-0

# When this value is greater than 20%, cap it at 20%.
combined$Percent_Change_Adjusted[combined$Percent_Change_Adjusted>max&
                                 is.na(combined$Percent_Change_Adjusted)==F]<-max

# When this value is less than -20%, cap it at -20%.
combined$Percent_Change_Adjusted[combined$Percent_Change_Adjusted<(-max)&
                                 is.na(combined$Percent_Change_Adjusted)==F]<- -max

# Add an adjusted imports column that is the value of imports capped at a +/-20%
# swing from the prior year.
combined$Adjusted_Imports<-(combined$Percent_Change_Adjusted+1)*combined$Imports1_Pre

# Add a column for ln(trade) for this new value.
combined$ln_Adjusted_Imports<-log(combined$Adjusted_Imports+1)

# Add a column for ln(imports) for the year before the World Cup.
combined$ln_Imports_Pre<-log(combined$Imports1_Pre+1)

# Add a column for the change in adjusted ln(imports).
combined$Change_ln_Adjusted_Imports<-combined$ln_Adjusted_Imports-
                                     combined$ln_Imports_Pre
                                     
# For a placebo outcome, we will repeat 
combined$Percent_Change_Adjusted_Prev<-(combined$Imports1_Pre-
                                        combined$Imports1_Pre2)/combined$Imports1_Pre2


combined$Percent_Change_Adjusted_Prev[combined$Percent_Change_Adjusted_Prev>max&
                                      is.na(combined$Percent_Change_Adjusted_Prev)==F]<-max


combined$Percent_Change_Adjusted_Prev[combined$Percent_Change_Adjusted_Prev<(-max)&
                                      is.na(combined$Percent_Change_Adjusted_Prev)==F]<- -max


combined$Adjusted_Imports_Prev<-(combined$Percent_Change_Adjusted_Prev+1)*combined$Imports1_Pre2
combined$ln_Adjusted_Imports_Prev<-log(combined$Adjusted_Imports_Prev+1)
combined$ln_Imports_Pre2<-log(combined$Imports1_Pre2+1)

combined$Change_ln_Adjusted_Imports_Prev<-combined$ln_Adjusted_Imports_Prev-
                                          combined$ln_Imports_Pre2                                     

# Fll missing GDP values using earlier predictions.
source("FillMissingGDP.R")

# Create a new data frame that excludes all the ties.
no_ties<-combined[combined$Z!=0,]

# Create a new data frame with just the ties.
ties<-combined[combined$Z==0,]

# Do a t-test for the countries that won or lost
# by one point.
t.test(Percent_Change_Adjusted~Loss,
       no_ties[abs(no_ties$Z)<=1,])


# Since who won and lost the close games was not really random,
# we will use local linear regression to try to adjust for small
# differences between the bare winners and bare losers.

# RDD Estimates

# Create the model for change in ln(imports) with controls.
model<-lm(ln_Adjusted_Imports-ln_Imports_Pre~Loss+Z+I(Loss*Z)+
          ln_Dist + ln_Country1_GDP + ln_Country2_GDP + 
            Both_GATT + Both_EU + Both_Dem +  
          Contiguous + Colony + Sibling + 
          Alliance_Year_Before+Any_Disputes_Before+
          as.factor(Year), no_ties[abs(no_ties$Z)<=3,])

# View the results.
summary(model)

# Create the model for change in ln(imports) without controls.
model<-lm(ln_Adjusted_Imports-ln_Imports_Pre~Loss+Z+I(Loss*Z),
          no_ties[abs(no_ties$Z)<=3,])

# View the results.
summary(model)

# Calculate the sample size for the two tests above.
with(no_ties[abs(no_ties$Z)<=3,], 
     sum(is.na(ln_Adjusted_Imports-ln_Imports_Pre)==F))



# We will now focus on the percentage change in imports.

# Create the model for change in percentage change in imports
# variable, with controls.

model<-lm(Percent_Change_Adjusted~Loss+Z+I(Loss*Z)+
            ln_Dist + ln_Country1_GDP + ln_Country2_GDP + 
            Both_GATT + Both_EU + Both_Dem +  
            Contiguous + Colony + Sibling + 
            Alliance_Year_Before+Any_Disputes_Before+
            as.factor(Year), no_ties[abs(no_ties$Z)<=3,])

# View the results.
summary(model)


# We will now create the model for change in percentage change in imports
# variable, without controls.

model<-lm(Percent_Change_Adjusted~Loss+Z+I(Loss*Z),
          no_ties[abs(no_ties$Z)<=3,])

# View the results.
summary(model)


# Calculate the sample size for the two tests above.
with(no_ties[abs(no_ties$Z)<=3,], 
     sum(is.na(Percent_Change_Adjusted)==F))







# We will now focus on the drop in trade outcome.

# Create the model for the drop in trade, with controls.

model<-lm(y1~Loss+Z+I(Loss*Z)+
            ln_Dist + ln_Country1_GDP + ln_Country2_GDP +
            Both_GATT + Both_EU + Both_Dem +  
            Contiguous + Colony + Sibling + 
            Alliance_Year_Before+Any_Disputes_Before+
            as.factor(Year), no_ties[abs(no_ties$Z)<=3,])

# View the results.
summary(model)


# We will now do the model without control variables.
model<-lm(y1~Loss+Z+I(Loss*Z),
          no_ties[abs(no_ties$Z)<=3,])

# View the results.
summary(model)




# Calculate the sample size for the two tests above.
with(no_ties[abs(no_ties$Z)<=3,], 
     sum(is.na(y1)==F))









            
# We will now create the RD graph with confidence intervals.

# Create a vector for win/loss differential: -3, -2, -1, 1, 2, 3
# We ignore ties here, as well as games that were decided by 4 points
# or more.

zs<-c(-3:-1,1:3)


# Create a regression model for the winners called mod1.
mod1<-lm(Percent_Change_Adjusted*100~Z,no_ties[no_ties$Z%in%1:3,])

# Store the intercept of this regression model as a1.
a1<-as.numeric(mod1$coefficients[1])

# Store the slope of this regression model as b1.
b1<-as.numeric(mod1$coefficients[2])


# Create a regression model for the losers called mod1.
mod2<-lm(Percent_Change_Adjusted*100~Z,no_ties[no_ties$Z%in%(-1:-3),])

# Store the intercept of this regression model as a1.
a2<-as.numeric(mod2$coefficients[1])

# Store the slope of this regression model as b2.
b2<-as.numeric(mod2$coefficients[2])

# Use tapply to calculate and store the average outcome 
# (Perecent_Change_Adjusted) for each value in the vector zs.

avgs<-with(no_ties[abs(no_ties$Z)<=3,],
     tapply(Percent_Change_Adjusted,Z,
            mean,na.rm=T)*100
)

# Create a data frame that contains the values in zs
# as one column and the corresponding average outcome
# for each value in zs as a second column.

x<-data.frame(zs,avgs)

# Create a ggplot called plot1.
plot1<-ggplot(x,aes(x=zs,y=avgs))+ # Set the x values as zs and the
								   # y values as the average outcomes
								   # for each value in zs.
geom_point(color="darkblue")+ # Set dark blue points.
theme_classic()+ # Set the theme.
xlim(-3.1,3.1)+xlab("Points from Winning")+ # Set the x-limits and x-axis label.
ylab("Percentage Change in Imports")+ # Set the y-axis label.
geom_vline(xintercept=0)+ # Draw a vertical line at x=0
geom_segment(aes(x=0,y=a1,xend=3.1,yend=a1+b1*3.1),
             color="cornflowerblue") + # Add the regression line for the winners.
geom_segment(aes(x=0,y=a2,xend=-3.1,yend=a2+b2*-3.1),
             color="cornflowerblue") # Add the regression line for the losers.
       
# We will now add confidence intervals to plot1.       

# Create a vector to store the upper bounds of the confidence intervals.
upper<-rep(NA,6)

# Create another vector to store the lower bounds of the confidence intervals.
lower<-rep(NA,6)

# Start a for loop to calculate and store the upper and lower 
# bounds for each value in zs.
for(i in 1:length(zs)){

# Calculte the standard error for each value in zs.
# Note here thatwe are using the formula SE=sigma/sqrt(n).
	
  se<-sd(no_ties$Percent_Change_Adjusted[no_ties$Z==zs[i]]*100,na.rm=T)/
    sqrt(sum(is.na(no_ties$Percent_Change_Adjusted[no_ties$Z==zs[i]]==F)))

# Calculate and store the upper bound of the confidence interval for
# each value in zs.    
  upper[i]<-avgs[i]+1.96*se
  
# Calculate and store the lower bound of the confidence interval for
# each value in zs.    
  lower[i]<-avgs[i]-1.96*se

# End the for loop.
}


# We will now generate plot1 with the confidence intervals for each value in zs.

# Start the plot.
plot1+ 
# Plot the confidence interval for z=-3.
geom_segment(aes(x=-3,xend=-3,y=upper[1],yend=lower[1]),color="darkblue")+
# Plot the confidence interval for z=-2.
geom_segment(aes(x=-2,xend=-2,y=upper[2],yend=lower[2]),color="darkblue")+
# Plot the confidence interval for z=-1.
geom_segment(aes(x=-1,xend=-1,y=upper[3],yend=lower[3]),color="darkblue")+
# Plot the confidence interval for z=1.
geom_segment(aes(x=1,xend=1,y=upper[4],yend=lower[4]),color="darkblue")+
# Plot the confidence interval for z=2.
geom_segment(aes(x=2,xend=2,y=upper[5],yend=lower[5]),color="darkblue")+
# Plot the confidence interval for z=3.
geom_segment(aes(x=3,xend=3,y=upper[6],yend=lower[6]),color="darkblue")+
scale_y_continuous(limits=c(-12,22), # Set the y-limits.
				   breaks=c(-10,-5,0,5,10,15,20), # Set the y-axis breaks. 
				   # Set the y-axis tick labels at the breaks. 
				   labels=c("-10%","-5%","0%","5%","10%","15%","20%"))
















# We will now turn to the data from Africa. We will subset to these country pairs.
all_pairs<-read.csv("All_Pairs_Regional_9_25.csv",stringsAsFactors = F)

all_pairs_africa<-all_pairs[all_pairs$Event=="Africa",]


# Total number of group stage games
sum(all_pairs_africa$Group_Stage==1)

# Total number of knockout stage games
sum(all_pairs_africa$Knockout_Stage==1)

# Percentage of teams that were not randomized to play in the
# group stage but met in the knockout stage
mean(all_pairs_africa$Group_Stage==0&all_pairs_africa$Knockout_Stage==1)

# Cases where countries played in both the group and knockout stages
all_pairs_africa[all_pairs_africa$Group_Stage==1&all_pairs_africa$Knockout_Stage==1,]

# Subset to pairs of countries that could have played in the group stage.
sample<-all_pairs_africa[all_pairs_africa$Could_Play_GS==1,]

# Print the total number of these dyads.
dim(sample)

# Number of these pairs that we have trade data on
sum(is.na(sample$Bi_Trade_Pre)==F &
    is.na(sample$Bi_Trade_Post)==F)

# Percentage of these pairs that we have trade data on
mean(is.na(sample$Bi_Trade_Pre)==F &
    is.na(sample$Bi_Trade_Post)==F)

# Drop the cases where we are missing the bilateral trade data.
sample<-sample[is.na(sample$Bi_Trade_Pre)==F &
               is.na(sample$Bi_Trade_Post)==F,]

sample<-sample[sample$Year==1998|sample$Year>2000,]

#### DYAD-LEVEL ANALYSIS

# View sample size
nrow(sample)

# View number of pairs of countries that played in the group stage
sum(sample$Group_Stage==1)

# View number of pairs of countries that could have played in the 
# group stage but did not
sum(sample$Group_Stage==0)

# Number of different countries in the sample
length(unique(c(sample$Country1,sample$Country2)))

# View the different countries in alphabetical order
sort(unique(c(sample$Country1,sample$Country2)))

# View the temporal span of the data
range(sample$Year)

# View the span of time that the data covers
range(sample$Year)[2]-range(sample$Year)[1]



# One and two-sided p-value from randomization inference

# One-tailed p-value for adjusted change in ln(trade)--gravity model approach--all dyads
mean(perm_data$ln_Trade_Change_Adjusted<=real_data$ln_Trade_Change_Adjusted)

# Two-tailed p-value for adjusted change in ln(trade)--gravity model approach--all dyads
mean(abs(perm_data$ln_Trade_Change_Adjusted-
           mean(perm_data$ln_Trade_Change_Adjusted))>=abs(real_data$ln_Trade_Change_Adjusted-
                                                            mean(perm_data$ln_Trade_Change_Adjusted)))

# One-tailed p-value for adjusted change in ln(trade)--gravity model approach--soccer dyads
mean(perm_data$Soccer_ln_Trade_Change_Adjusted<=
       real_data$Soccer_ln_Trade_Change_Adjusted)

# Two-tailed p-value for adjusted change in ln(trade)--gravity model approach--soccer dyads
mean(abs(perm_data$Soccer_ln_Trade_Change_Adjusted-
           mean(perm_data$Soccer_ln_Trade_Change_Adjusted))>=
       abs(real_data$Soccer_ln_Trade_Change_Adjusted-
             mean(perm_data$Soccer_ln_Trade_Change_Adjusted)))


# One-tailed p-value for adjusted percentage change in trade--all dyads
mean(perm_data$Percent_Change_Adjusted<=real_data$Percent_Change_Adjusted)

# Two-tailed p-value for adjusted percentage change in trade--all dyads
mean(abs(perm_data$Percent_Change_Adjusted-
           mean(perm_data$Percent_Change_Adjusted))>=abs(real_data$Percent_Change_Adjusted-
                                                           mean(perm_data$Percent_Change_Adjusted)))

# One-tailed p-value for adjusted percentage change in trade--soccer dyads
mean(perm_data$Soccer_Percent_Change_Adjusted<=
       real_data$Soccer_Percent_Change_Adjusted)

# Two-tailed p-value for adjusted percentage change in trade--soccer dyads
mean(abs(perm_data$Soccer_Percent_Change_Adjusted-
           mean(perm_data$Soccer_Percent_Change_Adjusted))>=
       abs(real_data$Soccer_Percent_Change_Adjusted-
             mean(perm_data$Soccer_Percent_Change_Adjusted)))

# One-tailed p-value for probability of trade drop--all dyads
mean(perm_data$y>=real_data$y)

# Two-tailed p-value for probability of trade drop--all dyads
mean(abs(perm_data$y-
         mean(perm_data$y))>=(real_data$y-
                              mean(perm_data$y)))


# One-tailed p-value for probability of trade drop--soccer dyads
mean(perm_data$Soccer_y>=real_data$Soccer_y)

# Two-tailed p-value for probability of trade drop--soccer dyads
mean(abs(perm_data$Soccer_y-
     mean(perm_data$Soccer_y))>=(real_data$Soccer_y-
                                 mean(perm_data$Soccer_y)))







# Predict missing GDP values.
source("PredictMissingGDP.R")


# Regression models

# All country dyads--Gravity model
model_v1<-lm(ln_Adjusted_Trade-ln_Bi_Trade_Pre ~ 
               Group_Stage + ln_Dist + ln_Country1_GDP +
               ln_Country2_GDP + Both_GATT + Both_EU +
               Both_Dem  + Contiguous + Colony + Sibling +
               Alliance_Year_Before + Any_Disputes_Before+
               as.factor(Year), sample)

# View the results.
summary(model_v1)



# All country dyads--percentage change in trade

model_v3<-lm(Percent_Change_Adjusted ~ Group_Stage + ln_Dist + ln_Country1_GDP +
               ln_Country2_GDP + Both_GATT + Both_EU +
               Both_Dem +  Contiguous + Colony + Sibling +
               Alliance_Year_Before + Any_Disputes_Before+
               as.factor(Year), sample)

# View the results
summary(model_v3)






# All country dyads--probability of drop in trade

model_v5<-lm(y ~ Group_Stage + ln_Dist + ln_Country1_GDP +
               ln_Country2_GDP + Both_GATT + Both_EU +
               Both_Dem + Contiguous + Colony + Sibling +
               Alliance_Year_Before + Any_Disputes_Before+
               as.factor(Year), sample)

# View the results
summary(model_v5)








# Logit results for probability of drop in trade

# All dyads

model_v1<-glm(y~Group_Stage + ln_Dist + ln_Country1_GDP +
             ln_Country2_GDP + Both_GATT + Both_EU +
             Both_Dem + Contiguous + Colony + Sibling +
             Alliance_Year_Before + Any_Disputes_Before+
             as.factor(Year),family="binomial",sample)

# View the results
summary(model_v1)

# Calculate the one-tailed p-value.
0.036322/2





# Probit for probability of drop in trade

# All dyads

model_v1<-glm(y~Group_Stage + ln_Dist + ln_Country1_GDP +
              ln_Country2_GDP + Both_GATT + Both_EU +
              Both_Dem + Contiguous + Colony + Sibling +
              Alliance_Year_Before + Any_Disputes_Before+
              as.factor(Year),
            family=binomial(link="probit"),sample)


# View the results
summary(model_v1)

# Calculate the one-tailed p-value.
0.040683/2












# Winners vs. Losers

# Create a data set with just the countries that played each other
# in the group stage or knockout stage.
games<-all_pairs[(all_pairs$Group_Stage==1|
                 all_pairs$Knockout_Stage==1)&
                 all_pairs$Event=="Africa",]

# Calculate goal difference in each game (knockout stage only).
games$Z<-games$KS_Score1-games$KS_Score2

# For all dyads that did not play in the knockout stage, we will
# use the score from their group stage game.

# Calculate goal difference for countries that played
# in the group stage and not in the knockout stage and set
# this value as Z for these dyads.
games$Z[is.na(games$Z)==T]<-(games$GS_Score1-games$GS_Score2)[is.na(games$Z)==T]

# Create a data frame called d1 that primarily has the information for Country 1 in each row.
d1<-games[,c("Country1","Country2","Year","Irst1", "Milex1", "Milper1","Tpop1","Upop1",
             "Democracy1","Country1_GATT","Country1_EU","IndependenceYear1","ln_Country1_GDP",
             "ln_Country2_GDP","Both_Dem","Both_NonDem", "Diff_Regime", "Contiguous",
             "Total_Disputes_Before","Any_Disputes_Before","Alliance_Year_Before","Dist",
             "ln_Dist","Group_Stage","Knockout_Stage","Imports1_Pre11","Imports1_Pre10",
             "Imports1_Pre9","Imports1_Pre8","Imports1_Pre7","Imports1_Pre6","Imports1_Pre5",
             "Imports1_Pre4","Imports1_Pre3","Imports1_Pre2","Imports1_Pre","Imports1",
             "Imports1_Post1","Imports1_Post2","Imports1_Post3","Imports1_Post4","Imports1_Post5",
             "Imports1_Post6","Imports1_Post7","Imports1_Post8","Imports1_Post9","Imports1_Post10",
             "ln_Dist","Common_Religion","Colony","Sibling","Both_GATT","One_GATT","Both_EU",
             "One_EU", "Could_Play_GS","y1","Z","Prediction1","y","y1m1",
             "Played_Before_Last_10_Years")]

# Create a column called win that is all NAs.
d1$Win<-NA

# Set the win column value to 1 for rows where the score is not 0 (since
# Country 1 won these games).
d1$Win[d1$Z!=0]<-1             

# Create a data frame called d2 that primarily has the information for Country 2 in each row.
d2<-games[,c("Country2","Country1","Year","Irst2", "Milex2", "Milper2","Tpop2","Upop2",
             "Democracy2","Country2_GATT","Country2_EU","IndependenceYear2","ln_Country2_GDP",
             "ln_Country1_GDP","Both_Dem","Both_NonDem", "Diff_Regime","Contiguous",
             "Total_Disputes_Before", "Any_Disputes_Before","Alliance_Year_Before","Dist",
             "ln_Dist","Group_Stage","Knockout_Stage","Imports2_Pre11","Imports2_Pre10",
             "Imports2_Pre9","Imports2_Pre8","Imports2_Pre7","Imports2_Pre6","Imports2_Pre5",
             "Imports2_Pre4","Imports2_Pre3","Imports2_Pre2","Imports2_Pre","Imports2",
             "Imports2_Post1","Imports2_Post2","Imports2_Post3","Imports2_Post4","Imports2_Post5",
             "Imports2_Post6","Imports2_Post7","Imports2_Post8","Imports2_Post9","Imports2_Post10",
             "ln_Dist","Common_Religion","Colony","Sibling","Both_GATT","One_GATT","Both_EU",
             "One_EU","Could_Play_GS","y2","Z","Prediction2","y","y2m1",
             "Played_Before_Last_10_Years")]

# Create a column called win that is all NAs.
d2$Win<-NA

# Set the win column value to 0 for rows where the score is not 0 (since
# Country 2 lost these games).
d2$Win[d1$Z!=0]<-0                

# Make the score differential negative for d2 (since Country 2
# lost these games unless the goal difference was 0, in which
# case the goal difference will remain 0).
d2$Z<- -d2$Z

# Set the d2 column names to be the same as the d1 column names so that
# we can merge the data sets (note that this will switch Country 2 to 
# "Country 1" in d2).
colnames(d2)<-colnames(d1)

# Merge d1 and d2 into a single data frame.
combined<-rbind(d1,d2)

# Create a column that records whether the main country in each row
# (now called "Country 1" for all rows) lost. 
combined$Loss<- 1-combined$Win

# Set the max/min for a change in imports at +/-20%.
max<-0.2

# Create a column for the percent change in trade.
combined$Percent_Change_Adjusted<-(combined$Imports1-
                                   combined$Imports1_Pre)/combined$Imports1_Pre

# Set this value at 0 for cases where Imports1 and Imports1_Pre are both 0 
# (meaning that Country 1 had no imports from Country 2 in the World Cup year
# or the previous year).
combined$Percent_Change_Adjusted[combined$Imports1==0 &
								 combined$Imports1_Pre==0 &
								 is.na(combined$Imports1)==F &
								 is.na(combined$Imports1_Pre)==F]<-0

# When this value is greater than 20%, cap it at 20%.
combined$Percent_Change_Adjusted[combined$Percent_Change_Adjusted>max&
                                 is.na(combined$Percent_Change_Adjusted)==F]<-max

# When this value is less than -20%, cap it at -20%.
combined$Percent_Change_Adjusted[combined$Percent_Change_Adjusted<(-max)&
                                 is.na(combined$Percent_Change_Adjusted)==F]<- -max

# Add an adjusted imports column that is the value of imports capped at a +/-20%
# swing from the prior year.
combined$Adjusted_Imports<-(combined$Percent_Change_Adjusted+1)*combined$Imports1_Pre

# Add a column for ln(trade) for this new value.
combined$ln_Adjusted_Imports<-log(combined$Adjusted_Imports+1)

# Add a column for ln(imports) for the year before the World Cup.
combined$ln_Imports_Pre<-log(combined$Imports1_Pre+1)

# Add a column for the change in adjusted ln(imports).
combined$Change_ln_Adjusted_Imports<-combined$ln_Adjusted_Imports-
                                     combined$ln_Imports_Pre
                                     
# For a placebo outcome, we will repeat 
combined$Percent_Change_Adjusted_Prev<-(combined$Imports1_Pre-
                                        combined$Imports1_Pre2)/combined$Imports1_Pre2


combined$Percent_Change_Adjusted_Prev[combined$Percent_Change_Adjusted_Prev>max&
                                      is.na(combined$Percent_Change_Adjusted_Prev)==F]<-max


combined$Percent_Change_Adjusted_Prev[combined$Percent_Change_Adjusted_Prev<(-max)&
                                      is.na(combined$Percent_Change_Adjusted_Prev)==F]<- -max


combined$Adjusted_Imports_Prev<-(combined$Percent_Change_Adjusted_Prev+1)*combined$Imports1_Pre2
combined$ln_Adjusted_Imports_Prev<-log(combined$Adjusted_Imports_Prev+1)
combined$ln_Imports_Pre2<-log(combined$Imports1_Pre2+1)

combined$Change_ln_Adjusted_Imports_Prev<-combined$ln_Adjusted_Imports_Prev-
                                          combined$ln_Imports_Pre2                                     

# Fll missing GDP values using earlier predictions.
source("FillMissingGDP.R")

# Create a new data frame that excludes all the ties.
no_ties<-combined[combined$Z!=0,]

# Create a new data frame with just the ties.
ties<-combined[combined$Z==0,]

# Do a t-test for the countries that won or lost
# by one point.
t.test(Percent_Change_Adjusted~Loss,
       no_ties[abs(no_ties$Z)<=1,])

# Calculate the estimated treatment effect,
# based on the t-test results.
0.05882096-0.03840755


# Since who won and lost the close games was not really random,
# we will use local linear regression to try to adjust for small
# differences between the bare winners and bare losers.

# RDD Estimates

# Create the model for change in ln(imports) with controls.
model<-lm(ln_Adjusted_Imports-ln_Imports_Pre~Loss+Z+I(Loss*Z)+
          ln_Dist + ln_Country1_GDP + ln_Country2_GDP + 
            Both_GATT + Both_EU + Both_Dem +  
          Contiguous + Colony + Sibling + 
          Alliance_Year_Before+Any_Disputes_Before+
          as.factor(Year), no_ties[abs(no_ties$Z)<=3,])

# View the results.
summary(model)


# Create the model for change in ln(imports) without controls.
model<-lm(ln_Adjusted_Imports-ln_Imports_Pre~Loss+Z+I(Loss*Z),
          no_ties[abs(no_ties$Z)<=3,])

# View the results.
summary(model)



# Calculate the sample size for the two tests above.
with(no_ties[abs(no_ties$Z)<=3,], 
     sum(is.na(ln_Adjusted_Imports-ln_Imports_Pre)==F))



# We will now focus on the percentage change in imports.

# Create the model for change in percentage change in imports
# variable, with controls.

model<-lm(Percent_Change_Adjusted~Loss+Z+I(Loss*Z)+
            ln_Dist + ln_Country1_GDP + ln_Country2_GDP + 
            Both_GATT + Both_EU + Both_Dem +  
            Contiguous + Colony + Sibling + 
            Alliance_Year_Before+Any_Disputes_Before+
            as.factor(Year), no_ties[abs(no_ties$Z)<=3,])

# View the results.
summary(model)



# We will now create the model for change in percentage change in imports
# variable, without controls.

model<-lm(Percent_Change_Adjusted~Loss+Z+I(Loss*Z),
          no_ties[abs(no_ties$Z)<=3,])

# View the results.
summary(model)


# Calculate the sample size for the two tests above.
with(no_ties[abs(no_ties$Z)<=3,], 
     sum(is.na(Percent_Change_Adjusted)==F))







# We will now focus on the drop in trade outcome.

# Create the model for the drop in trade, with controls.

model<-lm(y1~Loss+Z+I(Loss*Z)+
            ln_Dist + ln_Country1_GDP + ln_Country2_GDP +
            Both_GATT + Both_EU + Both_Dem +  
            Contiguous + Colony + Sibling + 
            Alliance_Year_Before+Any_Disputes_Before+
            as.factor(Year), no_ties[abs(no_ties$Z)<=3,])

# View the results.
summary(model)


# We will now do the model without control variables.
model<-lm(y1~Loss+Z+I(Loss*Z),
          no_ties[abs(no_ties$Z)<=3,])

# View the results.
summary(model)




# Calculate the sample size for the two tests above.
with(no_ties[abs(no_ties$Z)<=3,], 
     sum(is.na(y1)==F))









            
# We will now create the RD graph with confidence intervals.

# Create a vector for win/loss differential: -3, -2, -1, 1, 2, 3
# We ignore ties here, as well as games that were decided by 4 points
# or more.

zs<-c(-3:-1,1:3)


# Create a regression model for the winners called mod1.
mod1<-lm(Percent_Change_Adjusted*100~Z,no_ties[no_ties$Z%in%1:3,])

# Store the intercept of this regression model as a1.
a1<-as.numeric(mod1$coefficients[1])

# Store the slope of this regression model as b1.
b1<-as.numeric(mod1$coefficients[2])


# Create a regression model for the losers called mod1.
mod2<-lm(Percent_Change_Adjusted*100~Z,no_ties[no_ties$Z%in%(-1:-3),])

# Store the intercept of this regression model as a1.
a2<-as.numeric(mod2$coefficients[1])

# Store the slope of this regression model as b2.
b2<-as.numeric(mod2$coefficients[2])

# Use tapply to calculate and store the average outcome 
# (Perecent_Change_Adjusted) for each value in the vector zs.

avgs<-with(no_ties[abs(no_ties$Z)<=3,],
     tapply(Percent_Change_Adjusted,Z,
            mean,na.rm=T)*100
)

# Create a data frame that contains the values in zs
# as one column and the corresponding average outcome
# for each value in zs as a second column.

x<-data.frame(zs,avgs)

# Create a ggplot called plot1.
plot1<-ggplot(x,aes(x=zs,y=avgs))+ # Set the x values as zs and the
								   # y values as the average outcomes
								   # for each value in zs.
geom_point(color="darkblue")+ # Set dark blue points.
theme_classic()+ # Set the theme.
xlim(-3.1,3.1)+xlab("Points from Winning")+ # Set the x-limits and x-axis label.
ylab("Percentage Change in Imports")+ # Set the y-axis label.
geom_vline(xintercept=0)+ # Draw a vertical line at x=0
geom_segment(aes(x=0,y=a1,xend=3.1,yend=a1+b1*3.1),
             color="cornflowerblue") + # Add the regression line for the winners.
geom_segment(aes(x=0,y=a2,xend=-3.1,yend=a2+b2*-3.1),
             color="cornflowerblue") # Add the regression line for the losers.
       
# We will now add confidence intervals to plot1.       

# Create a vector to store the upper bounds of the confidence intervals.
upper<-rep(NA,6)

# Create another vector to store the lower bounds of the confidence intervals.
lower<-rep(NA,6)

# Start a for loop to calculate and store the upper and lower 
# bounds for each value in zs.
for(i in 1:length(zs)){

# Calculte the standard error for each value in zs.
# Note here thatwe are using the formula SE=sigma/sqrt(n).
	
  se<-sd(no_ties$Percent_Change_Adjusted[no_ties$Z==zs[i]]*100,na.rm=T)/
    sqrt(sum(is.na(no_ties$Percent_Change_Adjusted[no_ties$Z==zs[i]]==F)))

# Calculate and store the upper bound of the confidence interval for
# each value in zs.    
  upper[i]<-avgs[i]+1.96*se
  
# Calculate and store the lower bound of the confidence interval for
# each value in zs.    
  lower[i]<-avgs[i]-1.96*se

# End the for loop.
}


# We will now generate plot1 with the confidence intervals for each value in zs.

# Start the plot.
plot1+ 
# Plot the confidence interval for z=-3.
geom_segment(aes(x=-3,xend=-3,y=upper[1],yend=lower[1]),color="darkblue")+
# Plot the confidence interval for z=-2.
geom_segment(aes(x=-2,xend=-2,y=upper[2],yend=lower[2]),color="darkblue")+
# Plot the confidence interval for z=-1.
geom_segment(aes(x=-1,xend=-1,y=upper[3],yend=lower[3]),color="darkblue")+
# Plot the confidence interval for z=1.
geom_segment(aes(x=1,xend=1,y=upper[4],yend=lower[4]),color="darkblue")+
# Plot the confidence interval for z=2.
geom_segment(aes(x=2,xend=2,y=upper[5],yend=lower[5]),color="darkblue")+
# Plot the confidence interval for z=3.
geom_segment(aes(x=3,xend=3,y=upper[6],yend=lower[6]),color="darkblue")+
scale_y_continuous(limits=c(-12,22), # Set the y-limits.
				   breaks=c(-10,-5,0,5,10,15,20), # Set the y-axis breaks. 
				   # Set the y-axis tick labels at the breaks. 
				   labels=c("-10%","-5%","0%","5%","10%","15%","20%"))
























