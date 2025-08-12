# This file reproduces the main results from the paper. Before running
# this code, it is first necessary to construct the datasets by running
# BuildData.R and WCSimulation.R.

# We will start by loading the following packages.
require(foreign)
require(ggplot2)
require(cowplot)
require(rdrobust)

# We will next set the working directory to the folder with the replication
# materials from the "Nationalism_Trade_Replication" repository from Github
setwd("~/Downloads/Nationalism_Trade_Replication-main")


# We will next bring the following datasets into the workspace.

# The dataset with pairs of countries across different World Cup years.
all_pairs<-read.csv("All_Pairs_July25.csv", stringsAsFactors=F)

# The dataset with various statistics about the pairs of countries 
# that played in the World Cup group stage.
real_data<-read.csv("RealData_July24.csv",stringsAsFactors=F)

# The dataset with the country pairs that played in the World Cup group
# stage.
real_data_full<-read.csv("RealDataFull_July24.csv",stringsAsFactors=F)

# The dataset with the results from the simulations that involved
# rerandomizing the World Cup groups from 1930 to 2018.
perm_data<-read.csv("PermData_July25.csv",stringsAsFactors=F)


# Total number of group stage games
sum(all_pairs$Group_Stage==1)

# Total number of knockout stage games
sum(all_pairs$Knockout_Stage==1)

# Percentage of teams that were not randomized to play in the
# group stage but met in the knockout stage
mean(all_pairs$Group_Stage==0&all_pairs$Knockout_Stage==1)

# Cases where countries played in both the group and knockout stages
all_pairs[all_pairs$Group_Stage==1&all_pairs$Knockout_Stage==1,]

# Subset to pairs of countries that could have played in the group stage.
sample<-all_pairs[all_pairs$Could_Play_GS==1,]

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



#### DYAD-LEVEL ANALYSIS

# View sample size
nrow(sample)

# View number of pairs of countries that played in the group stage
sum(sample$Group_Stage==1)

# View number of pairs of countries that could have played in the 
# group stage but did not
sum(sample$Group_Stage==0)

# View number of pairs where soccer was the most popular sport for
# both sides
sum(sample$Both_Soccer==1)

# Demonstrating measurment error in data (Angola-Croatia 2006)
sample[2434,]
(0.4975291/0.000335-1)*100   

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

# Calculate the standardized estimated effect.
-0.010263/sd(sample$ln_Adjusted_Trade-sample$ln_Bi_Trade_Pre)

# Calculate the one-tailed p-value.
0.091871/2

# Calculate the one-tailed standard error.
0.006087*1.65/1.96


# Soccer dyads--Gravity model

model_v2<-lm(ln_Adjusted_Trade-ln_Bi_Trade_Pre ~ 
               Group_Stage + ln_Dist + ln_Country1_GDP +
               ln_Country2_GDP + Both_GATT + Both_EU +
               Both_Dem  + Contiguous + Colony + Sibling +
               Alliance_Year_Before + Any_Disputes_Before+
               as.factor(Year), sample[sample$Both_Soccer==1,])

# View the results
summary(model_v2)

# Calculate the standardized estimated effect
-0.0134348/sd((sample$ln_Adjusted_Trade-sample$ln_Bi_Trade_Pre)[sample$Both_Soccer==1])

# Calculate the one-tailed p-value.
0.041952/2

# Calculate the one-tailed standard error.
0.0066024*1.65/1.96


# All country dyads--percentage change in trade

model_v3<-lm(Percent_Change_Adjusted ~ Group_Stage + ln_Dist + ln_Country1_GDP +
               ln_Country2_GDP + Both_GATT + Both_EU +
               Both_Dem +  Contiguous + Colony + Sibling +
               Alliance_Year_Before + Any_Disputes_Before+
               as.factor(Year), sample)

# View the results
summary(model_v3)

# Calculate the standardized estimated effect
-0.0115638/sd(sample$Percent_Change_Adjusted)

# Calculate the one-tailed p-value.
0.080999/2

# Calculate the one-tailed standard error.
0.0066253*1.65/1.96




# Soccer dyads--percentage change in trade

model_v4<-lm(Percent_Change_Adjusted ~ Group_Stage + ln_Dist + ln_Country1_GDP +
               ln_Country2_GDP + Both_GATT + Both_EU +
               Both_Dem + Contiguous + Colony + Sibling +
               Alliance_Year_Before + Any_Disputes_Before+
               as.factor(Year), sample[sample$Both_Soccer==1,])

# View the results
summary(model_v4)

# Calculate the standardized estimated effect
-0.014225/sd(sample$Percent_Change_Adjusted[sample$Both_Soccer==1])

# Calculate the one-tailed p-value.
0.048853/2

# Calculate the one-tailed standard error.
0.007218*1.65/1.96





# All country dyads--probability of drop in trade

model_v5<-lm(y ~ Group_Stage + ln_Dist + ln_Country1_GDP +
               ln_Country2_GDP + Both_GATT + Both_EU +
               Both_Dem + Contiguous + Colony + Sibling +
               Alliance_Year_Before + Any_Disputes_Before+
               as.factor(Year), sample)

# View the results
summary(model_v5)

# Calculate the standardized estimated effect
0.048781/sd(sample$y)

# Calculate the one-tailed p-value.
0.027126/2

# Calculate the one-tailed standard error.
0.022067*1.65/1.96


# Soccer dyads--probability of drop in trade

model_v6<-lm(y ~ Group_Stage + ln_Dist + ln_Country1_GDP +
               ln_Country2_GDP + Both_GATT + Both_EU +
               Both_Dem + Contiguous + Colony + Sibling +
               Alliance_Year_Before + Any_Disputes_Before+
               as.factor(Year), sample[sample$Both_Soccer==1,])

# View the results
summary(model_v6)

# Calculate the standardized estimated effect
0.063950/sd(sample$y[sample$Both_Soccer==1])

# Calculate the one-tailed p-value.
0.007326/2

# Calculate the one-tailed standard error.
0.023831*1.65/1.96



# Calculate the number of soccer dyads.
sum(is.na(sample$y[sample$Both_Soccer==1])==F)





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
0.026752/2

# Soccer dyads

model_v2<-glm(y~Group_Stage + ln_Dist + ln_Country1_GDP +
             ln_Country2_GDP + Both_GATT + Both_EU +
             Both_Dem + Contiguous + Colony + Sibling +
             Alliance_Year_Before + Any_Disputes_Before+
             as.factor(Year),family="binomial",
           sample[sample$Both_Soccer==1,])

# View the results
summary(model_v2)

# Calculate the one-tailed p-value.
0.007119/2





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
0.029778/2

# Soccer dyads

model_v2<-glm(y~Group_Stage + ln_Dist + ln_Country1_GDP +
              ln_Country2_GDP + Both_GATT + Both_EU +
              Both_Dem + Contiguous + Colony + Sibling +
              Alliance_Year_Before + Any_Disputes_Before+
              as.factor(Year),
            family=binomial(link="probit"),
            sample[sample$Both_Soccer==1,])

# View the results
summary(model_v2)

# Calculate the one-tailed p-value.
0.007893/2









# Comparing the estimated effect to the estimated effect of
# the Muhammad cartoon controversy--19% compared to 1.2%

0.19*0.25*0.25

# So if World Cup nationalism caused a drop in trade about 25%
# the size as the drop in trade from the Muhammad cartoon controversy
# in about 25% of cases, it would match our estimated treatment
# effect pretty closely.





# Placebo test on previous outcome--gravity model approach

# All dyads
model_v1<-lm(ln_Adjusted_Trade_Prev-ln_Bi_Trade_M2~Group_Stage + 
             ln_Dist + ln_Country1_GDP +
             ln_Country2_GDP + Both_GATT + Both_EU +
             Both_Dem + Contiguous + Colony + Sibling +
             Alliance_Year_Before + Any_Disputes_Before+
             as.factor(Year), sample)

# View the results
summary(model_v1)



# Soccer dyads

model_v2<-lm(ln_Adjusted_Trade_Prev-ln_Bi_Trade_M2~Group_Stage + 
               ln_Dist + ln_Country1_GDP +
               ln_Country2_GDP + Both_GATT + Both_EU +
               Both_Dem + Contiguous + Colony + Sibling +
               Alliance_Year_Before + Any_Disputes_Before+
               as.factor(Year), sample[sample$Both_Soccer==1,])
               
# View the results.
summary(model_v2)





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

# Create a model with this new variable as the outcome.
  model<-lm(Percent_Change_Shift~Group_Stage + ln_Dist + 
              Both_GATT + Both_EU + Both_Dem  + Contiguous +
              Colony + Sibling + Alliance_Year_Before +
              Any_Disputes_Before + as.factor(Year), sample)
              
# Create a second model that focuses on the soccer dyads.
  model2<-lm(Percent_Change_Shift~Group_Stage + ln_Dist + 
              Both_GATT + Both_EU + Both_Dem  + Contiguous +
              Colony + Sibling + Alliance_Year_Before +
              Any_Disputes_Before + as.factor(Year),
              sample[sample$Both_Soccer==1,])
              

# Print max for this round of the for loop.
  print(max)
  
# Print the one-tailed p-value for all dyads this round of the for loop.
  print(summary(model)$coefficients[2,4]/2)
  
# Print the one-tailed p-value for soccer dyads this round of the for loop.
  print(summary(model2)$coefficients[2,4]/2)    

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
  
  sample$Adjusted_Trade_Post<-(1+sample$Percent_Change_Shift)*sample$Bi_Trade_Pre

# Calculate the ln(trade) value given the adjustment.
  sample$ln_Adjusted_Trade_Post<-log(sample$Adjusted_Trade_Post+1)

# Create a model with this new variable as the outcome.
  model<-lm(ln_Adjusted_Trade_Post-ln_Bi_Trade_Pre~Group_Stage + ln_Dist + 
              Both_GATT + Both_EU + Both_Dem  + Contiguous +
              Colony + Sibling + Alliance_Year_Before +
              Any_Disputes_Before + as.factor(Year), sample)

# Create a second model that focuses on the soccer dyads.
  model2<-lm(ln_Adjusted_Trade_Post-ln_Bi_Trade_Pre~Group_Stage + ln_Dist + 
              Both_GATT + Both_EU + Both_Dem  + Contiguous +
              Colony + Sibling + Alliance_Year_Before +
              Any_Disputes_Before + as.factor(Year), 
            sample[sample$Both_Soccer==1,])
            
              
# Print max for this round of the for loop.
  print(max)
  
# Print the one-tailed p-value for all dyads this round of the for loop.
  print(summary(model)$coefficients[2,4]/2)
  
# Print the one-tailed p-value for soccer dyads this round of the for loop.
  print(summary(model2)$coefficients[2,4]/2)  

# End the for loop.  
}






# Winners vs. Losers

# Create a data set with just the countries that played each other
# in the group stage or knockout stage.
games<-all_pairs[all_pairs$Group_Stage==1|
                 all_pairs$Knockout_Stage==1,]

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
             "One_EU", "SoccerMostPopular1","Could_Play_GS","y1","Z","Prediction1","y","y1m1",
             "Same_Continent","Played_Before_Last_10_Years")]

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
             "One_EU","SoccerMostPopular2","Could_Play_GS","y2","Z","Prediction2","y","y2m1",
             "Same_Continent","Played_Before_Last_10_Years")]

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

# Do a t-test for the soccer countries that won or lost
# by one point.
t.test(Percent_Change_Adjusted~Loss,
       no_ties[abs(no_ties$Z)<=1&
       no_ties$SoccerMostPopular1==1,])

# Calculate the estimated treatment effect for the soccer countries,
# based on the t-test results.
0.05873270-0.03759185


# Since who won and lost the close games was not really random,
# we will use local linear regression to try to adjust for small
# differences between the bare winners and bare losers.

# We will start using the rdrobust package to 
# check for the optimal bandwidth.

# Change in ln(imports) for all countries.
with(no_ties,rdrobust(ln_Adjusted_Imports-ln_Imports_Pre,Z))

# Percent change in imports for all countries.
with(no_ties,rdrobust(Percent_Change_Adjusted,Z))

# Drop in imports for all countries.
with(no_ties,rdrobust(y1,Z))

# Change in ln(imports) for soccer countries.
with(no_ties[no_ties$SoccerMostPopular1==1,],
     rdrobust(ln_Adjusted_Imports-ln_Imports_Pre,Z))

# Percent change in imports for soccer countries.
with(no_ties[no_ties$SoccerMostPopular1==1,],
     rdrobust(Percent_Change_Adjusted,Z))

# Drop in imports for soccer countries.
with(no_ties[no_ties$SoccerMostPopular1==1,],
     rdrobust(y1,Z))

# So the optimal bandwidth is between 3 and 4 in all the above cases.
# We will therefore set the bandwidth at <= 3.


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

# Calculate the one-tailed p-value.
0.007310/2

# Calculate the one-tailed standard error.
0.018669*1.65/1.96


# Create the model for change in ln(imports) without controls.
model<-lm(ln_Adjusted_Imports-ln_Imports_Pre~Loss+Z+I(Loss*Z),
          no_ties[abs(no_ties$Z)<=3,])

# View the results.
summary(model)

# Calculate the one-tailed p-value.
0.010590/2

# Calculate the one-tailed standard error.
0.019858*1.65/1.96


# Calculate the sample size for the two tests above.
with(no_ties[abs(no_ties$Z)<=3,], 
     sum(is.na(ln_Adjusted_Imports-ln_Imports_Pre)==F))



# Repeat the tests above, but for soccer countries.
model<-lm(ln_Adjusted_Imports-ln_Imports_Pre~Loss+Z+I(Loss*Z)+
            ln_Dist + ln_Country1_GDP + ln_Country2_GDP + 
            Both_GATT + Both_EU + Both_Dem +  
            Contiguous + Colony + Sibling + 
            Alliance_Year_Before+Any_Disputes_Before+
            as.factor(Year), no_ties[abs(no_ties$Z)<=3&
                                     no_ties$SoccerMostPopular1==1,])

# View the results.
summary(model)

# Calculate the one-tailed p-value.
0.007181/2

# Calculate the one-tailed standard error.
0.019298*1.65/1.96


# We will now do the model without control variables.
model<-lm(ln_Adjusted_Imports-ln_Imports_Pre~Loss+Z+I(Loss*Z),
          no_ties[abs(no_ties$Z)<=3&
                  no_ties$SoccerMostPopular1==1,])

# View the results.
summary(model)

# Calculate the one-tailed p-value.
0.007962/2

# Calculate the one-tailed standard error.
0.020525*1.65/1.96


# Calculate the sample size for the two tests above.
with(no_ties[abs(no_ties$Z)<=3&
             no_ties$SoccerMostPopular1==1,], 
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

# Calculate the one-tailed p-value.
0.00453/2

# Calculate the one-tailed standard error.
0.020958*1.65/1.96


# We will now create the model for change in percentage change in imports
# variable, without controls.

model<-lm(Percent_Change_Adjusted~Loss+Z+I(Loss*Z),
          no_ties[abs(no_ties$Z)<=3,])

# View the results.
summary(model)

# Calculate the one-tailed p-value.
0.006538/2

# Calculate the one-tailed standard error.
0.022053*1.65/1.96

# Calculate the sample size for the two tests above.
with(no_ties[abs(no_ties$Z)<=3,], 
     sum(is.na(Percent_Change_Adjusted)==F))





# Repeat the tests above, but for soccer countries.

# Create the model with controls.
model<-lm(Percent_Change_Adjusted~Loss+Z+I(Loss*Z)+
            ln_Dist + ln_Country1_GDP + ln_Country2_GDP + 
            Both_GATT + Both_EU + Both_Dem +  
            Contiguous + Colony + Sibling + 
            Alliance_Year_Before+Any_Disputes_Before+
            as.factor(Year), no_ties[abs(no_ties$Z)<=3&
                                     no_ties$SoccerMostPopular1==1,])

# View the results.
summary(model)

# Calculate the one-tailed p-value.
0.00650/2

# Calculate the one-tailed standard error.
0.021676*1.65/1.96


# We will now do the model without control variables.
model<-lm(Percent_Change_Adjusted~Loss+Z+I(Loss*Z),
          no_ties[abs(no_ties$Z)<=3&
                    no_ties$SoccerMostPopular1==1,])

# View the results.
summary(model)

# Calculate the one-tailed p-value.
0.007557/2

# Calculate the one-tailed standard error.
0.022787*1.65/1.96




# Calculate the sample size for the two tests above.
with(no_ties[abs(no_ties$Z)<=3&
             no_ties$SoccerMostPopular1==1,], 
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

# Calculate the one-tailed p-value.
0.07818/2

# Calculate the one-tailed standard error.
0.068155*1.65/1.96


# We will now do the model without control variables.
model<-lm(y1~Loss+Z+I(Loss*Z),
          no_ties[abs(no_ties$Z)<=3,])

# View the results.
summary(model)

# Calculate the one-tailed p-value.
0.0905/2

# Calculate the one-tailed standard error.
0.07121*1.65/1.96



# Calculate the sample size for the two tests above.
with(no_ties[abs(no_ties$Z)<=3,], 
     sum(is.na(y1)==F))






# Repeat the tests above, but for soccer countries.

# Create the model with controls.
model<-lm(y1~Loss+Z+I(Loss*Z)+
            ln_Dist + ln_Country1_GDP + ln_Country2_GDP +
            Both_GATT + Both_EU + Both_Dem +  
            Contiguous + Colony + Sibling + 
            Alliance_Year_Before+Any_Disputes_Before+
            as.factor(Year), no_ties[abs(no_ties$Z)<=3&
                                       no_ties$SoccerMostPopular1==1,])

# View the results.
summary(model)

# Calculate the one-tailed p-value.
0.0420/2

# Calculate the one-tailed standard error.
0.069799*1.65/1.96





# Create the model without controls.
model<-lm(y1~Loss+Z+I(Loss*Z),
          no_ties[abs(no_ties$Z)<=3&
                    no_ties$SoccerMostPopular1==1,])

# View the results.
summary(model)

# Calculate the one-tailed p-value.
0.0454/2

# Calculate the one-tailed standard error.
0.07302*1.65/1.96


# Calculate the sample size for the two tests above.
with(no_ties[abs(no_ties$Z)<=3&
             no_ties$SoccerMostPopular1==1,], 
     sum(is.na(y1)==F))








            
# We will now create the RD graph with confidence intervals.

# Create a new data frame for countries where soccer is the most 
# popular sport.
soc<-no_ties[no_ties$SoccerMostPopular1==1,]


# Create a vector for win/loss differential: -3, -2, -1, 1, 2, 3
# We ignore ties here, as well as games that were decided by 4 points
# or more.

zs<-c(-3:-1,1:3)


# Create a regression model for the winners called mod1.
mod1<-lm(Percent_Change_Adjusted*100~Z,soc[soc$Z%in%1:3,])

# Store the intercept of this regression model as a1.
a1<-as.numeric(mod1$coefficients[1])

# Store the slope of this regression model as b1.
b1<-as.numeric(mod1$coefficients[2])


# Create a regression model for the losers called mod1.
mod2<-lm(Percent_Change_Adjusted*100~Z,soc[soc$Z%in%(-1:-3),])

# Store the intercept of this regression model as a1.
a2<-as.numeric(mod2$coefficients[1])

# Store the slope of this regression model as b2.
b2<-as.numeric(mod2$coefficients[2])

# Use tapply to calculate and store the average outcome 
# (Perecent_Change_Adjusted) for each value in the vector zs.

avgs<-with(soc[abs(soc$Z)<=3,],
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
xlim(-3.1,3.1)+ ylim(-5,12)+# Set the x-limits and y-limits.
xlab("Points from Winning")+ylab("Percentage Change in Imports")+ # Set the x 
																  # and y labels.
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
	
  se<-sd(soc$Percent_Change_Adjusted[soc$Z==zs[i]]*100,na.rm=T)/
    sqrt(sum(is.na(soc$Percent_Change_Adjusted[soc$Z==zs[i]]==F)))

# Calculate and store the upper bound of the confidence interval for
# each value in zs.    
  upper[i]<-avgs[i]+1.96*se
  
# Calculate and store the lower bound of the confidence interval for
# each value in zs.    
  lower[i]<-m-1.96*se

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
scale_y_continuous(limits=c(-10,20), # Set the y-limits.
				   breaks=c(-10,-5,0,5,10,15,20), # Set the y-axis breaks. 
				   # Set the y-axis tick labels at the breaks. 
				   labels=c("-10%","-5%","0%","5%","10%","15%","20%"))








model<-lm(ln_Adjusted_Imports-ln_Imports_Pre~Win+Z+I(Win*Z) + 
            Irst1 + Milex1 + Milper1 + Tpop1 + Upop1 + 
            Democracy1 + Country1_GATT + Country1_EU,
          no_ties[no_ties$SoccerMostPopular1==1&abs(no_ties$Z)<=3,])
summary(model)

model<-lm(Percent_Change_Adjusted~Win+Z+I(Win*Z) + Irst1 + 
            Milex1 + Milper1 + Tpop1 + Upop1 + Democracy1 + 
            Country1_GATT + Country1_EU,
          no_ties[no_ties$SoccerMostPopular1==1&
                  abs(no_ties$Z)<=3,])
summary(model)

model<-lm(y1~Win+Z+I(Win*Z) + Irst1 + Milex1 + Milper1 + 
            Tpop1 + Upop1 + Democracy1 + Country1_GATT + 
            Country1_EU,
          no_ties[no_ties$SoccerMostPopular1==1&
                  abs(no_ties$Z)<=3,])
summary(model)

















# Joint p-values

ps1<-runif(100000,0,1)
ps2<-runif(100000,0,1)

# Joint p-value for all countries
mean((ps1*ps2)<(0.004*0.046))

# Joint p-value for soccer countries
mean((ps1*ps2)<(0.003*0.021))



# Time series graph

dat<-sample

years<-c("Bi_Trade_M11","Bi_Trade_M10","Bi_Trade_M9",
         "Bi_Trade_M8","Bi_Trade_M7","Bi_Trade_M6",
         "Bi_Trade_M5","Bi_Trade_M4","Bi_Trade_M3",
         "Bi_Trade_M2","Bi_Trade_Pre",
         "Bi_Trade_Post", "Bi_Trade_P1", 
         "Bi_Trade_P2",
         "Bi_Trade_P3","Bi_Trade_P4",
         "Bi_Trade_P5",
         "Bi_Trade_P6","Bi_Trade_P7","Bi_Trade_P8",
         "Bi_Trade_P9","Bi_Trade_P10")


dat$ym10<-NA
dat$ym9<-NA
dat$ym8<-NA
dat$ym7<-NA
dat$ym6<-NA
dat$ym5<-NA
dat$ym4<-NA
dat$ym3<-NA
dat$ym2<-NA
dat$ym1<-NA
dat$y0<-NA
dat$yp1<-NA
dat$yp2<-NA
dat$yp3<-NA
dat$yp4<-NA
dat$yp5<-NA
dat$yp6<-NA
dat$yp7<-NA
dat$yp8<-NA
dat$yp9<-NA
dat$yp10<-NA


outcomes<-c("ym10","ym9","ym8","ym7",
            "ym6","ym5","ym4","ym3",
            "ym2","ym1","y0","yp1",
            "yp2","yp3","yp4","yp5",
            "yp6","yp7","yp8","yp9",
            "yp10")



for(i in 1:(length(years)-1)){
  prev_year<-dat[,"Bi_Trade_Pre"]
  that_year<-dat[,years[(i+1)]]
  pct_change<- (that_year-prev_year)/prev_year
  pct_change[is.na(pct_change)==F&pct_change< -0.2]<- -0.2
  pct_change[is.na(pct_change)==F&pct_change>0.2]<-0.2
  dat[,outcomes[i]]<-pct_change
}


soccer_countries<-dat[dat$Both_Soccer==1,]

values<-matrix(NA,nrow=length(outcomes),ncol=2)

for(i in 1:length(outcomes)){
  values[i,]<-summary(lm(soccer_countries[,outcomes[i]]~
                          soccer_countries$Group_Stage +
                          soccer_countries$ln_Dist +
                          soccer_countries$ln_Country1_GDP+
                          soccer_countries$ln_Country2_GDP+
                          soccer_countries$Both_GATT + 
                          soccer_countries$Both_EU +
                          soccer_countries$Both_Dem +  
                          soccer_countries$Contiguous + 
                          soccer_countries$Colony + 
                          soccer_countries$Sibling+ 
                          soccer_countries$Alliance_Year_Before + 
                          soccer_countries$Any_Disputes_Before + 
                          as.factor(soccer_countries$Year)))$coefficients[2,1:2]}


est<-matrix(0,nrow=nrow(values),ncol=5)

est[,1]<-values[,1]
est[,2]<-values[,1]+1.65*values[,2]
est[,3]<-values[,1]-1.65*values[,2]
est[,4]<-values[,1]+1.28*values[,2]
est[,5]<-values[,1]-1.28*values[,2]

colnames(est)<-c("Estimate","Upper95", "Lower95","Upper90", "Lower90")
est<-data.frame(est)
est
est$Year<- -10:10


theme_nolegend <- function (base_size = 9, base_family = "", height, width) 
{
  theme_grey(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.text = element_text(size = rel(0.8)), 
          legend.position="none", 
          axis.ticks = element_line(colour = "black"), 
          legend.key = element_rect(colour = "grey80"), 
          panel.background = element_rect(fill = "white", colour = NA), 
          panel.border = element_rect(fill = NA,colour = "grey50"), 
          panel.grid.major = element_line(colour = "grey90", size = 0.2), 
          panel.grid.minor = element_line(colour = "grey98", size = 0.5), 
          strip.background = element_rect(fill = "grey80",  colour = "grey50"), 
          strip.background = element_rect(fill = "grey80", colour = "grey50"))
}


# make the graph
library(ggplot2)

colors<-c(rep("gray58",10),"blue",rep("gray58",10))

f <- ggplot(est, 
            aes(x=Estimate,y=Year),color=colors, size=rel(1.5))
f <- f+geom_vline(xintercept=0, linetype="longdash")+
  
  geom_errorbarh(aes(xmax =  Upper95, 
                     xmin = Lower95),
                 size=0.75, height=0,color=colors)+
  geom_errorbarh(aes(xmax =  Upper90, 
                     xmin = Lower90),
                 size=1.5, height=0,color=colors)  + geom_point( 
                   aes(x=Estimate,y=Year), group=2,color=colors, size=rel(1.5))+# geom_path(color="gray58")+
  # xlab("Estimated Impact on Having\nLess Trade than in Year -1\n(in percentage points)")+ylab("")
  xlab("Estimated Difference in Trade\nCompared to Year -1")+ylab("")+ theme(legend.position="none",axis.text=element_text(size=10),axis.title=element_text(size=12),
                                                                             plot.title = element_text(lineheight=1.8,size=rel(1.5),face="bold")) +scale_y_continuous(breaks=-10:10,labels=c("-10","-9","-8","-7","-6","-5","-4","-3","-2","-1","World\nCup\nYear","1","2","3","4","5","6","7","8","9","10")) +
  # scale_x_continuous(limits=c(-.12,.12),breaks=seq(-0.12,0.12,0.02),labels=c("-12%","-10%","-8%","-6%","-4%","-2%","0%","2%","4%","6%","8%","10%","12%"))+
  scale_x_continuous(limits=c(-.03,.03),breaks=seq(-0.03,0.03,0.01),labels=c("-3%","-2%","-1%","0%","1%","2%","3%"))+
  coord_flip() + ylab("Years Before or After the World Cup Year") +theme_classic()


f

ggsave("WorldCupTradeTSb.pdf",width=7,height=3)




years<-c("Imports1_Pre11","Imports1_Pre10","Imports1_Pre9","Imports1_Pre8","Imports1_Pre7","Imports1_Pre6","Imports1_Pre5",
         "Imports1_Pre4", "Imports1_Pre3", "Imports1_Pre2","Imports1_Pre","Imports1","Imports1_Post1","Imports1_Post2",
         "Imports1_Post3","Imports1_Post4","Imports1_Post5","Imports1_Post6","Imports1_Post7","Imports1_Post8","Imports1_Post9","Imports1_Post10")

no_ties$ym10<-NA
no_ties$ym9<-NA
no_ties$ym8<-NA
no_ties$ym7<-NA
no_ties$ym6<-NA
no_ties$ym5<-NA
no_ties$ym4<-NA
no_ties$ym3<-NA
no_ties$ym2<-NA
no_ties$ym1<-NA
no_ties$y0<-NA
no_ties$yp1<-NA
no_ties$yp2<-NA
no_ties$yp3<-NA
no_ties$yp4<-NA
no_ties$yp5<-NA
no_ties$yp6<-NA
no_ties$yp7<-NA
no_ties$yp8<-NA
no_ties$yp9<-NA
no_ties$yp10<-NA


outcomes<-c("ym10","ym9","ym8","ym7","ym6","ym5","ym4","ym3","ym2","ym1","y0","yp1","yp2","yp3","yp4","yp5","yp6","yp7","yp8","yp9","yp10")

for(i in 1:(length(years)-1)){
  prev_year<-no_ties[,years[i]]
  that_year<-no_ties[,years[(i+1)]]
  pct_change<- (that_year-prev_year)/prev_year
  pct_change[is.na(pct_change)==F&pct_change< -0.2]<- -0.2
  pct_change[is.na(pct_change)==F&pct_change>0.2]<-0.2
  no_ties[,outcomes[i]]<-pct_change
}

for(i in 1:(length(years)-1)){
  prev_year<-no_ties[,years[11]]
  that_year<-no_ties[,years[(i+1)]]
  pct_change<- (that_year-prev_year)/prev_year
  pct_change[is.na(pct_change)==F&pct_change< -0.2]<- -0.2
  pct_change[is.na(pct_change)==F&pct_change>0.2]<-0.2
  no_ties[,outcomes[i]]<-pct_change
}

soccer_countries<-no_ties[abs(no_ties$Z)<=3&no_ties$SoccerMostPopular1==1,]

soccer_countries$Loss<-abs(soccer_countries$Win-1)

values<-matrix(NA,nrow=length(outcomes),ncol=2)

for(i in 1:length(outcomes)){
  values[i,]<-summary(lm(soccer_countries[,outcomes[i]]~
                          soccer_countries$Loss+
                          soccer_countries$Z+
                          I(soccer_countries$Loss*soccer_countries$Z)+
                          soccer_countries$ln_Dist + 
                          soccer_countries$Both_GATT + 
                          soccer_countries$Both_EU + 
                          soccer_countries$Both_Dem +  
                          soccer_countries$Contiguous + 
                          soccer_countries$Colony + 
                          soccer_countries$Sibling+ 
                          soccer_countries$Alliance_Year_Before + 
                          soccer_countries$Any_Disputes_Before + 
                          as.factor(soccer_countries$Year)))$coefficients[2,1:2]}

est<-matrix(0,nrow=nrow(values),ncol=5)

est[,1]<-values[,1]
est[,2]<-values[,1]+1.65*values[,2]
est[,3]<-values[,1]-1.65*values[,2]
est[,4]<-values[,1]+1.28*values[,2]
est[,5]<-values[,1]-1.28*values[,2]

colnames(est)<-c("Estimate","Upper95", "Lower95","Upper90", "Lower90")
est<-data.frame(est)
est
est$Year<- -10:10

theme_nolegend <- function (base_size = 9, base_family = "", height, width) 
{
  theme_grey(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.text = element_text(size = rel(0.8)), 
          legend.position="none", 
          axis.ticks = element_line(colour = "black"), 
          legend.key = element_rect(colour = "grey80"), 
          panel.background = element_rect(fill = "white", colour = NA), 
          panel.border = element_rect(fill = NA,colour = "grey50"), 
          panel.grid.major = element_line(colour = "grey90", size = 0.2), 
          panel.grid.minor = element_line(colour = "grey98", size = 0.5), 
          strip.background = element_rect(fill = "grey80",  colour = "grey50"), 
          strip.background = element_rect(fill = "grey80", colour = "grey50"))
}

colors<-c(rep("gray58",10),"blue",rep("gray58",10))

f <- ggplot(est, 
            aes(x=Estimate,y=Year),color=colors, size=rel(1.5))
f <- f+geom_vline(xintercept=0, linetype="longdash")+
  
  geom_errorbarh(aes(xmax =  Upper95, 
                     xmin = Lower95),
                 size=0.75, height=0,color=colors)+
  geom_errorbarh(aes(xmax =  Upper90, 
                     xmin = Lower90),
                 size=1.5, height=0,color=colors)  + geom_point( 
                   aes(x=Estimate,y=Year), group=2,color=colors, size=rel(1.5))+ 
  xlab("Estimated Difference in Imports\nCompared to Year -1")+ylab("")+ theme(legend.position="none",axis.text=element_text(size=9),axis.title=element_text(size=12),
                                                                               plot.title = element_text(lineheight=1.8,size=rel(1.5),face="bold")) +scale_y_continuous(breaks=-10:10,labels=c("-10","-9","-8","-7","-6","-5","-4","-3","-2","-1","World\nCup\nYear","1","2","3","4","5","6","7","8","9","10")) +
  scale_x_continuous(limits=c(-.11,.11),breaks=seq(-0.10,0.10,0.02),labels=c("-10%","-8%","-6%","-4%","-2%","0%","2%","4%","6%","8%","10%"))+
  coord_flip() + ylab("Years Before or After the World Cup Year")+theme_classic()


f

ggsave("WorldCupTradeTS2b.pdf",width=7,height=3)




# Check results for pairs of countries that were (and were not)
# from the same continent

model_v1<-lm(ln_Adjusted_Trade-ln_Bi_Trade_Pre ~ 
               Group_Stage + ln_Dist + ln_Country1_GDP +
               ln_Country2_GDP + Both_GATT + Both_EU +
               Both_Dem + Contiguous + Colony +
               Sibling + Alliance_Year_Before + Any_Disputes_Before+
               as.factor(Year), sample[sample$Both_Soccer==1&
                                         sample$Same_Continent==1,])

summary(model_v1)$coefficients[2,]

sum(sample$Both_Soccer==1&sample$Same_Continent==1)



model_v2<-lm(Percent_Change_Adjusted ~ 
               Group_Stage + ln_Dist + ln_Country1_GDP +
               ln_Country2_GDP + Both_GATT + Both_EU +
               Both_Dem + Contiguous + Colony +
               Sibling + Alliance_Year_Before + Any_Disputes_Before+
               as.factor(Year), sample[sample$Both_Soccer==1&
                                         sample$Same_Continent==1,])

summary(model_v2)$coefficients[2,]



model_v3<-lm(y ~ Group_Stage + ln_Dist + ln_Country1_GDP +
               ln_Country2_GDP + Both_GATT + Both_EU +
               Both_Dem + Contiguous + Colony +
               Sibling + Alliance_Year_Before + Any_Disputes_Before+
               as.factor(Year), sample[sample$Both_Soccer==1&
                                         sample$Same_Continent==1,])

summary(model_v3)$coefficients[2,]




model_v4<-lm(ln_Adjusted_Trade-ln_Bi_Trade_Pre ~ 
               Group_Stage + ln_Dist + ln_Country1_GDP +
               ln_Country2_GDP + Both_GATT + Both_EU +
               Both_Dem + Contiguous + Colony +
               Sibling + Alliance_Year_Before + Any_Disputes_Before+
               as.factor(Year), sample[sample$Both_Soccer==1&
                                       sample$Same_Continent==0,])

summary(model_v4)$coefficients[2,]

-0.013754214/sd((sample$ln_Adjusted_Trade-sample$ln_Bi_Trade_Pre)[sample$Both_Soccer==1&
                                                                    sample$Same_Continent==0])

0.084931857/2

0.007980294*1.65/1.96

sum(sample$Both_Soccer==1&sample$Same_Continent==0)



model_v5<-lm(Percent_Change_Adjusted ~ 
               Group_Stage + ln_Dist + ln_Country1_GDP +
               ln_Country2_GDP + Both_GATT + Both_EU +
               Both_Dem + Contiguous + Colony +
               Sibling + Alliance_Year_Before + Any_Disputes_Before+
               as.factor(Year), sample[sample$Both_Soccer==1&
                                         sample$Same_Continent==0,])

summary(model_v5)$coefficients[2,]

-0.014540266/sd((sample$Percent_Change_Adjusted)[sample$Both_Soccer==1&
                                                 sample$Same_Continent==0])

0.098259914/2

0.008790776*1.65/1.96


model_v6<-lm(y ~ Group_Stage + ln_Dist + ln_Country1_GDP +
               ln_Country2_GDP + Both_GATT + Both_EU +
               Both_Dem +  Common_Religion + Contiguous + Colony +
               Sibling + Alliance_Year_Before + Any_Disputes_Before+
               as.factor(Year), sample[sample$Both_Soccer==1&
                                       sample$Same_Continent==0,])

summary(model_v6)$coefficients[2,]

0.06006997/sd((sample$y)[sample$Both_Soccer==1&sample$Same_Continent==0])

0.03446961/2

0.02839002*1.65/1.96




sum(abs(no_ties$Z)<=3&no_ties$SoccerMostPopular1==1&
      no_ties$Same_Continent==1)

model_v7<-lm(ln_Adjusted_Imports-ln_Imports_Pre~Loss+Z+I(Loss*Z)+
            ln_Dist + ln_Country1_GDP + ln_Country2_GDP + 
            Both_GATT + Both_EU + Both_Dem +  
            Contiguous + Colony + Sibling + 
            Alliance_Year_Before+Any_Disputes_Before+
            as.factor(Year), no_ties[abs(no_ties$Z)<=3&
                                     no_ties$SoccerMostPopular1==1&
                                     no_ties$Same_Continent==1,])

summary(model_v7)$coefficients[2,]




model_v8<-lm(Percent_Change_Adjusted~Loss+Z+I(Loss*Z)+
               ln_Dist + ln_Country1_GDP + ln_Country2_GDP + 
               Both_GATT + Both_EU + Both_Dem +  
               Contiguous + Colony + Sibling + 
               Alliance_Year_Before+Any_Disputes_Before+
               as.factor(Year), no_ties[abs(no_ties$Z)<=3&
                                          no_ties$SoccerMostPopular1==1&
                                          no_ties$Same_Continent==1,])

summary(model_v8)$coefficients[2,]




model_v9<-lm(y1~Loss+Z+I(Loss*Z)+
               ln_Dist + ln_Country1_GDP + ln_Country2_GDP + 
               Both_GATT + Both_EU + Both_Dem +  
               Contiguous + Colony + Sibling + 
               Alliance_Year_Before+Any_Disputes_Before+
               as.factor(Year), no_ties[abs(no_ties$Z)<=3&
                                          no_ties$SoccerMostPopular1==1&
                                          no_ties$Same_Continent==1,])

summary(model_v9)$coefficients[2,]







sum(abs(no_ties$Z)<=3&no_ties$SoccerMostPopular1==1&
      no_ties$Same_Continent==0)

model_v10<-lm(ln_Adjusted_Imports-ln_Imports_Pre~Loss+Z+I(Loss*Z)+
               ln_Dist + ln_Country1_GDP + ln_Country2_GDP + 
               Both_GATT + Both_EU + Both_Dem +  
               Contiguous + Colony + Sibling + 
               Alliance_Year_Before+Any_Disputes_Before+
               as.factor(Year), no_ties[abs(no_ties$Z)<=3&
                                          no_ties$SoccerMostPopular1==1&
                                          no_ties$Same_Continent==0,])

summary(model_v10)$coefficients[2,]

0.008213129/2

sum(abs(no_ties$Z)<=3&
      no_ties$SoccerMostPopular1==1&
      no_ties$Same_Continent==0&
      is.na(no_ties$ln_Adjusted_Imports-no_ties$ln_Imports_Pre)==F)

model_v11<-lm(Percent_Change_Adjusted~Loss+Z+I(Loss*Z)+
               ln_Dist + ln_Country1_GDP + ln_Country2_GDP + 
               Both_GATT + Both_EU + Both_Dem +  
               Contiguous + Colony + Sibling + 
               Alliance_Year_Before+Any_Disputes_Before+
               as.factor(Year), no_ties[abs(no_ties$Z)<=3&
                                          no_ties$SoccerMostPopular1==1&
                                          no_ties$Same_Continent==0,])

summary(model_v11)$coefficients[2,]

0.006285447/2


model_v12<-lm(y1~Loss+Z+I(Loss*Z)+
               ln_Dist + ln_Country1_GDP + ln_Country2_GDP + 
               Both_GATT + Both_EU + Both_Dem +  
               Contiguous + Colony + Sibling + 
               Alliance_Year_Before+Any_Disputes_Before+
               as.factor(Year), no_ties[abs(no_ties$Z)<=3&
                                          no_ties$SoccerMostPopular1==1&
                                          no_ties$Same_Continent==0,])

summary(model_v12)$coefficients[2,]

0.06594082/2







# Check results for pairs of countries that did (and did not)
# play at the World Cup in the previous 10 years

model_v1<-lm(ln_Adjusted_Trade-ln_Bi_Trade_Pre ~ 
               Group_Stage + ln_Dist + ln_Country1_GDP +
               ln_Country2_GDP + Both_GATT + Both_EU +
               Both_Dem + Contiguous + Colony +
               Sibling + Alliance_Year_Before + Any_Disputes_Before+
               as.factor(Year), sample[sample$Both_Soccer==1&
                                         sample$Played_Before_Last_10_Years==1,])

summary(model_v1)$coefficients[2,]



model_v2<-lm(Percent_Change_Adjusted ~ Group_Stage + ln_Dist + ln_Country1_GDP +
               ln_Country2_GDP + Both_GATT + Both_EU +
               Both_Dem + Contiguous + Colony +
               Sibling + Alliance_Year_Before + Any_Disputes_Before+
               as.factor(Year), sample[sample$Both_Soccer==1&
                                         sample$Played_Before_Last_10_Years==1,])

summary(model_v2)$coefficients[2,]



model_v3<-lm(y ~ Group_Stage + ln_Dist + ln_Country1_GDP +
               ln_Country2_GDP + Both_GATT + Both_EU +
               Both_Dem + Contiguous + Colony +
               Sibling + Alliance_Year_Before + Any_Disputes_Before+
               as.factor(Year), sample[sample$Both_Soccer==1&
                                       sample$Played_Before_Last_10_Years==1,])

summary(model_v3)$coefficients[2,]





model_v4<-lm(ln_Adjusted_Trade-ln_Bi_Trade_Pre ~ 
               Group_Stage + ln_Dist + ln_Country1_GDP +
               ln_Country2_GDP + Both_GATT + Both_EU +
               Both_Dem + Contiguous + Colony +
               Sibling + Alliance_Year_Before + Any_Disputes_Before+
               as.factor(Year), sample[sample$Both_Soccer==1&
                                       sample$Played_Before_Last_10_Years==0,])

summary(model_v4)$coefficients[2,]




model_v5<-lm(Percent_Change_Adjusted ~ 
               Group_Stage + ln_Dist + ln_Country1_GDP +
               ln_Country2_GDP + Both_GATT + Both_EU +
               Both_Dem + Contiguous + Colony +
               Sibling + Alliance_Year_Before + Any_Disputes_Before+
               as.factor(Year), sample[sample$Both_Soccer==1&
                                         sample$Played_Before_Last_10_Years==0,])

summary(model_v5)$coefficients[2,]



model_v6<-lm(y ~ 
               Group_Stage + ln_Dist + ln_Country1_GDP +
               ln_Country2_GDP + Both_GATT + Both_EU +
               Both_Dem + Contiguous + Colony +
               Sibling + Alliance_Year_Before + Any_Disputes_Before+
               as.factor(Year), sample[sample$Both_Soccer==1&
                                       sample$Played_Before_Last_10_Years==0,])

summary(model_v6)$coefficients[2,]





model_v7<-lm(ln_Adjusted_Imports-ln_Imports_Pre~Loss+Z+I(Loss*Z)+
               ln_Dist + ln_Country1_GDP + ln_Country2_GDP + 
               Both_GATT + Both_EU + Both_Dem +  
               Contiguous + Colony + Sibling + 
               Alliance_Year_Before+Any_Disputes_Before+
               as.factor(Year), no_ties[abs(no_ties$Z)<=3&
                                          no_ties$SoccerMostPopular1==1&
                                          no_ties$Played_Before_Last_10_Years==1,])

summary(model_v7)$coefficients[2,]




model_v8<-lm(Percent_Change_Adjusted~Loss+Z+I(Loss*Z)+
               ln_Dist + ln_Country1_GDP + ln_Country2_GDP + 
               Both_GATT + Both_EU + Both_Dem +  
               Contiguous + Colony + Sibling + 
               Alliance_Year_Before+Any_Disputes_Before+
               as.factor(Year), no_ties[abs(no_ties$Z)<=3&
                                          no_ties$SoccerMostPopular1==1&
                                          no_ties$Played_Before_Last_10_Years==1,])

summary(model_v8)$coefficients[2,]




model_v9<-lm(y1~Loss+Z+I(Loss*Z)+
               ln_Dist + ln_Country1_GDP + ln_Country2_GDP + 
               Both_GATT + Both_EU + Both_Dem +  
               Contiguous + Colony + Sibling + 
               Alliance_Year_Before+Any_Disputes_Before+
               as.factor(Year), no_ties[abs(no_ties$Z)<=3&
                                          no_ties$SoccerMostPopular1==1&
                                          no_ties$Played_Before_Last_10_Years==1,])

summary(model_v9)$coefficients[2,]









model_v10<-lm(ln_Adjusted_Imports-ln_Imports_Pre~Loss+Z+I(Loss*Z)+
                ln_Dist + ln_Country1_GDP + ln_Country2_GDP + 
                Both_GATT + Both_EU + Both_Dem +  
                Contiguous + Colony + Sibling + 
                Alliance_Year_Before+Any_Disputes_Before+
                as.factor(Year), no_ties[abs(no_ties$Z)<=3&
                                           no_ties$SoccerMostPopular1==1&
                                           no_ties$Played_Before_Last_10_Years==0,])

summary(model_v10)$coefficients[2,]



model_v11<-lm(Percent_Change_Adjusted~Loss+Z+I(Loss*Z)+
                ln_Dist + ln_Country1_GDP + ln_Country2_GDP + 
                Both_GATT + Both_EU + Both_Dem +  
                Contiguous + Colony + Sibling + 
                Alliance_Year_Before+Any_Disputes_Before+
                as.factor(Year), no_ties[abs(no_ties$Z)<=3&
                                           no_ties$SoccerMostPopular1==1&
                                           no_ties$Played_Before_Last_10_Years==0,])

summary(model_v11)$coefficients[2,]




model_v12<-lm(y1~Loss+Z+I(Loss*Z)+
                ln_Dist + ln_Country1_GDP + ln_Country2_GDP + 
                Both_GATT + Both_EU + Both_Dem +  
                Contiguous + Colony + Sibling + 
                Alliance_Year_Before+Any_Disputes_Before+
                as.factor(Year), no_ties[abs(no_ties$Z)<=3&
                                           no_ties$SoccerMostPopular1==1&
                                           no_ties$Played_Before_Last_10_Years==0,])

summary(model_v12)$coefficients[2,]


















# Check results for pairs of countries that did (and did not) 
# have an alliancce the year before

mean(sample$Alliance_Year_Before==1)

sum(sample$Alliance_Year_Before==1&sample$Both_Soccer==1)

model_v1<-lm(ln_Adjusted_Trade-ln_Bi_Trade_Pre ~ 
               Group_Stage + ln_Dist + ln_Country1_GDP +
               ln_Country2_GDP + Both_GATT + Both_EU +
               Both_Dem + Contiguous + Colony +
               Sibling + Any_Disputes_Before+
               as.factor(Year), sample[sample$Both_Soccer==1&
                                         sample$Alliance_Year_Before==1,])

summary(model_v1)$coefficients[2,]





model_v2<-lm(Percent_Change_Adjusted ~ 
               Group_Stage + ln_Dist + ln_Country1_GDP +
               ln_Country2_GDP + Both_GATT + Both_EU +
               Both_Dem + Contiguous + Colony +
               Sibling + Any_Disputes_Before+
               as.factor(Year), sample[sample$Both_Soccer==1&
                                         sample$Alliance_Year_Before==1,])

summary(model_v2)$coefficients[2,]







model_v3<-lm(y ~ 
               Group_Stage + ln_Dist + ln_Country1_GDP +
               ln_Country2_GDP + Both_GATT + Both_EU +
               Both_Dem + Contiguous + Colony +
               Sibling + Any_Disputes_Before+
               as.factor(Year), sample[sample$Both_Soccer==1&
                                         sample$Alliance_Year_Before==1,])

summary(model_v3)$coefficients[2,]







mean(sample$Alliance_Year_Before==0)

sum(sample$Alliance_Year_Before==0&sample$Both_Soccer==1)

model_v4<-lm(ln_Adjusted_Trade-ln_Bi_Trade_Pre ~ 
               Group_Stage + ln_Dist + ln_Country1_GDP +
               ln_Country2_GDP + Both_GATT + Both_EU +
               Both_Dem + Contiguous + Colony +
               Sibling + Alliance_Year_Before + Any_Disputes_Before+
               as.factor(Year), sample[sample$Both_Soccer==1&
                                         sample$Alliance_Year_Before==0,])

summary(model_v4)$coefficients[2,]

0.049586373/2



model_v5<-lm(Percent_Change_Adjusted ~ 
               Group_Stage + ln_Dist + ln_Country1_GDP +
               ln_Country2_GDP + Both_GATT + Both_EU +
               Both_Dem + Contiguous + Colony +
               Sibling + Alliance_Year_Before + Any_Disputes_Before+
               as.factor(Year), sample[sample$Both_Soccer==1&
                                         sample$Alliance_Year_Before==0,])

summary(model_v5)$coefficients[2,]

0.041125050/2





model_v6<-lm(y ~ 
               Group_Stage + ln_Dist + ln_Country1_GDP +
               ln_Country2_GDP + Both_GATT + Both_EU +
               Both_Dem + Contiguous + Colony +
               Sibling  + Any_Disputes_Before+
               as.factor(Year), sample[sample$Both_Soccer==1&
                                         sample$Alliance_Year_Before==0,])

summary(model_v6)$coefficients[2,]

0.02024019/2










model_v7<-lm(ln_Adjusted_Imports-ln_Imports_Pre~Loss+Z+I(Loss*Z)+
               ln_Dist + ln_Country1_GDP + ln_Country2_GDP + 
               Both_GATT + Both_EU + Both_Dem +  
               Contiguous + Colony + Sibling + 
               Any_Disputes_Before+
               as.factor(Year), no_ties[abs(no_ties$Z)<=3&
                                          no_ties$SoccerMostPopular1==1&
                                          no_ties$Alliance_Year_Before==1,])

summary(model_v7)$coefficients[2,]




model_v8<-lm(Percent_Change_Adjusted~Loss+Z+I(Loss*Z)+
               ln_Dist + ln_Country1_GDP + ln_Country2_GDP + 
               Both_GATT + Both_EU + Both_Dem +  
               Contiguous + Colony + Sibling + 
               Any_Disputes_Before+
               as.factor(Year), no_ties[abs(no_ties$Z)<=3&
                                          no_ties$SoccerMostPopular1==1&
                                          no_ties$Alliance_Year_Before==1,])

summary(model_v8)$coefficients[2,]




model_v9<-lm(y1~Loss+Z+I(Loss*Z)+
               ln_Dist + ln_Country1_GDP + ln_Country2_GDP + 
               Both_GATT + Both_EU + Both_Dem +  
               Contiguous + Colony + Sibling + 
               Any_Disputes_Before+
               as.factor(Year), no_ties[abs(no_ties$Z)<=3&
                                          no_ties$SoccerMostPopular1==1&
                                          no_ties$Alliance_Year_Before==1,])

summary(model_v9)$coefficients[2,]







sum(abs(no_ties$Z)<=3&no_ties$SoccerMostPopular1==1&
    no_ties$Alliance_Year_Before==0)

model_v10<-lm(ln_Adjusted_Imports-ln_Imports_Pre~Loss+Z+I(Loss*Z)+
                ln_Dist + ln_Country1_GDP + ln_Country2_GDP + 
                Both_GATT + Both_EU + Both_Dem +  
                Contiguous + Colony + Sibling + 
                Any_Disputes_Before+
                as.factor(Year), no_ties[abs(no_ties$Z)<=3&
                                           no_ties$SoccerMostPopular1==1&
                                           no_ties$Alliance_Year_Before==0,])

summary(model_v10)$coefficients[2,]

0.01056236/2

sum(abs(no_ties$Z)<=3&
      no_ties$SoccerMostPopular1==1&
      no_ties$Alliance_Year_Before==0&
      is.na(no_ties$ln_Adjusted_Imports-no_ties$ln_Imports_Pre)==F)


model_v11<-lm(Percent_Change_Adjusted~Loss+Z+I(Loss*Z)+
                ln_Dist + ln_Country1_GDP + ln_Country2_GDP + 
                Both_GATT + Both_EU + Both_Dem +  
                Contiguous + Colony + Sibling + 
                Any_Disputes_Before+
                as.factor(Year), no_ties[abs(no_ties$Z)<=3&
                                           no_ties$SoccerMostPopular1==1&
                                           no_ties$Alliance_Year_Before==0,])

summary(model_v11)$coefficients[2,]

0.009362019/2


model_v12<-lm(y1~Loss+Z+I(Loss*Z)+
                ln_Dist + ln_Country1_GDP + ln_Country2_GDP + 
                Both_GATT + Both_EU + Both_Dem +  
                Contiguous + Colony + Sibling + 
                Any_Disputes_Before+
                as.factor(Year), no_ties[abs(no_ties$Z)<=3&
                                           no_ties$SoccerMostPopular1==1&
                                           no_ties$Alliance_Year_Before==0,])

summary(model_v12)$coefficients[2,]


0.09908512/2








# Comparing the estimated treatment effects pre-WWII, during the
# Cold War, and post-Cold War

model_v1<-lm(Percent_Change_Adjusted ~ 
               Group_Stage + ln_Dist + ln_Country1_GDP +
               ln_Country2_GDP + Both_GATT + Both_EU +
               Both_Dem + Contiguous + Colony + Sibling +
               Alliance_Year_Before + Any_Disputes_Before +
               as.factor(Year), sample[sample$Both_Soccer==1&
                                         sample$Year<1940,])

summary(model_v1)$coefficients[2,]



model_v2<-lm(Percent_Change_Adjusted ~ 
               Group_Stage + ln_Dist + ln_Country1_GDP +
               ln_Country2_GDP + Both_GATT + Both_EU +
               Both_Dem + Contiguous + Colony + Sibling +
               Alliance_Year_Before + Any_Disputes_Before +
               as.factor(Year), sample[sample$Both_Soccer==1&
                                         sample$Year%in%1945:1990,])

summary(model_v2)$coefficients[2,]


-0.01714212/-0.03261901

model_v3<-lm(Percent_Change_Adjusted ~ 
               Group_Stage + ln_Dist + ln_Country1_GDP +
               ln_Country2_GDP + Both_GATT + Both_EU +
               Both_Dem + Contiguous + Colony + Sibling +
               Alliance_Year_Before + Any_Disputes_Before +
               as.factor(Year), sample[sample$Both_Soccer==1&
                                         sample$Year%in%1994:2018,])

summary(model_v3)$coefficients[2,]

1 - 0.01276788/0.01714212

1 - 0.01276788/0.03261901




model_v4<-lm(Percent_Change_Adjusted~Loss+Z+I(Loss*Z)+
            ln_Dist + ln_Country1_GDP + ln_Country2_GDP +
            Both_GATT + Both_EU + Both_Dem + Contiguous +
            Colony + Sibling + Alliance_Year_Before +
            Any_Disputes_Before + as.factor(Year),
          no_ties[no_ties$SoccerMostPopular1==1&
                  abs(no_ties$Z)<=3&no_ties$Year<1940,])

summary(model_v4)$coefficients[2,]



# The sample size is too small for this RDD test

sum(no_ties$SoccerMostPopular1==1&
    abs(no_ties$Z)<=3&
    no_ties$Year<1940&
    is.na(no_ties$Percent_Change_Adjusted)==F)



model_v5<-lm(Percent_Change_Adjusted~Loss+Z+I(Loss*Z)+
            ln_Dist + ln_Country1_GDP + ln_Country2_GDP +
            Both_GATT + Both_EU + Both_Dem + Contiguous +
            Colony + Sibling + Alliance_Year_Before +
            Any_Disputes_Before + as.factor(Year),
          no_ties[no_ties$SoccerMostPopular1==1&
                  abs(no_ties$Z)<=3&no_ties$Year%in%1945:1990,])

summary(model_v5)$coefficients[2,]



model_v6<-lm(Percent_Change_Adjusted~Loss+Z+I(Loss*Z)+
            ln_Dist + ln_Country1_GDP + ln_Country2_GDP +
            Both_GATT + Both_EU + Both_Dem + Contiguous +
            Colony + Sibling + Alliance_Year_Before +
            Any_Disputes_Before + as.factor(Year),
          no_ties[no_ties$SoccerMostPopular1==1&
                  abs(no_ties$Z)<=3&no_ties$Year>1990,])

summary(model_v6)$coefficients[2,]








covs<-c("Both_Dem",
       "Both_NonDem", "Diff_Regime", "Both_GATT","One_GATT",
       "Both_EU","One_EU",
       "Colony","Sibling","ln_Dist","Contiguous",
       "Alliance_Year_Before",
       "Total_Disputes_Before","Any_Disputes_Before",
       "ln_Trade_Change_Adjusted_Prev",
       "Percent_Change_Adjusted_Prev",
       "ym1")

ps<-rep(NA,length(covs))

avgs<-rep(NA,length(covs))

real<-rep(NA,length(covs))

for(i in 1:length(covs)){
  p<-mean(perm_data[,covs[i]]>=real_data[,covs[i]])
  if(p<0.5)p2<-2*p
  if(p>=0.5)p2<-(1-p)*2
  ps[i]<-p2}	

names<-c(
  "Both Democracies",
  "Both Non-Democracies", "Different Regimes", "Both GATT/WTO",
  "One GATT/WTO","Both EU",
  "One EU","Former Colony",
  "Sibling","ln(Dist)","Contiguous",
  "Alliance Year Before",
  "Total Military Disputes Year Before",
  "Any Military Dispute Year Before", 
  "Previous Change in ln(Trade)",
  "Previous % Change in Trade", "Previous Drop in Trade")

p_vals<-data.frame(names,ps)

p_vals$names <- factor(p_vals$names, levels=p_vals$names[rev(order(1:(length(covs)+3)))])

colors<-c("royalblue2")

place1 <- ggplot(p_vals,aes(x=ps,y=names))+geom_point(color="blue")
place1 <- place1 + scale_x_continuous(labels=c(0,0.25,0.5,0.75,1),breaks=c(0,0.25,0.5,0.75,1),limits=c(0,1),expand=c(0,0)) +geom_vline(xintercept=c(0.01,0.05),linetype="dashed") + ylab("Covariates") + xlab("p-value") +theme_bw()

place1

ggsave("WCBalance.pdf",width=4,height=2.2,scale = 1.6)





covs<-c("Soccer_Both_Dem",
        "Soccer_Both_NonDem", "Soccer_Diff_Regime", 
        "Soccer_Both_GATT","Soccer_One_GATT",
        "Soccer_Both_EU","Soccer_One_EU",
        "Soccer_Colony","Soccer_Sibling","Soccer_ln_Dist","Soccer_Contiguous",
        "Soccer_Alliance_Year_Before",
        "Soccer_Total_Disputes_Before","Soccer_Any_Disputes_Before",
        "Soccer_ln_Trade_Change_Adjusted_Prev",
        "Soccer_Percent_Change_Adjusted_Prev",
        "Soccer_ym1")

ps<-rep(NA,length(covs))

avgs<-rep(NA,length(covs))

real<-rep(NA,length(covs))

for(i in 1:length(covs)){
  p<-mean(perm_data[,covs[i]]>=real_data[,covs[i]])
  if(p<0.5)p2<-2*p
  if(p>=0.5)p2<-(1-p)*2
  ps[i]<-p2}	

names<-c(
  "Both Democracies",
  "Both Non-Democracies", "Different Regimes", "Both GATT/WTO",
  "One GATT/WTO","Both EU",
  "One EU","Former Colony",
  "Sibling","ln(Dist)","Contiguous",
  "Alliance Year Before",
  "Total Military Disputes Year Before",
  "Any Military Dispute Year Before", 
  "Previous Change in ln(Trade)",
  "Previous % Change in Trade", "Previous Drop in Trade")

p_vals<-data.frame(names,ps)

p_vals$names <- factor(p_vals$names, levels=p_vals$names[rev(order(1:(length(covs)+3)))])

colors<-c("royalblue2")

place1 <- ggplot(p_vals,aes(x=ps,y=names))+geom_point(color="blue")
place1 <- place1 + scale_x_continuous(labels=c(0,0.25,0.5,0.75,1),breaks=c(0,0.25,0.5,0.75,1),limits=c(0,1),expand=c(0,0)) +geom_vline(xintercept=c(0.01,0.05),linetype="dashed") + ylab("Covariates") + xlab("p-value") +theme_bw()

place1

ggsave("WCBalance_Soccer.pdf",width=4,height=2.2,scale = 1.6)















covs<-c("Irst1", "Milex1", "Milper1","Tpop1","Upop1",
        "Democracy1","Country1_GATT","Country1_EU","IndependenceYear1","Both_Dem",
        "Both_NonDem", "Diff_Regime", "Contiguous","Total_Disputes_Before",	
        "Any_Disputes_Before","Alliance_Year_Before",
        "ln_Dist","Colony","Sibling","Both_GATT","One_GATT","Both_EU",
        "One_EU","Change_ln_Adjusted_Imports_Prev","Percent_Change_Adjusted_Prev","y1m1")

samp<-no_ties[abs(no_ties$Z)<=3&
              is.na(no_ties$ln_Adjusted_Imports-
              no_ties$ln_Imports_Pre)==F,]

ps2<-rep(NA,length(covs))

for(i in 1:length(covs)){
  
  model<-lm(samp[,covs[i]]~samp$Win+samp$Z+I(samp$Win*samp$Z))
  ps2[i]<-summary(model)$coefficients[2,4]
  
}

names<-c("Iron and Steel Production", "Military Expenditures", "Military Personnel",
         "Total Population","Urban Population",
         "Democracy","GATT/WTO Member","EU Member","Independence Year",
         "Both Democracies","Both Non-Democracies", "Different Regimes", 
         "Contiguous","Total Military Disputes Year Before", 
         "Any Military Dispute Year Before",
         "Alliance Year Before",
         "ln(Dist)","Former Colony","Sibling",
         "Both GATT/WTO Members","One GATT/WTO Member","Both EU Members",
         "One EU Member","Previous Change in ln(Imports)","Previous % Change in Imports", 
         "Previous Drop in Imports")

p_vals2<-data.frame(names,ps2)

p_vals2$names <- factor(p_vals2$names, levels=p_vals2$names[rev(order(1:length(covs)))])

colors=c("royalblue2")

place2 <- ggplot(p_vals2,aes(x=ps2,y=names))+geom_point(color="blue")
place2 <- place2 + scale_x_continuous(labels=c(0,0.25,0.5,0.75,1),breaks=c(0,0.25,0.5,0.75,1),limits=c(0,1.05),expand=c(0,0)) +geom_vline(xintercept=c(0.01,0.05),linetype="dashed") + ylab("Covariates") + xlab("p-value") +theme_bw()

place2

ggsave("WCBalance2.pdf",width=4,height=2.7,scale = 1.6)







covs<-c("Irst1", "Milex1", "Milper1","Tpop1","Upop1",
        "Democracy1","Country1_GATT","Country1_EU","IndependenceYear1","Both_Dem",
        "Both_NonDem", "Diff_Regime", "Contiguous","Total_Disputes_Before",	
        "Any_Disputes_Before","Alliance_Year_Before",
        "ln_Dist","Colony","Sibling","Both_GATT","One_GATT","Both_EU",
        "One_EU","Change_ln_Adjusted_Imports_Prev","Percent_Change_Adjusted_Prev","y1m1")



samp<-no_ties[no_ties$SoccerMostPopular1==1&abs(no_ties$Z)<=3&
                is.na(no_ties$ln_Adjusted_Imports-
                        no_ties$ln_Imports_Pre)==F,]

ps2<-rep(NA,length(covs))

for(i in 1:length(covs)){
  
  model<-lm(samp[,covs[i]]~samp$Win+samp$Z+I(samp$Win*samp$Z))
  ps2[i]<-summary(model)$coefficients[2,4]
  
}


names<-c("Iron and Steel Production", "Military Expenditures", "Military Personnel",
         "Total Population","Urban Population",
         "Democracy","GATT/WTO Member","EU Member","Independence Year",
         "Both Democracies","Both Non-Democracies", "Different Regimes", 
         "Contiguous","Total Military Disputes Year Before", 
         "Any Military Dispute Year Before",
         "Alliance Year Before",
         "ln(Dist)","Former Colony","Sibling",
         "Both GATT/WTO Members","One GATT/WTO Member","Both EU Members",
         "One EU Member","Previous Change in ln(Imports)","Previous % Change in Imports", 
         "Previous Drop in Imports")

p_vals2<-data.frame(names,ps2)


p_vals2$names <- factor(p_vals2$names, levels=p_vals2$names[rev(order(1:length(covs)))])


colors=c("royalblue2")

place2 <- ggplot(p_vals2,aes(x=ps2,y=names))+geom_point(color="blue")
place2 <- place2 + scale_x_continuous(labels=c(0,0.25,0.5,0.75,1),breaks=c(0,0.25,0.5,0.75,1),limits=c(0,1.05),expand=c(0,0)) +geom_vline(xintercept=c(0.01,0.05),linetype="dashed") + ylab("Covariates") + xlab("p-value") +theme_bw()

place2

ggsave("WCBalance2_Soccer.pdf",width=4,height=2.7,scale = 1.6)









set.seed(0)

covs<-c("ln_Dist", "ln_Country1_GDP","ln_Country2_GDP", 
        "Both_GATT", "Both_EU", "Both_Dem", "Contiguous", 
        "Colony", "Sibling", "Alliance_Year_Before", 
        "Any_Disputes_Before")

getpvalue=function(line){
  model=lm(noquote(line),sample[sample$Both_Soccer==1,])
  return(summary(model)$coef[2,4]/2)
}

pvaluevector1=1:10000

n<-length(covs)

probs=0:n
for(j in 0:n){
  probs[j+1]=choose(n,j)/(2^n)}	

for(i in 1:10000){
  numberofcovs=sample(0:n,1,prob=probs)
  combo=sample(covs, numberofcovs, replace=FALSE)	
  combo=paste(combo,collapse="+")
  if(combo=="") line=paste(c("ln_Adjusted_Trade-ln_Bi_Trade_Pre~Group_Stage",
                             combo,"+ as.factor(Year)"),collapse=" ")
  if(combo!="") line=paste(c("ln_Adjusted_Trade-ln_Bi_Trade_Pre~Group_Stage+",
                             combo,"+ as.factor(Year)"),collapse=" ")
  
  pvaluevector1[i]=getpvalue(line)}

mean(pvaluevector1<0.10)
mean(pvaluevector1<0.05)


ps1=data.frame(p=pvaluevector1)

ggplot(ps1, aes(p))+geom_density(fill="cornflowerblue")+
  scale_x_continuous(limits=c(0,0.07),breaks=c(0,0.01,0.05,0.1))+
  geom_vline(xintercept=c(0,0.01,0.05),linetype="dashed")

ggsave("TradePvalueDistribution00.pdf",
       height=4, width=8)






set.seed(0)

covs<-c("ln_Dist", "ln_Country1_GDP","ln_Country2_GDP", 
       "Both_GATT", "Both_EU", "Both_Dem", "Contiguous", 
       "Colony", "Sibling", "Alliance_Year_Before", 
       "Any_Disputes_Before")

getpvalue=function(line){
  model=lm(noquote(line),sample[sample$Both_Soccer==1,])
  return(summary(model)$coef[2,4]/2)
}

pvaluevector1=1:10000

n<-length(covs)

probs=0:n
for(j in 0:n){
  probs[j+1]=choose(n,j)/(2^n)}	

for(i in 1:10000){
  numberofcovs=sample(0:n,1,prob=probs)
  combo=sample(covs, numberofcovs, replace=FALSE)	
  combo=paste(combo,collapse="+")
  if(combo=="") line=paste(c("Percent_Change_Adjusted~Group_Stage",combo,"+ as.factor(Year)"),collapse=" ")
  if(combo!="") line=paste(c("Percent_Change_Adjusted~Group_Stage+",combo,"+ as.factor(Year)"),collapse=" ")
  
  pvaluevector1[i]=getpvalue(line)}

mean(pvaluevector1<0.10)
mean(pvaluevector1<0.05)


ps1<-data.frame(p=pvaluevector1)

ggplot(ps1, aes(p))+geom_density(fill="cornflowerblue")+scale_x_continuous(limits=c(0,0.06),breaks=c(0,0.01,0.05,0.1))+geom_vline(xintercept=c(0,0.01,0.05),linetype="dashed")

ggsave("TradePvalueDistribution05.pdf",height=4, width=8)






set.seed(0)

covs<-c("ln_Dist", "ln_Country1_GDP","ln_Country2_GDP", 
        "Both_GATT", "Both_EU", "Both_Dem", "Contiguous", 
        "Colony", "Sibling", "Alliance_Year_Before", 
        "Any_Disputes_Before")

getpvalue=function(line){
  model=lm(noquote(line),sample[sample$Both_Soccer==1,])
  return(summary(model)$coef[2,4])
}

pvaluevector1=1:10000

n<-length(covs)

probs=0:n
for(j in 0:n){
  probs[j+1]=choose(n,j)/(2^n)}	

for(i in 1:10000){
  numberofcovs=sample(0:n,1,prob=probs)
  combo=sample(covs, numberofcovs, replace=FALSE)	
  combo=paste(combo,collapse="+")
  if(combo=="") line=paste(c("y~Group_Stage",combo,"+ as.factor(Year)"),collapse=" ")
  if(combo!="") line=paste(c("y~Group_Stage+",combo,"+ as.factor(Year)"),collapse=" ")
  
  pvaluevector1[i]=getpvalue(line)/2}

mean(pvaluevector1<0.10)
mean(pvaluevector1<0.05)


ps1=data.frame(p=pvaluevector1)

ggplot(ps1, aes(p))+geom_density(fill="cornflowerblue")+scale_x_continuous(limits=c(0,0.06),breaks=c(0,0.01,0.05,0.1))+geom_vline(xintercept=c(0,0.01,0.05),linetype="dashed")

ggsave("TradePvalueDistribution1.pdf",height=4, width=8)









covs<-c("Irst1", "Milex1", "Milper1","Tpop1","Upop1",
        "Democracy1","Country1_GATT","Country1_EU",
        "IndependenceYear1","ln_Dist", "ln_Country1_GDP",
        "ln_Country2_GDP",  "Both_GATT", "Both_EU",
        "Both_Dem", "Contiguous", "Colony", "Sibling",
         "Alliance_Year_Before", "Any_Disputes_Before")

getpvalue=function(line){
  model=lm(noquote(line),no_ties[no_ties$SoccerMostPopular1==1&abs(no_ties$Z)<=3,])
  return(summary(model)$coef[2,4]/2)
}

pvaluevector2=1:10000

n<-length(covs)

probs=0:n
for(j in 0:n){
  probs[j+1]=choose(n,j)/(2^n)}	

for(i in 1:10000){
  numberofcovs=sample(0:n,1,prob=probs)
  combo=sample(covs, numberofcovs, replace=FALSE)	
  combo=paste(combo,collapse="+")     
  if(combo=="") line=paste(c("ln_Adjusted_Imports-ln_Imports_Pre~Win+Z+I(Win*Z)",combo,"+ as.factor(Year)"),collapse=" ")
  if(combo!="") line=paste(c("ln_Adjusted_Imports-ln_Imports_Pre~Win+Z+I(Win*Z)+",combo,"+ as.factor(Year)"),collapse=" ")
  
  
  pvaluevector2[i]=getpvalue(line)}

mean(pvaluevector2<0.10)
mean(pvaluevector2<0.05)

ps2=data.frame(p=pvaluevector2)

ggplot(ps2, aes(p))+geom_density(fill="cornflowerblue")+scale_x_continuous(limits=c(0,0.06),breaks=c(0,0.01,0.05,0.1))+geom_vline(xintercept=c(0,0.01,0.05),linetype="dashed")

ggsave("TradePvalueDistribution100.pdf",height=4, width=8)




covs<-c("Irst1", "Milex1", "Milper1","Tpop1","Upop1",
        "Democracy1","Country1_GATT","Country1_EU",
        "IndependenceYear1","ln_Dist", "ln_Country1_GDP",
        "ln_Country2_GDP",  "Both_GATT", "Both_EU",
        "Both_Dem", "Contiguous", "Colony", "Sibling",
        "Alliance_Year_Before", "Any_Disputes_Before")


getpvalue=function(line){
  model=lm(noquote(line),no_ties[no_ties$SoccerMostPopular1==1&abs(no_ties$Z)<=3,])
  return(summary(model)$coef[2,4]/2)
}

pvaluevector2=1:10000

n<-length(covs)

probs=0:n
for(j in 0:n){
  probs[j+1]=choose(n,j)/(2^n)}	

for(i in 1:10000){
  numberofcovs=sample(0:n,1,prob=probs)
  combo=sample(covs, numberofcovs, replace=FALSE)	
  combo=paste(combo,collapse="+")     
  if(combo=="") line=paste(c("Percent_Change_Adjusted~Win+Z+I(Win*Z)",combo,"+ as.factor(Year)"),collapse=" ")
  if(combo!="") line=paste(c("Percent_Change_Adjusted~Win+Z+I(Win*Z)+",combo,"+ as.factor(Year)"),collapse=" ")
  
  
  pvaluevector2[i]=getpvalue(line)}

mean(pvaluevector2<0.10)
mean(pvaluevector2<0.05)

ps2=data.frame(p=pvaluevector2)

ggplot(ps2, aes(p))+geom_density(fill="cornflowerblue")+scale_x_continuous(limits=c(0,0.06),breaks=c(0,0.01,0.05,0.1))+geom_vline(xintercept=c(0,0.01,0.05),linetype="dashed")

ggsave("TradePvalueDistribution15.pdf",height=4, width=8)








covs<-c("Irst1", "Milex1", "Milper1","Tpop1","Upop1",
        "Democracy1","Country1_GATT","Country1_EU",
        "IndependenceYear1","ln_Dist", "ln_Country1_GDP",
        "ln_Country2_GDP",  "Both_GATT", "Both_EU",
        "Both_Dem", "Contiguous", "Colony", "Sibling",
        "Alliance_Year_Before", "Any_Disputes_Before")


getpvalue=function(line){
  model=lm(noquote(line),no_ties[no_ties$SoccerMostPopular1==1&abs(no_ties$Z)<=3,])
  return(summary(model)$coef[2,4]/2)
}

pvaluevector2=1:10000

n<-length(covs)

probs=0:n
for(j in 0:n){
  probs[j+1]=choose(n,j)/(2^n)}	

for(i in 1:10000){
  numberofcovs=sample(0:n,1,prob=probs)
  combo=sample(covs, numberofcovs, replace=FALSE)	
  combo=paste(combo,collapse="+")     
  if(combo=="") line=paste(c("y1~Win+Z+I(Win*Z)",combo,"+ as.factor(Year)"),collapse=" ")
  if(combo!="") line=paste(c("y1~Win+Z+I(Win*Z)+",combo,"+ as.factor(Year)"),collapse=" ")
  
  
  pvaluevector2[i]=getpvalue(line)}

mean(pvaluevector2<0.10)
mean(pvaluevector2<0.05)

ps2=data.frame(p=pvaluevector2)

ggplot(ps2, aes(p))+geom_density(fill="cornflowerblue")+scale_x_continuous(limits=c(0,0.06),breaks=c(0,0.01,0.05,0.1))+geom_vline(xintercept=c(0,0.01,0.05),linetype="dashed")

ggsave("TradePvalueDistribution2.pdf",height=4, width=8)

















covs=c("Both_Dem",
       "Both_NonDem", "Diff_Regime", "Both_GATT","One_GATT",
       "Both_EU","One_EU",
       "Colony","Sibling","ln_Dist","Contiguous",
       "Alliance_Year_Before",
       "TotalDisputesBefore","AnyDisputesBefore")


ps=rep(NA,length(covs))

avgs=rep(NA,length(covs))

real=rep(NA,length(covs))

for(i in 1:length(covs)){
  p=mean(perm_data[,covs[i]]>=real_data[,covs[i]])
  if(p<0.5)p=2*p
  if(p>=0.5)p=(1-p)*2
  ps[i]=p}	

ps<-c(ps, 0.4, 0.4942792)

names=c("Previous Percent Change in Trade", "Previous Trade Drop",
        "Iron and Steel Production", "Military Expenditures", "Military Personnel","Total Population","Urban Population",
        "Democracy","GATT/WTO Member","EU Member",
        "Independence Year","Both Democracies","Both Non-Democracies", "Different Regimes", "Contiguous","Total Military Disputes Year Before", "Any Military Dispute Year Before","Alliance Year Before", "ln(Dist)","Former Colony","Sibling","Both GATT/WTO Members","One GATT/WTO Member","Both EU Members", "One EU Member")

names=c(
  "Both Democracies",
  "Both Non-Democracies", "Different Regimes", "Both GATT/WTO",
  "One GATT/WTO","Both EU",
  "One EU","Former Colony",
  "Sibling","ln(Dist)","Contiguous",
  "Alliance Year Before",
  "Total Military Disputes Year Before",
  "Any Military Dispute Year Before", 
  "Previous % Change in Trade", "Previous Drop in Trade")




p_vals=data.frame(names,ps)

p_vals$names <- factor(p_vals$names, levels=p_vals$names[rev(order(1:(length(covs)+2)))])



colors=c("royalblue2")

place1 <- ggplot(p_vals,aes(x=ps,y=names))+geom_point(color="blue")
place1 <- place1 + scale_x_continuous(labels=c(0,0.25,0.5,0.75,1),breaks=c(0,0.25,0.5,0.75,1),limits=c(0,1),expand=c(0,0)) +geom_vline(xintercept=c(0.01,0.05),linetype="dashed") + ylab("Covariates") + xlab("p-value") +theme_bw()


place1

ggsave("WCBalance.pdf",width=4,height=2.2,scale = 1.6)











covs=c("p_Both_Dem",
       "p_Both_NonDem", "p_Diff_Regime", "p_Both_GATT","p_One_GATT",
       "p_Both_EU","p_One_EU",
       "p_Common_Religion","p_Colony","p_Sibling","p_ln_Dist","p_Contiguous",
       "p_Alliance_Year_Before",
       "p_Total_Disputes_Before","p_Any_Disputes_Before"
)


ps=rep(NA,length(covs))

avgs=rep(NA,length(covs))

real=rep(NA,length(covs))

for(i in 1:length(covs)){
  p=mean(perm_data[,covs[i]]>=real_data[,covs[i]])
  if(p<0.5)p=2*p
  if(p>=0.5)p=(1-p)*2
  ps[i]=p}	


names=c("Iron and Steel Production", "Military Expenditures", "Military Personnel","Total Population","Urban Population",
        "Democracy","GATT/WTO Member","EU Member",
        "Independence Year","Both Democracies","Both Non-Democracies", "Different Regimes", "Contiguous","Total Military Disputes Year Before", "Any Military Dispute Year Before","Alliance Year Before", "ln(Dist)","Common Religion","Former Colony","Sibling","Both GATT/WTO Members","One GATT/WTO Member","Both EU Members", "One EU Member")



names=c("Both Democracies",
        "Both Non-Democracies", "Different Regimes", "Both GATT/WTO",
        "One GATT/WTO","Both EU",
        "One EU","Common Religion","Former Colony",
        "Sibling","ln(Dist)","Contiguous",
        "Alliance Year Before",
        "Total Military Disputes Year Before",
        "Any Military Dispute Year Before")




p_vals=data.frame(names,ps)

p_vals$names <- factor(p_vals$names, levels=p_vals$names[rev(order(1:length(covs)))])



colors=c("royalblue2")

place1 <- ggplot(p_vals,aes(x=ps,y=names))+geom_point(color="blue")
place1 <- place1 + scale_x_continuous(breaks=c(0,0.25,0.5,0.75,1),limits=c(0,1.05),expand=c(0,0)) +geom_vline(xintercept=c(0.01,0.05),linetype="dashed") + ylab("Covariates") + xlab("p-value") +theme_bw()


place1

ggsave("WCBalance.pdf",width=4,height=2.5,scale = 1.6)




covs<-c("Both_Dem",
        "Both_NonDem", "Diff_Regime", "Both_GATT","One_GATT",
        "Both_EU","One_EU",
        "Common_Religion","Colony","Sibling","ln_Dist","Contiguous",
        "Alliance_Year_Before",
        "Total_Disputes_Before","Any_Disputes_Before"
)

ps=rep(NA,length(covs))

avgs=rep(NA,length(covs))

real=rep(NA,length(covs))

for(i in 1:length(covs)){
  model<-lm(sample[sample$Both_Soccer==1,covs[i]]~sample$Group_Stage[sample$Both_Soccer==1])
  ps[i]=summary(model)$coefficients[2,4]}	




names=c("Iron and Steel Production", "Military Expenditures", "Military Personnel","Total Population","Urban Population",
        "Democracy","GATT/WTO Member","EU Member",
        "Independence Year","Both Democracies","Both Non-Democracies", "Different Regimes", "Contiguous","Total Military Disputes Year Before", "Any Military Dispute Year Before","Alliance Year Before", "ln(Dist)","Common Religion","Former Colony","Sibling","Both GATT/WTO Members","One GATT/WTO Member","Both EU Members", "One EU Member")



names=c("Both Democracies",
        "Both Non-Democracies", "Different Regimes", "Both GATT/WTO",
        "One GATT/WTO","Both EU",
        "One EU","Common Religion","Former Colony",
        "Sibling","ln(Dist)","Contiguous",
        "Alliance Year Before",
        "Total Military Disputes Year Before",
        "Any Military Dispute Year Before")




p_vals=data.frame(names,ps)

p_vals$names <- factor(p_vals$names, levels=p_vals$names[rev(order(1:length(covs)))])



colors=c("royalblue2")

place1 <- ggplot(p_vals,aes(x=ps,y=names))+geom_point(color="blue")
place1 <- place1 + scale_x_continuous(breaks=c(0,0.25,0.5,0.75,1),limits=c(0,1.05),expand=c(0,0)) +geom_vline(xintercept=c(0.01,0.05),linetype="dashed") + ylab("Covariates") + xlab("p-value") +theme_bw()


place1

ggsave("WCBalance.pdf",width=4,height=2.5,scale = 1.6)






# Comparing countries that won and lost by one point

model<-lm(ln_Adjusted_Imports-ln_Imports_Pre~Loss+
            ln_Dist + ln_Country1_GDP + ln_Country2_GDP + 
            Both_GATT + Both_EU + Both_Dem +  
            Contiguous + Colony + Sibling + 
            Alliance_Year_Before+Any_Disputes_Before+
            as.factor(Year), no_ties[abs(no_ties$Z)<=1,])

summary(model)

0.007310/2

0.018669*1.65/1.96

y1



model<-lm(ln_Adjusted_Imports-ln_Imports_Pre~Loss+
            ln_Dist + ln_Country1_GDP + ln_Country2_GDP + 
            Both_GATT + Both_EU + Both_Dem +  
            Contiguous + Colony + Sibling + 
            Alliance_Year_Before+Any_Disputes_Before+
            as.factor(Year), no_ties[abs(no_ties$Z)<=1&
                                     no_ties$SoccerMostPopular1==1,])

summary(model)

0.093607/2

model<-lm(Percent_Change_Adjusted~Loss+
            ln_Dist + ln_Country1_GDP + ln_Country2_GDP + 
            Both_GATT + Both_EU + Both_Dem +  
            Contiguous + Colony + Sibling + 
            Alliance_Year_Before+Any_Disputes_Before+
            as.factor(Year), no_ties[abs(no_ties$Z)<=1&
                                       no_ties$SoccerMostPopular1==1,])

summary(model)

0.13266/2

model<-lm(y1~Loss+
            ln_Dist + ln_Country1_GDP + ln_Country2_GDP + 
            Both_GATT + Both_EU + Both_Dem +  
            Contiguous + Colony + Sibling + 
            Alliance_Year_Before+Any_Disputes_Before+
            as.factor(Year), no_ties[abs(no_ties$Z)<=1&
                                       no_ties$SoccerMostPopular1==1,])

summary(model)

0.1305/2
