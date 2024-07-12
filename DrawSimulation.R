resample<-sample(groups1930[,2:4][groups1930[,2:4]!="NONE"])

perm_groups1930<-cbind(groups1930[,1],resample[1:4],
                       resample[5:8],c(rep("NONE",4)))

perm_groups1930[sample(1:4,1),4]<-resample[9]

perm_groups1934<-cbind(groups1934[,1],sample(groups1934[,2]),
                       groups1934[,3:4])

perm_groups1938<-cbind(groups1938[,1],sample(groups1938[,2]),
                       groups1938[,3:4])

perm_groups1950<-cbind(groups1950[,1],sample(groups1950[,2]),
                       sample(groups1950[,3]),sample(groups1950[,4]))

perm_groups1954<-groups1954

zero_draw<-sample(c("Austria","United Kingdom"))

perm_groups1954[c(3,11),1]<-zero_draw[1]

perm_groups1954[c(1,9),1]<-zero_draw[2]

next_draw<-sample(c("Italy","Turkey","France"))

perm_groups1954[c(6,14),1]<-next_draw[1]

perm_groups1954[c(7,15),1]<-next_draw[2]

perm_groups1954[c(4,12),1]<-next_draw[3]

again_draw<-sample(c("Uruguay","Brazil","Hungary"))

perm_groups1954[c(8,16),1]<-again_draw[1]

perm_groups1954[c(2,10),1]<-again_draw[2]

perm_groups1954[c(5,13),1]<-again_draw[3]

first_draw<-sample(c("West Germany","Yugoslavia"),2)

perm_groups1954[c(5,7),2]<-first_draw[1]

perm_groups1954[c(10,12),2]<-first_draw[2]

second_draw<-sample(c("Mexico","Scotland","Belgium","South Korea"),4)

perm_groups1954[c(2,4),2]<-second_draw[1]

perm_groups1954[c(9,16),2]<-second_draw[2]

perm_groups1954[c(11,14),2]<-second_draw[3]

perm_groups1954[c(13,15),2]<-second_draw[4]



perm_groups1958<-cbind(groups1958[,1],sample(groups1958[,2]),
                       sample(groups1958[,3]),sample(groups1958[,4]))

perm_groups1962<-matrix(c("Chile","Argentina","Brazil","Uruguay",
                          "Czechoslovakia","United Kingdom",
                          "Soviet Union","West Germany","Italy",
                          "Hungary","Spain","Yugoslavia","Bulgaria",
                          "Colombia","Mexico","Switzerland"),nrow=4)

perm_groups1962[1:2]<-sample(perm_groups1962[1:2])
perm_groups1962[c(5,7,8)]<-sample(perm_groups1962[c(5,7,8)])
perm_groups1962[10:12]<-sample(perm_groups1962[10:12])
perm_groups1962[13:16]<-sample(perm_groups1962[13:16])

perm_groups1966<-matrix(c("Argentina","Brazil","Chile","Uruguay","United Kingdom","Hungary","Soviet Union","West Germany","France","Portugal","Italy","Spain","Bulgaria","North Korea","Mexico","Switzerland"),nrow=4)

perm_groups1966[c(1,3,4)]<-sample(perm_groups1966[c(1,3,4)])
perm_groups1966[c(6,7)]<-sample(perm_groups1966[c(6,7)])
perm_groups1966[c(9,10,12)]<-sample(perm_groups1966[c(9,10,12)])
perm_groups1966[13:16]<-sample(perm_groups1966[13:16])

perm_groups1970<-cbind(groups1970[,1],sample(groups1970[,2]),
                       sample(groups1970[,3]),sample(groups1970[,4]))

perm_groups1974<-matrix(c("West Germany","Italy","Netherlands",
                          "Scotland","Bulgaria","East Germany",
                          "Poland","Yugoslavia","Chile","Argentina",
                          "Uruguay","Brazil","Australia","Haiti",
                          "Sweden","Zaire"),nrow=4)

perm_groups1974[c(3,4)]<-sample(perm_groups1974[c(3,4)])
perm_groups1974[5:8]<-sample(perm_groups1974[5:8])
perm_groups1974[c(9,10)]<-sample(perm_groups1974[c(9,10)])
perm_groups1974[13:16]<-sample(perm_groups1974[13:16])

perm_groups1978<-cbind(groups1978[,1],sample(groups1978[,2]),
                       sample(groups1978[,3]),sample(groups1978[,4]))

perm_groups1982<-cbind(groups1982[,1],sample(groups1982[,2]),
                       sample(groups1982[,3]),sample(groups1982[,4]))

perm_groups1986<-cbind(groups1986[,1],sample(groups1986[,2]),
                       sample(groups1986[,3]),sample(groups1986[,4]))

perm_groups1990<-cbind(groups1990[,1],sample(groups1990[,2]),
                       sample(groups1990[,3]),sample(groups1990[,4]))

perm_groups1994<-cbind(groups1994[,1],sample(groups1994[,2]),
                       sample(groups1994[,3]),sample(groups1994[,4]))

perm_groups1998<-cbind(groups1998[,1],groups1998[,2],
                       groups1998[,3],sample(groups1998[,4]))

g2<-sample(c(groups1998[,2],groups1998[2,3]))

perm_groups1998[,2]<-g2[1:8]

g3<-sample(c(groups1998[,3][-2]))

perm_groups1998[3:8,3]<-g3[1:6]

perm_groups1998[1:2,3]<-sample(c(g2[9],g3[7]))

perm_groups2002<-as.matrix(groups2002)

perm_groups2002[9:19]<-sample(perm_groups2002[9:19])

perm_groups2002[20:24]<-sample(perm_groups2002[20:24])

perm_groups2002[17:20]<-sample(perm_groups2002[17:20])

if(perm_groups2002[18]%in%c("Saudi Arabia","China")){
	new_spot<-sample(c(17,20),1)
	perm_groups2002[c(18,new_spot)]<-perm_groups2002[c(new_spot,18)]}

if(perm_groups2002[19]%in%c("Saudi Arabia","China")){
	new_spot<-sample(c(17,20),1)
	perm_groups2002[c(19,new_spot)]<-perm_groups2002[c(new_spot,19)]}
	
if(perm_groups2002[16]%in%c("Uruguay","Paraguay","Ecuador")){
	new_spot<-sample(c(18,19),1)
	perm_groups2002[c(16,new_spot)]<-perm_groups2002[c(new_spot,16)]}	


if(perm_groups2002[20]%in%c("Uruguay","Paraguay","Ecuador")){
	new_spot<-sample(c(18,19),1)
	perm_groups2002[c(20,new_spot)]<-perm_groups2002[c(new_spot,20)]}	

perm_groups2002[25:32]<-sample(perm_groups2002[25:32])

perm_groups2006<-cbind(groups2006[,1],sample(groups2006[,2]),
                       sample(groups2006[,3]),groups2006[,4])

g1<-sample(perm_groups2006[2:8,4])

perm_groups2006[c(3,4,5,6,8),4]<-g1[1:5]

perm_groups2006[c(1,2,7),4]<-sample(c(g1[6:7],perm_groups2006[1,4]))

perm_groups2010<-cbind(groups2010[,1],sample(groups2010[,2]),
                       sample(groups2010[,3]),sample(groups2010[,4]))



perm_groups2014<-groups2014

europe<-sample(c(perm_groups2014[4,2],perm_groups2014[,4]))

perm_groups2014[4,2]<-europe[1]

perm_groups2014[,4]<-europe[-1]

perm_groups2014<-cbind(perm_groups2014[,1],perm_groups2014[,2],
                       sample(perm_groups2014[,3]),
                       sample(perm_groups2014[,4]))

Italy<-sample(c(1,3,4,6),1)

Chile_Ecuador<-sample(c(2,5,7,8),2)

Remaining<-sample((1:8)[-c(Italy,Chile_Ecuador)])

perm_groups2014[Italy,2]<-europe[1]

perm_groups2014[Chile_Ecuador,2]<-c("Chile","Ecuador")

perm_groups2014[Remaining,2]<-groups2014[c(1,3,6:8),2]




perm_groups2018<-groups2018

step1<-sample(c(2,4,5,6,7),2)
step2<-sample((1:8)[-step1])

perm_groups2018[c(4,5),2]<-groups2018[step1,2]
perm_groups2018[-c(4,5),2]<-groups2018[step2,2]

mex<-which(perm_groups2018[,2]=="Mexico")

costa_rica<-sample((1:8)[-mex],1)

perm_groups2018[costa_rica,3]<-"Costa Rica"

uefa1<-c(1,2,3,6,7,8)
uefa2<-which(perm_groups2018[,2]%in%c("Spain","Switzerland",
                                      "United Kingdom","Croatia"))

double_uefa<-uefa1[uefa1%in%uefa2]

remaining<-(1:8)[-costa_rica]

places<-remaining[remaining%in%double_uefa]

non_uefa<-sample(c(1,2,7,8),length(places)) # Second

perm_groups2018[places,3]<-groups2018[non_uefa,3]

rem<-(1:8)[-c(costa_rica,places)]

perm_groups2018[rem,3]<-groups2018[sample((1:8)[-c(non_uefa,5)]),3]


perm_groups2018[,4]<-NA

while(sum(is.na(perm_groups2018[,4]))>0){

perm_groups2018[,4]<-NA

iran<-which(perm_groups2018[,3]=="Iran")

asia<-sample((1:8)[-iran],4)

perm_groups2018[asia,4]<-groups2018[c(1,3,6,8),4] 

africa3<-which(perm_groups2018[,3]%in%c("Tunisia","Egypt","Senegal"))

possible<-(1:8)[-c(asia,africa3)]

if(length(possible)>=2){ africa4<-sample(possible,2)

perm_groups2018[africa4,4]<-groups2018[c(2,4),4]

r<-which(is.na(perm_groups2018[,4]))

na<-c(mex,costa_rica)

open<-r[!r%in%na]

if(length(open)>1) open<-sample(open,1)

if(length(open)==1) perm_groups2018[open,4]<-"Panama"



r<-which(is.na(perm_groups2018[,4]))

uefa3<-which(perm_groups2018[,3]%in%c("Denmark","Iceland","Sweden"))

uefa<-c(uefa1,uefa2,uefa3)

uefa<-uefa[duplicated(uefa)]

non_uefa<-(1:8)[-uefa]

overlap<-intersect(non_uefa,r)

if(length(overlap)==1) perm_groups2018[overlap,4]<-"Serbia"

extra<-0

if(length(overlap)==0){
	if(length(r)>1) r<-sample(r,1)
if(perm_groups2018[r,2]=="Mexico"|perm_groups2018[r,3]=="Costa Rica") extra<-1
if(perm_groups2018[r,3]%in%c("Tunisia","Egypt","Senegal")) extra<-2
if(perm_groups2018[r,3]=="Iran") extra<-3


if(extra==0) cands<-groups2018[c(1:4,5:8),4]
if(extra==1) cands<-groups2018[c(1:4,6,8),4]
if(extra==2) cands<-groups2018[c(1,3,5:8),4]
if(extra==3) cands<-groups2018[c(2,4,7),4]

ind<-which(groups2018[,4]%in%cands)

new<-intersect(ind,non_uefa)

if(length(new)!=0){

if(length(new)>1) new<-sample(new,1)

perm_groups2018[r,4]<-perm_groups2018[new,4]

perm_groups2018[new,4]<-"Serbia"}}}}



perm_groups<-list(perm_groups1930,perm_groups1934,perm_groups1938,
                  perm_groups1950,perm_groups1954,perm_groups1958,
                  perm_groups1962,perm_groups1966,perm_groups1970,
                  perm_groups1974,perm_groups1978,perm_groups1982,
                  perm_groups1986,perm_groups1990,perm_groups1994,
                  perm_groups1998,perm_groups2002,perm_groups2006,
                  perm_groups2010,perm_groups2014,perm_groups2018)



