
shuffle1<-unlist(sample(c(africa1998[c(2,4),3])))

shuffle2<-unlist(sample(c(africa1998[1,2],africa1998[c(2,4),1])))

shuffle3<-unlist(sample(c(africa1998[2:4,2],africa1998[3,3],
			africa1998[2,4])))

shuffle4<-unlist(sample(c(africa1998[2:4,4],africa1998[1,3])))

 perm_groups1998<-africa1998

 perm_groups1998[c(2,4),3]<-shuffle1

 perm_groups1998[1,2]<-shuffle2[1]

perm_groups1998[c(2,4),1]<-shuffle2[2:3]

perm_groups1998[2:4,2]<-shuffle3[1:3]

perm_groups1998[3,3]<-shuffle3[4]

perm_groups1998[2,4]<-shuffle3[5]

perm_groups1998[2:4,4]<-shuffle4[1:3]

perm_groups1998[1,3]<-shuffle4[4]





perm_groups2002<-africa2002

perm_groups2002[,1]<-sample(perm_groups2002[,1])

perm_groups2002[,2]<-sample(perm_groups2002[,2])

perm_groups2002[,3]<-sample(perm_groups2002[,3])

perm_groups2002[,4]<-sample(perm_groups2002[,4])

perm_groups2002 
 




perm_groups2004<-africa2004

perm_groups2004[,1]<-sample(perm_groups2004[,1])

perm_groups2004[,2]<-sample(perm_groups2004[,2])

perm_groups2004[,3]<-sample(perm_groups2004[,3])

perm_groups2004[,4]<-sample(perm_groups2004[,4])

perm_groups2004 
 

 
 
perm_groups2006<-africa2006

perm_groups2006[,1]<-sample(perm_groups2006[,1])

perm_groups2006[,2]<-sample(perm_groups2006[,2])

perm_groups2006[,3]<-sample(perm_groups2006[,3])

perm_groups2006[,4]<-sample(perm_groups2006[,4])

perm_groups2006 
 
 
 
perm_groups2008<-africa2008

perm_groups2008[,1]<-sample(perm_groups2008[,1])

perm_groups2008[,2]<-sample(perm_groups2008[,2])

perm_groups2008[,3]<-sample(perm_groups2008[,3])

perm_groups2008[,4]<-sample(perm_groups2008[,4])

perm_groups2008 
 

perm_groups2010<-africa2010

perm_groups2010[,1]<-sample(perm_groups2010[,1])

perm_groups2010[,2]<-sample(perm_groups2010[,2])

perm_groups2010[,3]<-sample(perm_groups2010[,3])

perm_groups2010[,4]<-sample(perm_groups2010[,4])

perm_groups2010










perm_groups2012<-africa2012

perm_groups2012[,1]<-sample(perm_groups2012[,1])

perm_groups2012[,2]<-sample(perm_groups2012[,2])

perm_groups2012[,3]<-sample(perm_groups2012[,3])

perm_groups2012[,4]<-sample(perm_groups2012[,4])

perm_groups2012

 


perm_groups2013<-africa2013

perm_groups2013[,1]<-sample(perm_groups2013[,1])

perm_groups2013[,2]<-sample(perm_groups2013[,2])

perm_groups2013[,3]<-sample(perm_groups2013[,3])

perm_groups2013[,4]<-sample(perm_groups2013[,4])

perm_groups2013





perm_groups2015<-africa2015

perm_groups2015[,1]<-sample(perm_groups2015[,1])

perm_groups2015[,2]<-sample(perm_groups2015[,2])

perm_groups2015[,3]<-sample(perm_groups2015[,3])

perm_groups2015[,4]<-sample(perm_groups2015[,4])

perm_groups2015
 
 
 
perm_groups2017<-africa2019

perm_groups2017[,1]<-sample(perm_groups2017[,1])

perm_groups2017[,2]<-sample(perm_groups2017[,2])

perm_groups2017[,3]<-sample(perm_groups2017[,3])

perm_groups2017[,4]<-sample(perm_groups2017[,4])

perm_groups2017
 

perm_groups2019<-africa2019

perm_groups2019[2:6,1]<-sample(perm_groups2019[2:6,1])

perm_groups2019[,2]<-sample(perm_groups2019[,2])

perm_groups2019[,3]<-sample(perm_groups2019[,3])

perm_groups2019[,4]<-sample(perm_groups2019[,4])

perm_groups2019
 
 


perm_groups2022<-africa2022

perm_groups2022[,1]<-sample(perm_groups2022[,1])

perm_groups2022[,2]<-sample(perm_groups2022[,2])

perm_groups2022[,3]<-sample(perm_groups2022[,3])

perm_groups2022[,4]<-sample(perm_groups2022[,4])

perm_groups2022










perm_groups<-list(perm_groups1998,
                  perm_groups2002,perm_groups2004,perm_groups2006,
                  perm_groups2008,perm_groups2010,perm_groups2012,
                  perm_groups2013,perm_groups2015,perm_groups2017,
                  perm_groups2019,perm_groups2022)


