### William Keilsohn
### Power and Effect, Chi Square

library(dplyr)

### s from the varience table is "standard deviation" in this case. 
#Re-calculate this in R, and double check the math.

v1<-(sd(copper$Untreated[1:12]))^2
v2<-(sd(copper$Exposed))^2
S1<-v1*(length(copper$Untreated[1:12])-1)
S2<-v2*(length(copper$Exposed)-1)  
pv1<-(S1+S2)/(length(copper$Untreated[1:12])+length(copper$Exposed)-2)
sd1<-sqrt(pv1)

power.t.test(n=13, power = 0.90, delta = NULL, sd = sd1, sig.level = 0.05)
power.t.test(n=13, power = NULL, delta = 3, sd = sd1, sig.level = 0.05)

dm<-abs(mean(copper$Untreated[1:12])-mean(copper$Exposed))
power.t.test(n=13, power = NULL, delta = dm, sd = sd1, sig.level = 0.05)



shapiro.test(pulse$bpm) ### Not normally distributed
wilcox.test(pulse$bpm, mu = 69.35, alternative = c("less"), paired = F) ### alternative dirrection given for tested
###Forgot to remove the 103 value

pulse2<-filter(pulse, bpm < 103) ### Removes that pesky 103 value.

shapiro.test(pulse2$bpm) ### Now it is normally distributed
### Now create a Z test function:
m1 <- 69.35
dr1 <- 9.79
m2<- mean(pulse2$bpm)
n1<-sqrt(length(pulse2$bpm))
z1<-((m1-m2)/(dr1/n1))

p1<-mean(pulse2$bpm)
d1<-abs(p1-69.35)
power.t.test(n = length(pulse2$bpm), power = NULL, delta = d1, sd = 9.79, sig.level = 0.05)

### Chi Square
obs<-c(23,57,20) ### Actual data
exp<-c(25/100,50/100,25/100) ### Assuming 1:2:1 from 100 samples --- probabilities must sum to one

chisq.test(x=obs, p=exp)


### Creating a Table
exrp<-data.frame(suppressed=c(37,34,0,3),normal=c(13,16,50,47))
row.names(exrp)<-c("gallate","acid","Theasinensin","H2O")
exrp<-mutate(exrp,sum1=rowSums(exrp))
exrp<-rbind(exrp,sums2 = colSums(exrp))


### Chi square
chisq.test(exrp) #This worked...


### Additional Tables
treatment<-c(colSums(exrp[1:3,1:2]))
control<-c(colSums(exrp[4,1:2]))
exrp2<-data.frame(treatment,control)
chisq.test(exrp2)

###  Each table tests "contingency"
## Based on the ability to combine chi square tables
treatment2<-c(colSums(exrp[3,1:2]))
control2<-c(colSums(exrp[4,1:2]))
exrp3<-data.frame(treatment2,control2)
chisq.test(exrp3)


treatment3<-c(colSums(exrp[1,1:2]))
control3<-c(colSums(exrp[2,1:2]))
exrp4<-data.frame(treatment3,control3)
chisq.test(exrp4)


treatment4<-c(colSums(exrp[1:2,1:2]))
control4<-c(colSums(exrp[3:4,1:2]))
exrp5<-data.frame(treatment4,control4)
chisq.test(exrp5)

