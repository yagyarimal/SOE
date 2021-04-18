#ordinal regression

data=read.csv("D:/dataanalysisfinal/SOE/rcode/CTG.csv")
str(data)
data=data.frame(data)
#NSP Ordinal
data$NSP=as.ordered(data$NSP)
summary(data)
data$Tendency=as.factor(data$Tendency)
summary(data)
xtabs(~NSP+Tendency,data)# explain sufficient sample size
ind=sample(2,nrow(data),replace=TRUE, prob=c(.8,.2))
train=data[ind==1,]
test=data[ind==2,]
# ordinal logistic regression or proportional odds logistic regression
library(MASS)
model=polr(NSP~LB+AC+FM, data=train,Hess=TRUE)# only three independent and Hess is standard errors
summary(model)#one unit increase in dependent variable will increase/decrease in independent variable in log odd scale when all other vaeiables are in constant

#calculating p value
(ctable=coef(summary(model)))
p=pnorm(abs(ctable[,"t value"]),lower.tail = FALSE)*2
(ctable=cbind(ctable,"p value"=p))#p <0.05 significant
#prediction
pred=predict(model,train[1:5,],type='prob')#only five variable
print(pred,digits=3)# alpha and coefficient 
train[1:5,1:3]
#probability calculation b1= b2=b3= a1=a2= y=b1x1+b2x2+b3x3 p(1)=1/1+e-(a1-y)p(10r 2)=1/1+e-(a2-y)=p2=(p1 or 2)-p(1) p(1 or 2 or 3)=1 p(3)=1-p(1 or 2)
#book1 calculation
model=polr(NSP~.-Max-LB, data=train,Hess=TRUE)#- for removing variables 
summary(model) # not converse os - variable in model
(ctable=coef(summary(model)))
p=pnorm(abs(ctable[,"t value"]),lower.tail = FALSE)*2
(ctable=cbind(ctable,"p value"=p))#p <0.05 significant
#evaluate p value again and significance
#after removing <0.05 p values become final model
#confusion matrix 
pred=predict(model,train)#final prediction
 (tab=table(pred,train$NSP))
 1-sum(diag(tab))/sum(tab)## should be similar with
 #confusion for test
 pred1=predict(model,test)
 tab2=table(pred1,test$NSP)
 1-sum(diag(tab2))/sum(tab2)## should be similar with
 
 #######Ordinal logistic regression or proportional odds logistic regression######################
 data=read.csv("D:/dataanalysisfinal/SOE/rcode/Dataanalysiscsv.csv")
 str(data)
 data=data.frame(data)
 #Ordinal creation to the independent
 data$OrdGPA=as.ordered(data$OrdGPA)
 data$GPAAA=as.ordered(as.factor(data$GPAAA))
 data$GPAa=as.ordered(as.factor(data$GPAa))
 summary(data)
#Cross tabulation of GPA and School 
 xtabs(~OrdGPA+School.1,data)# explain sufficient sample size
 xtabs(~OrdGPA+Parent1.1,data)# explain sufficient sample size
 xtabs(~GPAa+Parent1.1,data)# explain sufficient sample size
 str(data)
library(tidyverse)
library(magrittr)
library(tidyverse) #needs to installed tidyverse
 #Taking OrdGPA as predictor variable
ss=data %>% 
   select(OrdGPA,SLC,School,Plus,Physics,Math,Chemistry,Bio,Parent1,Parent2,
          X1,X2,X3,X4,X5,X6) 
 dim(ss)
 
 ind=sample(2,nrow(ss),replace=TRUE, prob=c(.8,.2))
 train=ss[ind==1,]
 test=ss[ind==2,]
 # ordinal logistic regression or proportional odds logistic regression
library(MASS)
 str(ss)   
 summary(ss)
 ss$GPAA=as.ordered(as.numeric(ss$GPAA))   #the Predicted variable must be Ordinal
 model=polr(OrdGPA~., data=train,Hess=TRUE)
 model=polr(OrdGPA~SLC+School+Plus+Physics+Math+Chemistry+Bio+Parent1+Parent2
            +X1+X2+X3+X4+X5+X6, data=train,Hess=TRUE)# only three independent and Hess is standard errors
 summary(model)#one unit increase in dependent variable will increase/decrease in independent variable in log odd scale when all other vaeiables are in constant
 
 #calculating p value
 (ctable=coef(summary(model)))
 p=pnorm(abs(ctable[,"t value"]),lower.tail = FALSE)*2
 (ctable=cbind(ctable,"p value"=p))#p <0.05 significant
 #prediction
 pred=predict(model,train[1:5,],type='prob')#only five variable
 print(pred,digits=3)# alpha and coefficient 
 train[1:5,1:14]
 #probability calculation b1= b2=b3= a1=a2= y=b1x1+b2x2+b3x3 p(1)=1/1+e-(a1-y)p(10r 2)=1/1+e-(a2-y)=p2=(p1 or 2)-p(1) p(1 or 2 or 3)=1 p(3)=1-p(1 or 2)
 #book1 calculation
 library(MASS)
 str(ss)
 ss$OrdGPA=as.ordered(ss$OrdGPA)
 #removing the values those whose probability is greater than 0.5 removing to model  testing p values
 model=polr(OrdGPA~.-Plus-Physics-Bio-Parent2-Chemistry-School-X3-Math-X5, data=train,Hess=TRUE)  #- for removing variables 
 # is final Model
 summary(model) # not converse os - variable in model
 (ctable=coef(summary(model)))
 p=pnorm(abs(ctable[,"t value"]),lower.tail = FALSE)*2
 (ctable=cbind(ctable,"p value"=p))#p <0.05 significant
 #evaluate p value again and significance
 #after removing <0.05 p values become final model
 #confusion matrix 
 pred=predict(model,train)#final prediction
 (tab=table(pred,train$OrdGPA))
 1-sum(diag(tab))/sum(tab)## should be similar with
 #confusion for test
 pred1=predict(model,test)
 (tab2=table(pred1,test$OrdGPA))
 1-sum(diag(tab2))/sum(tab2)## should be similar with

 data$GPAOr<-ifelse(data$GPA>=3.7,10,
                    ifelse(data$GPA>=3.3,9,
                           ifelse(data$GPA>=3,8, 
                                  ifelse(data$GPA>=2.7,7,
                                         ifelse(data$GPA>=2.3,6,
                                                ifelse(data$GPA>=2.2,5,
                                                       ifelse(data$GPA>=1.7,4,
                                                              ifelse(data$GPA>=1.3,3,
                                                                     ifelse(data$GPA>=1,2,
                                                                            ifelse(data$GPA>=0,1))))))))))
 str(data)
 data$GPAOr=as.ordered(data$GPAOr)
 xtabs(~GPAOr+Prog, data)
 summary(data)
 
 
 #creating dummy of school and grade 
 library(dummies)
 data=read.csv("D:/dataanalysisfinal/SOE/rcode/Dataanalysiscsv.csv")
 str(data)
 levels(data$School)
 data=data.frame(data)
 data$School=as.numeric(as.factor(data$School))
 str(data)
 data$School=as.factor(data$School)
 str(data)
 levels(data$School)
 #gpa grade to categorical
 data=as.data.frame(data)
 #gPA Grade
 gcut=cut(data$GPA, br=c(0,1,1.3,1.7,2.0,2.3,2.7,3.0,3.3,3.7,4), 
               levels=c("F","D","D+","c-","C","C+","B-","B","B+","A-","A"))
 gcut
 #school public private
 str(data)
 data$School
 scut=as.factor(data$School)
 scut
 dd=data.frame(data,gcut,scut)
 str(dd)
 reg=lm(GPAA~gcut+scut,data=dd)
 reg$coefficients
 summary(reg)
 #y=2.44494+0.36301*+0.66108+1.00096+1.39560+0.02903
 
 #Multi-collinearity test
 data=read.csv("D:/dataanalysisfinal/SOE/rcode/Dataanalysiscsv.csv")
 str(data)
 data=data.frame(data)
 summary(data)
 library(tidyverse)
 ss=data %>% 
   select(FinalGPA,SLC,School,Plus,Physics,Math,Chemistry,Bio,Parent1,Parent2,
          X1,X2,X3,X4,X5,X6) 
 dim(ss)
 round(cor(ss),2)
 model=lm(FinalGPA~SLC+Plus+Physics+Math+Chemistry+Bio+Math+Parent1+Parent2+X1+X2+X3+X4+X5+X6,data=ss)
 summary(model)#one unit increase in dependent variable will increase/decrease in independent variable in log odd scale when all other vaeiables are in constant
library(car)
  vif(model)# all values less than 5. or >10 become multi-colinarity
 mean(vif(model)) #this concludes there were not multicolinairy in data set so model predicts accurate
# Similarly
 plot(ss[,2:16])
 eigen(cor(ss[,6:16]))$value
 max(eigen(cor(ss))$values)/min(eigen(cor(ss))$values)
 kappa(cor(ss),exact=TRUE)
 # is the term used to denote the presence of linear and nonlinear relationship[ among explanatory variables the variables  on explanatory variables should not have any sort of co-relationship relationship to predictor variable
 # this is a problem in degree not of kind if R2 of x1 and x2 become 1 become perfect co-linarity ( 0 to 1) when there were large number of association to the model the current saving impacts with last year income or expenses thus it is association with past and present data
 # the data are in cross-sectional data  of input and output
 # remove first column data=data.frame(data[,-1])
 
 #hypothesis testing 
 data=read.csv("D:/dataanalysisfinal/SOE/rcode/Dataanalysiscsv.csv")
 str(data)
 library(tidyverse)
 ss=
   data %>% 
   select(FinalGPA,Int,GPAA) %>% filter(FinalGPA>0)
 t.test(ss$Int,ss$FinalGPA,alternative ='two.sided',mu=0)
 #after seeing p value (if p<0.05 then reject null and there is difference between these values)

 str(data)
 sss=
   data %>% 
   select(Prog,FinalGPA,GPAA) %>% filter(GPAA>0)
sss$Prog=as.factor(sss$Prog)
sss$GPAA=as.factor(sss$GPAA)
str(sss)
# one way anova test in various groups data the anova is test is applicable when analysis in group data
 ano=aov(sss$FinalGPA~sss$Prog)
 summary(ano)
 # mean salary differs with significant when p<0.05 reject null and  conclude there is no random change to become such association
 #the post hock test analyzed within which group it will differs
 tuk=TukeyHSD(ano)
 tuk# compares p values within group
 
 #post hoc test bonferronic test
 #two way anova int, two factors(Parent education Schools and Programs information)
 library(ggpubr)
 str(data)
 dd=data
 data$Parent1.1=as.factor(data$Parent1.1) 
 data$School.1=as.factor(data$School.1)
 data$Prog=as.factor(data$Prog)
 library(tidyverse)
 str(dd)
 dd=dd %>% 
   select(Parent1.1,School.1,Prog,GPA) %>% 
   filter(GPA>0)
 str(dd)
 dd$Parent1.1=as.factor(dd$Parent1.1) 
 dd$School.1=as.factor(dd$School.1)
plot=ggline(dd,x="Parent1.1",y="GPA",color= "School.1",
             add=c("men_se","dotplot"),
             palette(c("#00AFBB","#F7B800")))
 plot
 model=lm(GPA~Parent1.1*School.1,data=dd)
 summary(model)
 anova(model)
 par(mfrow=c(1,2))
 plot(model,2)#for normality
 plot(model,1)#for residual
 
 library("lsmeans")
 library("multcompView")
 posthoc <- lsmeans(model,
                        pairwise ~Parent1.1*School.1, 
                        adjust="tukey")
 posthoc

 












#multi-nomial logistic regression is apply when  predicative variable in numeric with dependent more than one categorical variable the target variables has not order of grade and parent education (the color, red, blue,yellow are the category and iincome hight, middle and low in order but not specific order ) mutinomial and multi probit model were suitable but the assumption is the error terms were being equal distribution in all terms the values are being tested wtih based value
data <- read.csv("D:/dataanalysisfinal/SOE/rcode/Dataanalysiscsv.csv",stringsAsFactors = TRUE)
str(data)
#all except GPA numeric categorical are called as features
library(tidyverse)
library(magrittr)
str(data)
ss=data %>% 
  select(GPAA,SLC,School,Plus,Physics,Math,Chemistry,Bio,Parent1,
         Parent2,X1,X2,X3,X4,X5,X6) 
dim(ss)
str(ss)
str(GPAA)
ss=as.data.frame(ss)
ss$GPAA=as.factor(ss$GPAA)
str(ss)
 set.seed(111)
 ind=sample(2,nrow(ss),replace = TRUE,prob = c(.7,.3))
 training=ss[ind==1,]
 testing=ss[ind==2,]
ss$GPAA=relevel(ss$GPAA,ref="0")# this makes total category with reference 1 the GPA HAS 5 district level
#multi-nominal regression model
library(nnet)# neural network
mymodel=multinom(GPAA~SLC+Physics+School+Plus+Physics+Math+Chemistry+Bio+Parent1+Parent2
                 +X1+X2+X3+X4+X5+X6,data=training)
summary(mymodel)
# initial high errors then decreased finally it conversed
#the coefficient and its interpretation with specified category with its category 
str(ss)
# the explanation is when log odds of category base zero another components rise for + - negative by intercepts
#2 tailed z test 
summary(mymodel)
z=summary(mymodel)$coefficients/summary(mymodel)$standard.errors
z
p=(1-pnorm(abs(z),0,1))*2# 0 means and 1 standard deviation 2 for two tail test
p
#if all values of variables is grater than 0.5 will be drop
#so model is again rearranged using - in model
mymodel=multinom(GPAA~SLC+Physics+Plus+Physics+Math+Chemistry+Bio+Parent1
                 +X1+X2+X5+X6,data=training) #excluding
summary(mymodel)
p=predict(mymodel,training)
head(p)
head(training$GPAA)
tab=table(p,training$GPAA)
tab
1-sum(diag(tab))/sum(tab)# miss classification accuracy
p1=predict(mymodel,testing)
tab2=table(p1,testing$GPAA)
tab2
1-sum(diag(tab2))/sum(tab2)
#prediction
n=table(training$GPAA)
n/sum(n)
round(tab/colSums(tab),2)
n=table(testing$GPAA)
n/sum(n)
round(tab2/colSums(tab2),2)


#correlation and linear regression of data sets
data <- read.csv("D:/dataanalysisfinal/SOE/rcode/Dataanalysiscsv.csv",stringsAsFactors = TRUE)
 # all variable must in numerical  representation the predicted variable in either0 or 1 too
str(data)
dim(data)
#convert all factor to dummylibrary(caret)dmy=dummyVars(" ~ .",data=data) dmyy=data.frame(predict(dmy,newdata=dmy))
library(tidyverse)
ss=
  data %>% 
  select(FinalGPA,Int) %>% filter(FinalGPA>0)
par(mfrow=c(2,2))
plot(ss$FinalGPA,ss$Int/2,main="Scatterplot",ylab = "GPA",xlab = "Internal", las=1)
abline(lm(ss$FinalGPA~ss$Int))
cor(ss$Int,ss$FinalGPA,method="pearson")
cor.test(ss$Int,ss$FinalGPA, method = "pearson",alt="greater",conf.level=.99)
 model=lm(ss$FinalGPA~ss$Int)
 summary(model)
library(corrplot)
k=cor(data[,7:21])
corrplot(k)
#corrplot(k,method="number")
corrplot(k,method="number",order="AOE")
corrplot.mixed(k,order="AOE")
dim(data)
str(data)
par(mfrow=c(1,1))
pairs(data[,7:21])
mean(ss$FinalGPA,na.rm=T)
m=mean(data$Int, na.rm=T)
#the correlation between final and internal marks
m
model=lm(ss$FinalGPA~ss$Int, data=data)
model
par(mfrow=c(2,2))
plot(model)
summary(model)
library(caTools)
set.seed(112)
split= sample.split(data,SplitRatio=.80)
split
train=subset(data,split=="TRUE")
test=subset(data,split=="FALSE")
str(data)
data=na.omit(data)
data$GPAA=as.factor(data$GPAA)
data$pf=as.factor(data$pf)
names(data)
# corelation between pass/fail with 
#ind=sample(2,nrow(data),replace=TRUE, prob=c(.8,.2))
#train=data[ind==1,]
#test=data[ind==2,]
model=glm(pf~SLC+School+Plus+Physics+Math+Chemistry+Bio+Parent1+Parent2+X1+X2+X3+X4+X5+X6, data=train, family="binomial")
summary(model)
model=glm(pf~SLC+Plus+Physics+Parent1+X1+X2+X4, data=train, family="binomial")
res=predict(model,train,type="response")
res
ress=predict(model,test,type="response")
ress
cmatrix=table(Actual=train$pf,predicted=res>0.5)
cmatrix
accuracy=(cmatrix[[1,1]]+cmatrix[[2,2]])/sum(cmatrix)
accuracy  #100 percent accuracy

cmatrix=table(Actual=test$pf,predicted=ress>0.5)
cmatrix
accuracy=(cmatrix[[1,1]]+cmatrix[[2,2]])/sum(cmatrix)
accuracy  #100 percent accuracy
par(mfrow=c(2,2))
plot(model)

#Similarly progamwise 
data$Prog=as.factor((data$Prog))
str(data)
table(data$Prog)
library(dplyr)
library(tidyverse)
data=tbl_df(data)

k=model.matrix(~data$Prog-1)
k=as.data.frame(k)
k=as_tibble(k)
colnames(k)
names(k)[names(k) == "data$ProgCivil I"] <-"CivilI"
names(k)[names(k) == "data$ProgCivil III"] <-"CivilIII"
names(k)[names(k) == "data$ProgCivil V"] <-"CivilV"
names(k)[names(k) == "data$ProgCivil VII"] <-"CivilVII"
names(k)[names(k) == "data$ProgCivil VIII"] <-"CivilVIII"
names(k)[names(k) == "data$ProgComputer"] <-"Computer"
names(k)[names(k) == "data$ProgElex III"] <-"ElexIII"
names(k)[names(k) == "data$ProgElex V"] <-"ElexV"
names(k)[names(k) == "data$ProgElex VII"] <-"ElexVII"
names(k)[names(k) == "data$ProgELEX VIII"] <-"ElexVIII"
names(k)[names(k) == "data$ProgElexI"] <-"ElexI"
names(k)[names(k) == "data$ProgRural"] <-"Rural"
names(k)[names(k) == "data$ProgSoftware"] <-"Software"
k
summary(k) # with dummy variable
data2=cbind(data,k)
str(data2)
data2=na.omit(data2)
xtabs(~GPAA+Prog, data=data2)
set.seed(222)
data2$GPAA=as.factor(data2$GPAA)
ind=sample(2,nrow(data2),replace=T,prob=c(.8,.2))
train=data2[ind==1,]
test=data2[ind==2,]
train
str(data2)
colnames(data)
#linear model with GPA with dummy predictor which has 
mod=lm(OrdGPA ~CivilI+CivilIII+CivilV+CivilVII+CivilVIII+Computer+ElexIII+ElexV+ElexVII+ElexVIII+ElexI+Rural+Software,data=train)
summary(mod)
# explaiin model in average the intercept has  change incease or decrease
mod=lm(OrdGPA ~CivilI+CivilIII+CivilV+CivilVII+CivilVIII+ElexIII+ElexVII+ElexVIII+ElexI+Rural,data=train)
summary(mod)
train
test
library(caret)
pred=predict(mod,train)#final prediction
tab=table(pred,train$OrdGPA)
tab
1-sum(diag(tab))/sum(tab)## should be similar with
#confusion for test
pred1=predict(mod,test)
tab2=table(pred1,test$OrdGPA)
1-sum(diag(tab2))/sum(tab2)## should be similar with


########################## Explanation 
#Neuralnet concept explain for testing explaining example 
library(keras)
library(tensorflow)
library(tfruns)
library(neuralnet)
library(caret)
library(dplyr)
library(tidyverse)
library(heatmaply)
library(class)
library(e1071)
data <- read.csv("D:/dataanalysisfinal/SOE/rcode/Dataanalysiscsv.csv", stringsAsFactors = FALSE)
# all variable must in numerical  representation the predicted variable in either0 or 1 too
str(data)
data=as.data.frame(data)
dim(data)
hist(data$Math)
#For GPA in 1,2,3,4,5,6,7,0 All
library(tidyverse)
ss=data %>% 
  select(GPAA,SLC,School,Plus,Physics,Math,Chemistry,Bio,Parent1,Parent2,
         X1,X2,X3,X4,X5,X6) 
dim(ss)
head(ss)
GPA=ss[,1]
GPA
Sr=as.data.frame(apply(ss[,2:16], 2, function(x) (x - min(x))/(max(x)-min(x))))
str(Sr)
SS=data.frame(cbind(GPA,Sr))
str(SS)
hist(SS$Math) 
#normalization min-max/min-max
#data$SLC=(data$SLC-min(data$SLC)/max(data$SLC)-min(data$SLC))
#hist(data$SLC)
#data$Plus=(data$Plus-min(data$Plus)/max(data$Plus)-min(data$Plus))
#hist(data$Plus)
#data$Physics=(data$Physics-min(data$Physics)/max(data$Physics)-min(data$Physics))
#data$Math=(data$Math-min(data$Math)/max(data$Math)-min(data$Math))
#data$Chemistry=(data$Chemistry-min(data$Chemistry)/max(data$Chemistry)-min(data$Chemistry))
#data$Bio=(data$Bio-min(data$Bio)/max(data$Bio)-min(data$Bio))
#data$Math2=(data$Math2-min(data$Math2)/max(data$Math2)-min(data$Math2))
#data$int1=(data$int1-min(data$int1)/max(data$int1)-min(data$int1))
#data$int2=(data$int2-min(data$int2)/max(data$int2)-min(data$int2))
#data$int3=(data$int3-min(data$int3)/max(data$int3)-min(data$int3))
#data$int4=(data$int4-min(data$int4)/max(data$int4)-min(data$int4))
#data$int5=(data$int5-min(data$int5)/max(data$int5)-min(data$int5))
#data$int6=(data$int6-min(data$int6)/max(data$int6)-min(data$int6))
#data[,15]=as.numeric(data[,15])-1  #The outout matters when dependent variables in numeric than network converse with SGPA when SGPA become Factor it will converse with its categories like Fail, A grade, B grade and c grade
#data[,1:13]=normalize(data[,1:13]) #normalized all variable at once
head(SS)
levels(as.factor(SS$GPA))
ss$GPA=as.numeric(SS$GPA)
str(SS)
set.seed(123)
ind=sample(2,nrow(SS),replace=T,prob=c(.7,.3))
training=SS[ind==1,1:16]
test=SS[ind==2,1:16]
str(SS)
str(data$GPA)
set.seed(123)
library(neuralnet)
n=neuralnet(GPA~SLC+School+Plus+Physics+Math+Chemistry+Bio+Parent1+Parent2+
              X1+X2+X3+X4+X5+X6,
            data=training,
            hidden=c(1), 
            err.fct="sse",   # Here dependent GPA has 1,2,3,4,5,6,7,0l function for calculation of errors "ce" cross entropy or "sse" sum of square s
            linear.output=FALSE) 
plot(n)
output=neuralnet::compute(n,training[,-1]) # to get confusion  matrix of 5th use rep=5
output$net.result# is probabilities
head(output$net.result)# similar to probability  0.9999458
hist(output$net.result)
head(training[1,])# first records 0.3907611
#calculation if hidden 1
# 0.8333333 0.77777778 0.67567568 0.8653846 0.39473684 0.7608696 1.0000000 0.80 0.82 0.91666667 0.72 0.72 0.64
in14=-0.13406+
  (-0.44306*0.69)+
  (-9.57289*1)+
  (0.08044*0.8333333)+
  (-11.73374*0.8108108)+
  (1.81571*0.5192308)+
  (-11.40211*0.7894737)+
  (-1.33862*0.923913)+
  (-16.48685*0)+
  (-6.13984*0.6666667)+
  (1.17142*0.88)+
  (0.12294*0.78)+
  (5.00518*0.5416667)+
  (-1.24513*0.88)+  #training$int6[1])
  (-2.49769*0.84)+
  (6.549*0.84)
in14   #-26.70309
out4=1/(1+exp(-in14))  #2.529267e-12
out4 # must always lines between 1 and 0 because we used sigmodi function 
SGPI= 9.82221+(-14.23502*out4)
SGPI  #9.82221
SGPAO=1/(1+exp(-SGPI))
SGPAO # 0.9999458   this matched with head first records
#Model evaluation
output=neuralnet::compute(n,training[,-1]) 
p1=output$net.result
p1# the first value is SGPO
par(mfrow=c(1,2))
hist(p1)
pred1=ifelse(p1>=0.9,1,ifelse(p1>=.85,2,
      ifelse(p1>=.75,3,ifelse(p1>=0.65,4,
      ifelse(p1>=0.55,5,ifelse(p1>=0.45,6,ifelse(p1>=.20,7,0)))))))
tab1=table(pred1,training$GPA)
tab1
1-sum(diag(tab1))/sum(tab1) 
output=neuralnet::compute(n,test[,-1]) 
p2=output$net.result
hist(p2)
pred2=ifelse(p2>=0.9,1,ifelse(p2>=.85,2,
      ifelse(p2>=.75,3,ifelse(p2>=0.65,4,
      ifelse(p2>=0.55,5,ifelse(p2>=0.45,6,
                        ifelse(p2>=.20,7,0)))))))
tab2=table(pred2,test$GPA)
tab2
1-sum(diag(tab2))/sum(tab2) 

##############################Only passed std
#For GPA in 1,2,3,4,5,6,7 who were in passed only
data <- read.csv("D:/dataanalysisfinal/SOE/rcode/Dataanalysiscsv.csv", stringsAsFactors = FALSE)
# all variable must in numerical  representation the predicted variable in either0 or 1 too
str(data)
data=as.data.frame(data)
library(tidyverse)
ss=data %>% 
  select(GPAA,GPA,SLC,School,Plus,Physics,Math,Chemistry,Bio,Parent1,Parent2,
         X1,X2,X3,X4,X5,X6) %>% 
  filter(GPA>0)
dim(ss)
head(ss)
GPA=ss[,1]
GPA
Sr=as.data.frame(apply(ss[,2:17], 2, function(x) (x - min(x))/(max(x)-min(x))))
str(Sr)
SS=as.data.frame(cbind(GPA,Sr[,-1]))
str(SS)

str(SS)
hist(SS$Math) 
head(SS)
set.seed(1234)
ind=sample(2,nrow(SS),replace=T,prob=c(.7,.3))
training=SS[ind==1,1:16]
test=SS[ind==2,1:16]
str(SS)
str(SS$GPA)
library(dplyr)
library(neuralnet)
n=neuralnet(GPA~SLC+School+Plus+Physics+Math+Chemistry+Bio+Parent1+Parent2
            +X1+X2+X3+X4+X5+X6,
            data=training,
            hidden=c(1), 
            err.fct="sse",   # Here dependent GPA has 1,2,3,4,5,6,7l function for calculation of errors "ce" cross entropy or "sse" sum of square s
            linear.output=TRUE) 
plot(n)
output=neuralnet::compute(n,training[,-1]) # to get confusion  matrix of 5th use rep=5
output$net.result# is probabilities
head(output$net.result)# similar to probability
head(training[1,])# first records 0.3907611
#calculation if hidden 1
# 0.8333333 0.77777778 0.67567568 0.8653846 0.39473684 0.7608696 1.0000000 0.80 0.82 0.91666667 0.72 0.72 0.64
in14=2.39998+
  (2.08578*0.69)+
  (4.64526*1)+
  (0.25413*0.8285714)+
  (2.83408*0.8108108)+
  (1.0983*0.4047619)+
  (2.69845*0.7894737)+
  (1.2807*0.923913)+
  (0.65258*0)+
  (4.74991*0.6666667)+
  (0.31636*.7777778)+
  (0.93483*0.56)+
  (0.29306*0.5217391)+
  (0.63501*0.7777778)+
  (0.59444*0.7575758)+ 
  (1.36278*0.7647059)
in14#20.82648
out4=1/(1+exp(-in14))
out4 # 1   must always lines between 1 and 0 because we used sigmodi function 
SGPI= 0.21029+(2.43648*out4)
SGPI   #2.64677
SGPAO=1/(1+exp(-SGPI))
SGPAO #   0.9338116  this matched with head first records
#Model evaluation
p1=output$net.result
p1# the first value is SGPO
levels(as.factor(SS$GPA))# "1" "2" "3" "4" "5" "6" "7"
par(mfrow=c(1,2))
hist(p1)
pred1=ifelse(p1>=5.5,1,ifelse(p1>=5,2,
      ifelse(p1>=4.5,3,ifelse(p1>=4.0,4,
      ifelse(p1>=3.5,5,ifelse(p1>=3,6,7))))))
tab1=table(pred1,training$GPA)
tab1
1-sum(diag(tab1))/sum(tab1)

output=neuralnet::compute(n,test[,-1]) 
p2=output$net.result
hist(p2)
pred2=ifelse(p2>=5.5,1,ifelse(p2>=5.0,2,
      ifelse(p2>=4.5,3,ifelse(p2>=4.0,4,
      ifelse(p2>=3.50,5,ifelse(p2>=3.0,6,7))))))
tab2=table(pred2,test$GPA)
tab2
1-sum(diag(tab2))/sum(tab2) 





# plot
set.seed(298346)                     # Create example data
x <- rnorm(300)
y <- rnorm(300) + x
group <- rep(1:2, each = 150)
plot(x, y, pch = 16, col = group)    # Draw plot without legend
legend("topleft",                    # Add legend to plot
       legend = c("Group 1", "Group 2"),
       col = 1:2,
       pch = 16)
data <- data.frame(x = c("A", "B", "C", "D", "E"),            # Create example data
                   y = c(0.3, 0.8, 0.9, 0.2, 0.6),
                   z=c(30,40,20,60,20))
#install.packages("ggplot2")                                   # Install ggplot2 package
library("ggplot2") 
ggp <- ggplot(data, aes(x, z, fill = y)) +                    # ggplot2 with default settings
  geom_bar(stat = "identity")
ggp 

# Main Neuralnet concept explain Main
#Neural Network Result Prediction using Single Layer
library(keras)
library(tensorflow)
library(tfruns)
library(neuralnet)
library(caret)
library(dplyr)
library(heatmaply)
library(class)
library(e1071)
#####################################
#Neural Network Result Prediction using Single Layer
data2 <- read.csv("D:/dataanalysisfinal/SOE/rcode/Dataanalysiscsv.csv",stringsAsFactors = FALSE)
# all variable must in numerical  representation the predicted variable in either0 or 1 too
str(data2)
library(tidyverse)
ss=data2 %>% 
  select(pf,GPA,SLC,School,Plus,Physics,Math,Chemistry,Bio,Parent1,Parent2,
         X1,X2,X3,X4,X5,X6,X111,X222,X333,X444,X555,X666) 
head(ss)
pf=ss[,1]
pf
Sr=as.data.frame(apply(ss[,2:23], 2, function(x) (x - min(x))/(max(x)-min(x))))
str(Sr)
SS=data.frame(cbind(pf,Sr[,-1]))
str(SS)

set.seed(1234)
ind=sample(2,nrow(SS),replace=T,prob=c(.7,.3))
training=SS[ind==1,1:22]
test=SS[ind==2,1:22]
str(SS)
#SS$pf=as.factor(SS$pf)
library(neuralnet)
n=neuralnet(pf~SLC+Plus+Physics+Math+Chemistry+Bio+Parent1+Parent2
            +X1+X2+X3+X4+X5+X6+X111+X222+X333+X444+X555+X666,
            data=training,
            hidden=c(1),
            err.fct="ce",# for binary classification only
            linear.output=FALSE) 

plot(n)# the output depends on dependent variable SGPA type i. e numeric or factor too
names(training)
output=neuralnet::compute(n,training[,-1])
p1=output$net.result
p1
levels(as.factor(SS$pf))
par(mfrow=c(1,2))
hist(p1)
library(caret)
pred1=ifelse(p1>=.4887877,1,0)
tab1=table(pred1,training$pf)
tab1
1-sum(diag(tab1))/sum(tab1)
output=neuralnet::compute(n,test[,-1])
p2=output$net.result
hist(p2)
pred2=ifelse(p2>=.4887877,1,0)
tab2=table(pred2,test$pf)
tab2
1-sum(diag(tab2))/sum(tab2)# the miss classification errors the higher is better


#########################
#Pass   ONly
data2 <- read.csv("D:/dataanalysisfinal/SOE/rcode/Dataanalysiscsv.csv",stringsAsFactors = FALSE)
# all variable must in numerical  representation the predicted variable in either0 or 1 too
str(data2)
library(tidyverse)
ss=data2 %>% 
  select(pf,GPA,SLC,School,Plus,Physics,Math,Chemistry,Bio,Parent1,Parent2,
         X1,X2,X3,X4,X5,X6,X111,X222,X333,X444,X555,X666) %>% 
  filter(GPA>0)
head(ss)
pf=ss[,1]
pf
Sr=as.data.frame(apply(ss[,2:23], 2, function(x) (x - min(x))/(max(x)-min(x))))
str(Sr)
SS=data.frame(cbind(pf,Sr[,-1]))
str(SS)

set.seed(1234)
ind=sample(2,nrow(SS),replace=T,prob=c(.7,.3))
training=SS[ind==1,1:22]
test=SS[ind==2,1:22]
str(SS)
SS$pf=as.factor(SS$pf)
library(neuralnet)
n=neuralnet(pf~SLC+Plus+Physics+Math+Chemistry+Bio+Parent1+Parent2
            +X1+X2+X3+X4+X5+X6+X111+X222+X333+X444+X555+X666,
            data=training,
            hidden=c(1),
            err.fct="ce",# for binary classification only
            linear.output=FALSE) 

plot(n)# the output depends on dependent variable SGPA type i. e numeric or factor too
names(training)
output=neuralnet::compute(n,training[,-1])
p1=output$net.result
p1
levels(as.factor(SS$pf))
par(mfrow=c(1,2))
hist(p1)
library(caret)
pred1=ifelse(p1>=.0,1,0)
tab1=table(pred1,training$pf)
tab1
1-sum(diag(tab1))/sum(tab1)
output=neuralnet::compute(n,test[,-1])
p2=output$net.result
hist(p2)
pred2=ifelse(p2>=.00,1,0)
tab2=table(pred2,test$pf)
tab2
1-sum(diag(tab2))/sum(tab2)# the miss classification errors the higher is better

#note while designed only passed student the model converse and predict 100 accuracy on traiing and test sets.the model evaluation is not possible
################################# Neural Network of All Student GPA Prediction using Single Layer
#Grading 0 for Fail,1 for A grade ,2for  A- Grade,3=B+,4, prediction of passed graded only
data2 <- read.csv("D:/dataanalysisfinal/SOE/rcode/SSeeee.csv",stringsAsFactors = FALSE)
# all variable must in numerical  representation the predicted variable in either0 or 1 too
data2=as.data.frame(data2)
str(data2)
library(tidyverse)
ss=data2 %>% 
  select(GPAX,GPA,SLC,Plus,Physics,Math,Chemistry,Bio,Parent1,Parent2,
         X1,X2,X3,X4,X5,X6,X111,X222,X333,X444,X555,X666)
set.seed(1234)
ind=sample(2,nrow(ss),replace=T,prob=c(.7,.3))
training=ss[ind==1,1:22]
test=ss[ind==2,1:22]
str(ss)
n=neuralnet(GPAX~SLC+Plus+Physics+Math+Chemistry+Bio+Parent1+Parent2+
              X1+X2+X3+X4+X5+X6+X111+X222+X333+X444+X555+X666,
            data=training,
            hidden=c(1),
            err.fct="sse",
            linear.output=TRUE) 
plot(n)# the output depends on dependent variabble SGPA type i. e numeric or factor too
# after seeing repitation minimum errors choose  plot(n,rep=1)
output=neuralnet::compute(n,training[,-1]) # to get confusion  matrix of 5th use rep=5
output$net.result
head(output$net.result)# similar to probability
output
head(training[1,])# first records

output=neuralnet::compute(n,training[,-1])
p1=output$net.result
p1
par(mfrow=c(1,2))
str(ss)
levels(as.factor(ss$GPAX))
hist(p1)
#nested if else
pred1=ifelse(p1>=2.2,1,ifelse(p1>=1.83,2,ifelse(p1>=1.5,3,
      ifelse(p1>=1.2,4,ifelse(p1>=.80,5,ifelse(p1>=.5,6,ifelse(p1>=0.1,7,0)))))))
pred1
head(training$GPAX)# describes label
tab1=table(pred1,training$GPAX)
tab1
training$GPAX
1-sum(diag(tab1))/sum(tab1)# accuracy and miss classification
head(training[,],5)
output=neuralnet::compute(n,test[,-1])
p2=output$net.result
p2
hist(p2)
pred2=ifelse(p2>=2.2,1,ifelse(p2>=1.83,2,ifelse(p2>=1.5,3,
      ifelse(p2>=1.2,4,ifelse(p2>=.80,5,
      ifelse(p2>=.5,6,ifelse(p2>=0.1,7,0)))))))
pred1
tab2=table(pred2,test$GPAX)
tab2
1-sum(diag(tab2))/sum(tab2)# the miss classification errors the higher is better
head(training[,],115) # 115to get all test data


#######################################
#Grading 1" "2" "3" "4" "5" "6" "7" prediction of passed graded only
data2 <- read.csv("D:/dataanalysisfinal/SOE/rcode/SSeeee.csv",stringsAsFactors = FALSE)
# all variable must in numerical  representation the predicted variable in either0 or 1 too
str(data2)
library(tidyverse)
ss=data2 %>% 
  select(GPAX,GPA,SLC,Plus,Physics,Math,Chemistry,Bio,Parent1,Parent2,
         X1,X2,X3,X4,X5,X6,X111,X222,X333,X444,X555,X666) %>% 
  filter(GPA>0)
set.seed(1234)
ind=sample(2,nrow(ss),replace=T,prob=c(.7,.3))
training=ss[ind==1,1:22]
test=ss[ind==2,1:22]
str(ss)
ss$GPAX=as.factor(ss$GPAX)
levels(ss$GPAX)
str(ss)
n=neuralnet(GPAX~SLC+Plus+Physics+Math+Chemistry+Bio+Parent1+Parent2+
              X1+X2+X3+X4+X5+X6+X111+X222+X333+X444+X555+X666,
            data=training,
            hidden=c(1),
            err.fct="sse",
            linear.output=TRUE) 
plot(n)# the output depends on dependent variabble SGPA type i. e numeric or factor too
# after seeing repitation minimum errors choose  plot(n,rep=1)
output=neuralnet::compute(n,training[,-1]) # to get confusion  matrix of 5th use rep=5
output$net.result
head(output$net.result)# similar to probability
output
head(training[1,])# first records

output=neuralnet::compute(n,training[,-1])
p1=output$net.result
p1
par(mfrow=c(1,2))
levels(ss$GPAX)
hist(p1)
#nested if else
pred1=ifelse(p1>=4.6,1,ifelse(p1>=4.8,2,
      ifelse(p1>=3.8,3,ifelse(p1>=3.4,4,
        ifelse(p1>=2.9,5,ifelse(p1>=2.5,6,7))))))

pred1
head(training$GPAX)# describes label
tab1=table(pred1,training$GPAX)
tab1
training$GPAX
1-sum(diag(tab1))/sum(tab1)# accuracy and miss classification
head(training[,],5)
output=neuralnet::compute(n,test[,-1])
p2=output$net.result
p2
hist(p2)
pred2=ifelse(p2>=4.6,1,ifelse(p2>=4.8,2,
      ifelse(p2>=3.8,3,ifelse(p2>=3.4,4,
      ifelse(p2>=2.9,5,ifelse(p2>=2.5,6,7))))))
pred2
tab2=table(pred2,test$GPAX)
tab2
1-sum(diag(tab2))/sum(tab2)# the miss classification errors the higher is better
head(training[,],115) # 115to get all test data



#################################################
# correction
# 1. making  more nodes in hidden  layers N=5 with Fail results
data <- read.csv("D:/dataanalysisfinal/SOE/rcode/SSeeee.csv",stringsAsFactors = FALSE)
# all variable must in numerical  representation the predicted variable in either0 or 1 too
str(data)
dim(data)
library(tidyverse)
ss=data %>% 
  select(GPAA,GPA,SLC,School,Plus,Physics,Math,Chemistry,Bio,Parent1,Parent2,
         X1,X2,X3,X4,X5,X6) 
dim(ss)
set.seed(1234)
ind=sample(2,nrow(ss),replace=T,prob=c(.7,.3))
training=ss[ind==1,1:17]
test=ss[ind==2,1:17]
str(ss)
library(neuralnet)
n=neuralnet(GPAA~SLC+Plus+Physics+Math+Chemistry+Bio+Parent1+Parent2+X1+X2+X3+X4+X5+X6,
            data=training,
            hidden=5,
            err.fct="sse",# "ce for binary classification of predicated variable
            linear.output=FALSE) 
plot(n)#

output=neuralnet::compute(n,training[,-1])
p1=output$net.result
p1
levels(as.factor(ss$GPAA))# "0" "1" "2" "3" "4" "5" "6" "7"
par(mfrow=c(1,2))
hist(p1)
#nested if else
pred1=ifelse(p1>=.999955,1,ifelse(p1>=.99995,2,
      ifelse(p1>=.999945,3,ifelse(p1>=.99994,4,
      ifelse(p1>=.999935,5,ifelse(p1>=.99993,6,
      ifelse(p1>=.999925,7,0)))))))
tab1=table(pred1,training$GPAA)
tab1
1-sum(diag(tab1))/sum(tab1)# accuracy and miss classification
head(training[,],401)
output=neuralnet::compute(n,test[,-1])
p2=output$net.result
hist(p2)
pred2=ifelse(p2>=.999955,1,ifelse(p2>=.99995,2,
      ifelse(p2>=.999945,3,ifelse(p2>=.99994,4,
      ifelse(p2>=.999935,5,ifelse(p2>=.99993,6,
      ifelse(p2>=.999925,7,0)))))))
pred2
tab2=table(pred2,test$GPAA)
tab2
1-sum(diag(tab2))/sum(tab2)# the missclassification errors the higher is better
head(training[,],22) 
###################
#5 hidden with pass student only
data <- read.csv("D:/dataanalysisfinal/SOE/rcode/SSeeee.csv",stringsAsFactors = FALSE)
# all variable must in numerical  representation the predicted variable in either0 or 1 too
str(data)
dim(data)
ss=data %>% 
  select(GPAA,GPA,SLC,School,Plus,Physics,Math,Chemistry,Bio,Parent1,Parent2,
         X1,X2,X3,X4,X5,X6) %>% 
  filter(GPA>0)
dim(ss)
set.seed(1234)
ind=sample(2,nrow(ss),replace=T,prob=c(.7,.3))
training=ss[ind==1,1:17]
test=ss[ind==2,1:17]
str(ss)
library(neuralnet)
n=neuralnet(GPAA~SLC+Plus+Physics+Math+Chemistry+Bio+Parent1+Parent2+X1+X2+X3+X4+X5+X6,
            data=training,
            hidden=5,
            err.fct="sse",# "ce for binary classification of predicated variable
            linear.output=FALSE) 
plot(n)#

output=neuralnet::compute(n,training[,-1])
p1=output$net.result
p1
levels(as.factor(ss$GPAA))# "1" "2" "3" "4" "5" "6" "7"
par(mfrow=c(1,2))
hist(p1)
#nested if else
pred1=ifelse(p1>=.9999808,1,ifelse(p1>=.9999806,2,
      ifelse(p1>=.9999804,3,ifelse(p1>=.9999801,4,
      ifelse(p1>=.9999798,5,ifelse(p1>=.9999796,6,7))))))
tab1=table(pred1,training$GPAA)
tab1
1-sum(diag(tab1))/sum(tab1)# accuracy and miss classification
head(training[,],401)
output=neuralnet::compute(n,test[,-1])
p2=output$net.result
hist(p2)
pred2=ifelse(p2>=.9999802,1,ifelse(p2>=.9999801,2,
      ifelse(p2>=.9999799,3,ifelse(p2>=.9999798,4,
      ifelse(p2>=.9999797,5,ifelse(p2>=.9999796,6,7))))))
pred2
tab2=table(pred2,test$GPAA)
tab2
1-sum(diag(tab2))/sum(tab2)# the missclassification errors the higher is better
head(training[,],81) 



########################################### Neural Network of Two Hidden Layer  (7,3)
# 2making  more nodes in hidden  layers only passed student(7,3) with fail
data2 <- read.csv("D:/dataanalysisfinal/SOE/rcode/SSeeee.csv",stringsAsFactors = FALSE)
# all variable must in numerical  representation the predicted variable in either0 or 1 too
str(data2)
library(tidyverse)
ss=data2 %>% 
  select(GPAA,GPA,SLC,School,Plus,Physics,Math,Chemistry,Bio,Parent1,Parent2,
         X1,X2,X3,X4,X5,X6) 
set.seed(1234)
ind=sample(2,nrow(ss),replace=T,prob=c(.7,.3))
training=ss[ind==1,1:17]
test=ss[ind==2,1:17]
library(neuralnet)
n=neuralnet(GPAA~SLC+Plus+Physics+Math+Chemistry+Bio+Parent1+Parent2+X1+X2+X3+X4+X5+X6,
            data=training,
            hidden=c(7,3),
            err.fct="sse",# "ce for binary classification of predicated variable
            linear.output=FALSE) 
plot(n)#
output=neuralnet::compute(n,training[,-1])
p1=output$net.result
min(p1)
max(p1)
par(mfrow=c(1,2))
hist(p1)
levels(as.factor(ss$GPAA))#"0" "1" "2" "3" "4" "5" "6" "7"
#nested if else
pred1=ifelse(p1>=.999944,1,ifelse(p1>=.999942,2,
      ifelse(p1>=.999943,3,ifelse(p1>=.999940,4,
      ifelse(p1>=.999938,5,ifelse(p1>=.999936,6,
      ifelse(p1>=.999936,6,0)))))))
tab1=table(pred1,training$GPAA)
tab1
training$GPAA
pred1
tab1
1-sum(diag(tab1))/sum(tab1)# accuracy and miss classification
head(training[,],88)

output=neuralnet::compute(n,test[,-1])
p2=output$net.result
hist(p2)
pred2=ifelse(p2>=.999944,1,ifelse(p2>=.999942,2,
      ifelse(p2>=.999943,3,ifelse(p2>=.999940,4,
      ifelse(p2>=.999938,5,ifelse(p2>=.999936,6,
      ifelse(p2>=.999936,6,0)))))))
pred2
tab2=table(pred2,test$GPAA)
tab2
1-sum(diag(tab2))/sum(tab2)# the miss classification errors the higher is better
head(training[,],22) 
################################################ two (7,3)Pass only
# after confusion matrix needs more  correction
# 3 making  more nodes in hidden two (7,3) layers more repitation with back prorogation algorithm
data2 <- read.csv("D:/dataanalysisfinal/SOE/rcode/SSeeee.csv",stringsAsFactors = FALSE)
# all variable must in numerical  representation the predicted variable in either0 or 1 too
str(data2)
library(tidyverse)
ss=data2 %>% 
  select(GPAA,GPA,SLC,Plus,Physics,Math,Chemistry,Bio,Parent1,Parent2,
         X1,X2,X3,X4,X5,X6) %>% 
  filter(GPA>0)
set.seed(1234)
ind=sample(2,nrow(ss),replace=T,prob=c(.7,.3))
training=ss[ind==1,1:16]
test=ss[ind==2,1:16]
library(neuralnet)
n=neuralnet(GPAA~SLC+Plus+Physics+Math+Chemistry+Bio+Parent1+Parent2+
              X1+X2+X3+X4+X5+X6,
            data=training,
            hidden=c(7,3),
            err.fct="sse",# "ce for binary classification of predicated variable
            linear.output=FALSE) 
plot(n)#
output=neuralnet::compute(n,training[,-1])
p1=output$net.result
min(p1)
max(p1)
par(mfrow=c(1,2))
hist(p1)
levels(as.factor(ss$GPAA))#"1" "2" "3" "4" "5" "6" "7"
#nested if else
pred1=ifelse(p1>=.999979389,1,ifelse(p1>=.99997937,2,
      ifelse(p1>=.999979365,3,ifelse(p1>=.99997935,4,
      ifelse(p1>=.99997934,5,ifelse(p1>=.99997932,6,7))))))
tab1=table(pred1,training$GPAA)
tab1
training$GPAA
pred1
1-sum(diag(tab1))/sum(tab1)# accuracy and miss classification
head(training[,],88)
output=neuralnet::compute(n,test[,-1])
p2=output$net.result
hist(p2)
pred2=ifelse(p2>=.999979389,1,ifelse(p2>=.99997937,2,
      ifelse(p2>=.999979365,3,ifelse(p2>=.99997935,4,
      ifelse(p2>=.99997934,5,ifelse(p2>=.9999792,6,7))))))
pred2
tab2=table(pred2,test$GPAA)
tab2
1-sum(diag(tab2))/sum(tab2)# the missclassification errors the higher is better
head(training[,],22) 

###########################################
# with three hidden layer( 7,4,2) With All students
# # 3 making  (7,3,1) in hidden  layers 
# all variable must in numerical  representation the predicted variable in either0 or 1 too
data2 <- read.csv("D:/dataanalysisfinal/SOE/rcode/SSeeee.csv",stringsAsFactors = FALSE)
str(data2)
library(tidyverse)
ss=data2 %>% 
  select(GPAA,SLC,Plus,Physics,Math,Chemistry,Bio,Parent1,Parent2,
         X1,X2,X3,X4,X5,X6,X111,X222,X333,X444,X555,X666) 
set.seed(1234)
str(ss)
ss$GPAA=as.numeric(ss$GPAA)
ind=sample(2,nrow(ss),replace=T,prob=c(.7,.3))
training=ss[ind==1,1:21]
test=ss[ind==2,1:21]
library(neuralnet)
set.seed(112)
?neuralnet()
n=neuralnet(GPAA~SLC+Plus+Physics+Math+Chemistry+Bio+Parent1+Parent2+
              X1+X2+X3+X4+X5+X6,
            data=training,
            hidden=c(7,4,2),
            err.fct="sse", #act.fct="tanh",ce for binary classification of predicated variable
            linear.output=FALSE, #linear.output=TRUE,
            lifesign='full',
            rep=5,
            threshold = 0.01,
            algorithm="rprop+",
            stepmax=10000)
# the lifesign indicates teh printing properties after seeing errors in the model
#the linear.output could not converse when we sets TRUE the more than that the model wont be conversed
plot(n,rep=3) # selection of repitation using converse and minimum errors
#again confusion matrix
output=neuralnet::compute(n,training[,-1], rep=3)
p1=output$net.result
p1
par(mfrow=c(1,2))
hist(p1)
levels(as.factor(ss$GPAA))#"0" "1" "2" "3" "4" "5" "6" "7"
#nested if else
pred1=ifelse(p1>=.99994319,1,ifelse(p1>=.99994317,2,ifelse(p1>=.99994315,3,
      ifelse(p1>=.99994313,4,ifelse(p1>=.99994312,5,ifelse(p1>=.99994311,6,
      ifelse(p1>=.9999332,7,0)))))))
tab1=table(pred1,training$GPAA)
tab1
training$GPAA
pred1
tab1
1-sum(diag(tab1))/sum(tab1)# accuracy and miss classification
head(training[,],88)

output=neuralnet::compute(n,test[,-1], rep=3)
p2=output$net.result
hist(p2)
pred2=ifelse(p2>=.99994319,1,ifelse(p2>=.99994317,2,ifelse(p2>=.99994315,3,
      ifelse(p2>=.99994313,4,ifelse(p2>=.99994312,5,ifelse(p2>=.99994311,6,
                                                                                                                ifelse(p1>=.9999332,7,0)))))))
tab2=table(pred2,test$GPAA)
tab2
1-sum(diag(tab2))/sum(tab2)# the missclassification errors the higher is better
head(test[,],179)
####################### with pass only
# with three hidden layer(7,4,2) With  student
data2 <- read.csv("D:/dataanalysisfinal/SOE/rcode/SSeeee.csv",stringsAsFactors = FALSE)
str(data2)
library(tidyverse)
ss=data2 %>% 
  select(FinalGPA,GPA,SLC,Plus,Physics,Math,Chemistry,Bio,Parent1,Parent2,
         X1,X2,X3,X4,X5,X6,X111,X222,X333,X444,X555,X666) %>% 
  filter(GPA>0)
set.seed(1234)
ss$GPAA=as.numeric(ss$FinalGPA)
ind=sample(2,nrow(ss),replace=T,prob=c(.7,.3))
training=ss[ind==1,1:22]
test=ss[ind==2,1:22]
library(neuralnet)
set.seed(112)
n=neuralnet(FinalGPA~SLC+Plus+Physics+Math+Chemistry+Bio+Parent1+Parent2+
              X1+X2+X3+X4+X5+X6,
            data=training,
            hidden=c(7,4,2),
            err.fct="sse", #act.fct="tanh",ce for binary classification of predicated variable
            linear.output=FALSE, #linear.output=TRUE,
            lifesign='full',
            rep=5,
            threshold = 0.01,
            algorithm="rprop+",
            stepmax=10000)
plot(n,rep=3) # selection of repitation using converse and minimum errors
#again confusion matrix
output=neuralnet::compute(n,training[,-1], rep=3)
p1=output$net.result
p1
par(mfrow=c(1,2))
hist(p1)
levels(as.factor(ss$FinalGPA))#1" "2" "3" "4" "5" "6" "7"
#nested if else
pred1=ifelse(p1>.9,1,ifelse(p1>=.8,2,
      ifelse(p1>=.7,3,ifelse(p1>=.6,4,
      ifelse(p1>=.5,5,ifelse(p1>=.2,6,7))))))
tab1=table(pred1,training$FinalGPA)
tab1
training$FinalGPA
pred1
tab1
1-sum(diag(tab1))/sum(tab1)# accuracy and miss classification
head(training[,],88)

output=neuralnet::compute(n,test[,-1], rep=3)
p2=output$net.result
hist(p2)
pred2=ifelse(p2>.9,1, ifelse(p2>=.8,2,
      ifelse(p2>=.7,3,ifelse(p2>=.6,4,
      ifelse(p2>=.5,5,ifelse(p2>=.2,6,7))))))
tab2=table(pred2,test$FinalGPA)
tab2
1-sum(diag(tab2))/sum(tab2)# the missclassification errors the higher is better
head(test[,],81)


#################################
#Neural network with Original data

data <- read.csv("D:/dataanalysisfinal/SOE/rcode/Dataanalysiscsv.csv", stringsAsFactors = FALSE)

#data <- read.csv("D:/dataanalysisfinal/SOE/rcode/SSeeee.csv",stringsAsFactors = FALSE)

str(data)
data$GPAA=as.numeric(data$GPAA)
data$SLC=as.numeric(data$SLC)
data$Plus=as.numeric(data$Plus)
data$Physics=as.numeric(data$Physics)
data$Math=as.numeric(data$Math)
data$Chemistry=as.numeric(data$Chemistry)
data$Bio=as.numeric(data$Bio)
data$Parent1=as.numeric(data$Parent1)
data$X1=as.numeric(data$X1)
data$X2=as.numeric(data$X2)
data$X3=as.numeric(data$X3)
data$X4=as.numeric(data$X4)
data$X5=as.numeric(data$X5)
data$X6=as.numeric(data$X6)
str(data)
set.seed(1234)
library(tidyverse)
ss=data %>% 
  select(GPAA,SLC,Plus,Physics,Math,Chemistry,Bio,Parent1,X1,X2,X3,X4,X5,X6) #%>% filter(GPAA>0)
data=as.data.frame(ss)
library(neuralnet)
set.seed(112)    
nn=neuralnet(GPAA~SLC+Plus+Physics+Math+Chemistry+Bio+Parent1+X1+X2+X3+X4+X5+X6,
             data=data, 
             hidden=c(7,3,2),
             err.fct="sse", # "sse" hidden layer is sqrt of input
             linear.output=FALSE,
             lifesign='full',
             rep=5,
             algorithm="rprop+",
             stepmax=10000)# because output variable is categorical so output is True when data  in qualitative the default algorithm is rprop+ default activation logistic
nn # describes its converse at with fixed steps the bias terms makes great role to converse with out bias the output become average of input
plot(nn,rep=2)
nn$net.result# output of each replication
str(data)
output=neuralnet::compute(nn,data[,-1],rep=2)
p3=output$net.result#
max(p3)
min(p3)
hist(nn$net.result[[1]])
pred1=ifelse(nn$net.result[[1]]>=.9999623,1,
             ifelse(p3>=.9999620,2, ifelse(p3>=.9999615,3,
                                           ifelse(p3>=.9999610,4,  ifelse(p3>=.9999605,5,
                                                                          ifelse(p3>=.9999603,6,ifelse(p3>=.9999600,7,0)      ))))))
#pred1=ifelse(p3>=.8,1,ifelse(p3>=.7,2, ifelse(p3>=.6,3,ifelse(p3>=.5,4,ifelse(p3>=.4,5,ifelse(p3>=.3,6,ifelse(p3>=.1,7,0)))))))
tab3=table(pred1,ss$GPAA)
tab3
1-sum(diag(tab3))/sum(tab3)
misClassificationError=mean(ss$GPAA != pred1) # the missclassification is  again compared with original values gives percent of errors(remember here the neural net work model may not produce same output when run another time due to random weight initially taken part)
misClassificationError
ss$GPAA
nn
Outputvspred=cbind(ss$GPAA,pred1) # seeing side by side in reality but model produces
Outputvspred
new.output=neuralnet::compute(nn,covariate = matrix(c(79,0,0,1,1,1,1,1,1,1,1,1,1,
                                                      90,1,1,1,1,1,1,1,1,1,1,1,1,
                                                      50,1,1,1,1,1,1,1,1,1,0,0,0,
                                                      50,0,0,0,0,0,0,0,0,0,0,0,0),
                                                    byrow = TRUE,
                                                    ncol = 13))  #of four variables

new.output$net.result
#Back prorogation the learning rate is hypermater if we take larger it will converse quickly
nn.bp=neuralnet(GPAA~SLC+Plus+Physics+Math+Chemistry+Bio+Parent1+X1+X2+X3+X4+X5+X6,
                data=ss, 
                hidden=c(7,3,2),
                learningrate=0.01,
                algorithm='backprop',# back prorogation will converse very fast
                err.fct="sse",
                linear.output=FALSE)# 
nn.bp# describes its error rate threshold and steps
t.test(ss$GPAA)
plot(nn.bp)# same output. the test variable in matrix the input variable age,pariety indued and spontanous of all alternatives
nn.bp$covariate # describes the input information
#max: 1.75833,1.0513,1.80523,1.89084,2.1164,1.9588,1.0367,0.8717,0.7494,1.49984,1.9816,1.7435,1.977383
#min:-2.96252,-0.9426,-2.41896,-2.51074,-1.7471,-2.0809,-1.8036,-1.6298,-2.4624,-2.33367,-3.1066,-3.068,1.977383
summary(nn.bp$covariate)
nn.bp$covariate
output=neuralnet::compute(nn,data[,-1])
p3=output$net.result#
max(p3)
min(p3)
hist(nn$net.result[[1]])
pred1=ifelse(nn$net.result[[1]]>=.9999566,1,
             ifelse(p3>=.9999564,2, ifelse(p3>=.9999562,3,
                                           ifelse(p3>=.9999550,4,  
                                          ifelse(p3>=.9999545,5,
                                          ifelse(p3>=.9999542,6,
                                          ifelse(p3>=.9999539,7,0)      ))))))
#pred1=ifelse(p3>=.8,1,ifelse(p3>=.7,2, ifelse(p3>=.6,3,ifelse(p3>=.5,4,ifelse(p3>=.4,5,ifelse(p3>=.3,6,ifelse(p3>=.1,7,0)))))))
tab3=table(pred1,ss$GPAA)
tab3
1-sum(diag(tab3))/sum(tab3)
misClassificationError=mean(ss$GPAA != pred1) # the missclassification is  again compared with original values gives percent of errors(remember here the neural net work model may not produce same output when run another time due to random weight initially taken part)
misClassificationError
ss$GPAA
nn
Outputvspred=cbind(ss$GPAA,pred1) # seeing side by side in reality but model produces
Outputvspred

new.output=neuralnet::compute(nn.bp,covariate = matrix(c(1,1,1,1,1,1,1,1,1,1,1,1,1,
                                                         1,1,1,1,1,1,1,1,1,1,1,1,1,
                                                         1,1,1,1,1,1,1,1,1,1,1,1,1,
                                                         1,1,6,1,0,0,0,1,1,7,1,1,1),
                                                       byrow = TRUE,
                                                       ncol = 13))  #of four variables

new.output$net.result# interpretation result with percentage of above  input values
#confidence interval

ci=t.test(data$GPAA,conf.level=0.90) #  the lower and upper bound of confidence interval
ci

#max: 1.75833,1.0513,1.80523,1.89084,2.1164,1.9588,1.0367,0.8717,0.7494,1.49984,1.9816,1.7435,1.977383
#min:-2.96252,-0.9426,-2.41896,-2.51074,-1.7471,-2.0809,-1.8036,-1.6298,-2.4624,-2.33367,-3.1066,-3.068,1.977383
summary(data)
par(mfrow=c(2,4))
library(ggplot2)
gwplot(nn,selected.covariate="SLC",min=-2.96252, max=1.75833)
gwplot(nn,selected.covariate="Plus",min=-0.9426, max=1.0513)
gwplot(nn,selected.covariate="Physics",min=-2.41896, max=1.80523)
gwplot(nn,selected.covariate="Math",min=-2.51074, max=1.89084)
gwplot(nn,selected.covariate="Chemistry",min=-1.7471, max=2.1164)
gwplot(nn,selected.covariate="Bio",min=-2.0809, max=1.9588)
gwplot(nn,selected.covariate="Parent1",min=-1.8036, max=1.0367)
gwplot(nn,selected.covariate="X1",min=-1.6298, max=0.8717)
gwplot(nn,selected.covariate="X2",min=-2.4624, max=0.7494)
gwplot(nn,selected.covariate="X3",min=-2.33367, max=1.49984)
gwplot(nn,selected.covariate="X4",min=-3.1066, max=1.9816)
gwplot(nn,selected.covariate="X5",min=-3.068, max=1.7435)
gwplot(nn,selected.covariate="X6",min=1.977383, max=1.977383)


########################################################
#Five Hidden Layer (25,12,6,3)Neural Network with  Back Prorogation using GPA
data <- read.csv("D:/dataanalysisfinal/SOE/rcode/Dataanalysiscsv.csv", stringsAsFactors = FALSE)

#data <- read.csv("D:/dataanalysisfinal/SOE/rcode/SSeeee.csv",stringsAsFactors = FALSE)
str(data)
data$GPAA=as.numeric(data$GPAA)
data$SLC=as.numeric(data$SLC)
data$Plus=as.numeric(data$Plus)
data$Physics=as.numeric(data$Physics)
data$Math=as.numeric(data$Math)
data$Chemistry=as.numeric(data$Chemistry)
data$Bio=as.numeric(data$Bio)
data$Parent1=as.numeric(data$Parent1)
data$X1=as.numeric(data$X1)
data$X2=as.numeric(data$X2)
data$X3=as.numeric(data$X3)
data$X4=as.numeric(data$X4)
data$X5=as.numeric(data$X5)
data$X6=as.numeric(data$X6)

set.seed(1234)
library(tidyverse)
ss=data %>% 
  select(GPAA,SLC,Plus,Physics,Math,Chemistry,Bio,Parent1,X1,X2,X3,X4,X5,X6) %>%filter(GPAA>0)
data=ss
library(neuralnet)
set.seed(112) 
str(ss)
ss$GPAA=as.numeric(ss$GPAA)
nn=neuralnet(GPAA~SLC+Plus+Physics+Math+Chemistry+Bio+Parent1+X1+X2+X3+X4+X5+X6,
             data=data, 
             hidden=c(25,12,6,3),
             err.fct="sse", # "sse" hidden layer is sqrt of input
             linear.output=FALSE,
             lifesign='full',
             rep=5,
             algorithm="rprop+",
             stepmax=10000) #because output variable is categorical so output is True when data  in qualitative the default algorithm is rprop+ default activation logistic
nn # describes its converse at with fixed steps the bias terms makes great role to converse with out bias the output become average of input
plot(nn,rep=3)
nn$net.result# output of each replication
str(data)
output=neuralnet::compute(nn,ss[,-1],rep=3)
p3=output$net.result
hist(p3)
max(p3)
min(p3)

pred1=ifelse(p3>=.99998318,1,ifelse(p3>=.99998317,2,
      ifelse(p3>=.99998316,3,ifelse(p3>=.99998315,4,
      ifelse(p3>=.99998314,5,ifelse(p3>=.99998313,6,7))))))
tab3=table(pred1,ss$GPAA)
tab3
1-sum(diag(tab3))/sum(tab3)

misClassificationError=mean(ss$GPAA != pred1) # the missclassification is  again compared with original values gives percent of errors(remember here the neural net work model may not produce same output when run another time due to random weight initially taken part)
misClassificationError
ss$GPAA
Outputvspred=cbind(ss$GPAA,pred1) # seeing side by side in reality but model produces
Outputvspred
new.output=neuralnet::compute(nn,covariate = matrix(c(1,1,1,1,1,1,1,1,1,1,1,1,1,
                                                      1,1,1,1,1,1,1,1,1,1,1,1,1,
                                                      1,1,1,1,1,1,1,1,1,1,1,1,1,
                                                      0,0,0,1,0,0,0,1,1,7,1,1,1),                                                    byrow = TRUE,
                                                    ncol = 13))  #of four variables

new.output$net.result

###########################################
       

          #####ENDED ENDED



data <- read.csv("D:/dataanalysisfinal/SOE/rcode/prog.csv")
dd=data

str(data)
  for(i in 7: 21){
    data[,i]=(data[,i]-min(data[,i]))/(max(data[,i])-min(data[,i]))
  }
str(data)
ind=sample(1:nrow(data),80)
train_data=data[ind,]
test_data=data[-ind,]
str(data)
library(neuralnet)
n=neuralnet(pf~ SLC+Plus+Physics+Math+Chemistry+Bio+Parent1+X1+X2+X3+X4+X5+X6,
            data=train_data,
            hidden=3,
            linear.output=FALSE)
plot(n)
str(data)
output=neuralnet::compute(n,test_data[,-1:-6])
prediction=output$net.result*(max(data[-ind,21])-min(data[-ind,21])+min(data[-ind,21]))
prediction
actual=data[-ind,21]
actual
MSE=sum((prediction-actual)^2)/nrow(test_data)
MSE
table(round(actual),round(prediction))

str(dd)
dd$GPAOR=as.integer(as.factor(dd$GPAOR))
str(dd)
m=cor(dd)
library(corrplot)
corrplot(m,method="circle")
corrplot(m,method="pie")
corrplot(m,method="number")
corrplot(m,method="color",col=col(200),type="upper",order="hclust",)
corrplot(m,method="circle")
#barplot
attatch(dd)
str(dd)
counts=table(dd$Total)
?table
counts=13
for(i in 2: 9){
  dd[,i]=(dd[,i]-min(data[,i]))/(max(data[,i])-min(data[,i]))

  }
barplot(counts, main="Simple batplot",
        xlab="improvement",
        ylab="frequency",
        legned=rownames(counts),
        col=c("red","green","yellow","pink"))
#
counts=table(mtcars$gear,mtcars$vs)
barplot(counts, main="Car distribution",
        xlab="improvement",
        ylab="frequency",
        legned=rownames(counts),
        beside = TRUE)

library(plotrix)
slices=c(12,8,3,4)
pct=round(slices/sum(slices)*100)
lbls=paste(c("us","uk","Nepal","china"),
           "",pct,"%",sep="")
pie3D(slices,labels=lbls,explode=0.0,main="3D Pie")

#histogram
hist(mtcars$mpg, breaks=8,col="darkgreen")
density= density(mtcars$mpg)
plot(density, main="total plots")
plot(density, col="skyblue",border="black")
plot(density(x))

ggplot(titanic,aes(x=Age,fill=survied))+
  theme_bw()+
  facet_warp(sex~Pclass)+
  geom_density(alpha=0.5)+
  labs(y="Age",
       x="survived",
       title="titanic")