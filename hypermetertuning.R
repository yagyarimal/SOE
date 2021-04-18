data=cbind(66,66)
colnames(data)=c("Input","Output")
View(data)
library(neuralnet)
mod=neuralnet(formula=Output~Input,
              data=data,
              hidden=2,
              threshold=0.01,
              lifesign = "full",
              lifesign.step = 10)
plot(mod)

#ann
traininginput=as.data.frame(runif(50,min=0,max=225))
trainingoutput=sqrt(traininginput)
data=cbind(traininginput,trainingoutput)
colnames(data)=c("Input","Output")
View(data)
library(neuralnet)
mod=neuralnet(formula=Output~Input,
              data=data,
              hidden=10,
              threshold=0.01,
              lifesign = "full",
              lifesign.step = 10)
plot(mod)

library(keras)
library(mlbench)
library(dplyr)
library(MASS)
library(magrittr)
library(neuralnet)
data("BostonHousing")
data=BostonHousing

?BostonHousing
str(data)
data%<>%mutate_if(is.factor,as.numeric)#converting factor to numeric
n=neuralnet(medv~crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+b+lstat,
            data=data,
            hidden=c(10,5),
            linear.output=F,
            lifesign='full',
            rep=1)
plot(n, col.hidden='darkgreen',
     col.hidden.synapse = 'darkgreen',
     show.weight=T,
     information=F,
     fill='lightblue')
#matrix
data=as.matrix(data)
dimnames(data)=NULL
#partition
set.seed(1234)
ind=sample(2,nrow(data),replace=T, prob=c(.7,.3))
training=data[ind==1,1:13]
test=data[ind==2,1:13]
trainingtarget=data[ind==1,14]#independent
testtarget=data[ind==2,14]#independent 
#normalization
m=colMeans(training)
s=apply(training,2,sd)
training=scale(training,center = m, scale = s)
test=scale(test,center = m, scale = s)
#model
library(tensorflow)
install_tensorflow(version = "gpu")
model=keras_model_sequential()
model%>%
  layer_dense(units=5,activation = 'relu',input_shape =c(13)) %>% 
  layer_dense(units=1)#default output activation Linear activation
summary(model)
 #compile Y is numeric there is regression problem
model %>% compile(loss='mse',
                  optimizer='rmsprop', #default
                  metrics='mae')# mean absolute error
#fit
mymodel=model %>% 
  fit(training,trainingtarget,
      epochs=100,
      batch_size=32,
      validation_split=0.2)#20% data validation loss for outof sample 
#evaluation
model %>% 
  evaluate(test,testtarget)
pred=model %>% predict(test)
mean((testtarget-pred)^2)
 plot(testtarget,pred)
#fine tune improvement
model=keras_model_sequential()
model%>%
  layer_dense(units=10,activation = 'relu',input_shape =c(13)) %>% 
  layer_dense(units=5,activation = 'relu') %>% 
  layer_dense(units=1)#default activation Linear activation
summary(model)
 #compile Y is numeric there is regression problem
model %>% compile(loss='mse',
                  optimizer='rmsprop', #default
                  metrics='mae')# mean absolute error
#fit
mymodel=model %>% 
  fit(training,trainingtarget,
      epochs=100,
      batch_size=32,
      validation_split=0.2)
#evaluation
model %>% 
  evaluate(test,testtarget)
pred=model %>% predict(test)
mean((testtarget-pred)^2)
plot(testtarget,pred)
#fine tune improvement for improvement
model=keras_model_sequential()
model%>%
  layer_dense(units=100,activation = 'relu',input_shape =c(13)) %>% 
  layer_dropout(rate=0.4) %>%      # 40 % neurons were being ignored while model
  layer_dense(units=50,activation = 'relu') %>% 
  layer_dropout(rate=0.3) %>% 
  layer_dense(units=20,activation = 'relu') %>% 
  layer_dropout(rate=0.2) %>% 
  layer_dense(units=1)#default activation Linear activation

summary(model)
#compile Y is numeric there is regression problem
model %>% compile(loss='mse',
                  optimizer=optimizer_rmsprop(lr=0.001), #rmsprop', #default
                  metrics='mae')# mean absolute error
#fit
mymodel=model %>% 
  fit(training,trainingtarget,
      epochs=100,
      batch_size=32,
      validation_split=0.2)
#evaluation
model %>% 
  evaluate(test,testtarget)
       pred=model %>% predict(test)
mean((testtarget-pred)^2)
plot(testtarget,pred)

# hypermeter tuinig: according wikipedia in machine learning, hypermeter optimization is the p roblem of chosing a set of optmal hypermerers for learing alroritn which  control the lerning process with the valuue of other paramer learned.
setwd("D:/dataanalysisfinal/SOE/rcode/") 
getwd()

devtools::install_github("rstudio/tensorflow",dependencies = TRUE)
install_tensorflow()
library(reticulate)
library(tensorflow)
devtools::install_github("r-lib/crayon")
devtools::install_github("rstudio/keras",dependencies = TRUE)
library(keras)
install_keras()
library(tfruns)

data <- read.csv("D:/dataanalysisfinal/SOE/rcode/SSS.csv") 
str(data)
data=na.omit(data)
is.na(data)
data=as.matrix(data)# convertion matrix
dimnames(data)=NULL# making v1 v2
str(data)
#normalization
data[,1:13]=normalize(data[,1:13]) #normalized
data[,14]=as.numeric(data[,14])-1  #NSP to int to numeric

str(data)
#data partation
set.seed(1234)
ind=sample(2,nrow(data),replace=T,prob=c(.7,.3))
training=data[ind==1,1:13]
test=data[ind==2,1:13]
traintarget=data[ind==1,14]
testtarget=data[ind==2,14] 

#onehot encoding
trainlabels= to_categorical(traintarget)
testlabels= to_categorical(testtarget)
str(data)

runs=tuning_run("experiment1.R",
                flags=list(dense_units=c(32,64)))

runs=tuning_run("experiment2.R",flags=list(dense_units1=c(32,64),dense_units2=c(16,32),dropout1=c(0.1,0.2),dropout2=c(0.1,0.2),batch_size=c(32,64))) # 2^5=32 parameters  in five layers
#best hypermeter values
head(runs)
runs[,c(5,6)]
results=runs[,c(3,5:10)] 
results
