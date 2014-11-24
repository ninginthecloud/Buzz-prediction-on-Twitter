library(neuralnet)
library(nnet)
library(e1071)
load(file="E:\\UW\\autumn 2014\\STAT535\\project\\train.RData")
dim(BUZZ)#856  78
dim(NONBUZZ)# 99144    78
##########
#FUNCTION
#ROC
roc<-function(prob,Y){
	N<-length(prob)
	location<-order(prob)
	result<-prob(location)
	Y.sort<-Y(location)
	point<-NULL;
	for(i in 1:(N+1)){
		temp<-c(rep(0,i-1),rep(1,N-i+1))
		temp==0)
	}

}
try=rbind(BUZZ,NONBUZZ[sample(1:99144,856*70),])
dim(try)
N=dim(try)[1]
X=try[,-78]
Y=try[,78]
###########
#neural network
size=dim(X)[1]
select=sample(1:size,size*.8)
X.train=X[select,]
X.valid=X[-select,]
Y.train=Y[select]
Y.valid=Y[-select]
model=nnet(x=X.train,y=Y.train,weights=Y.train*100+1,size=11)
result=model$fitted.values
N=length(Y.train)
TF=(sum((result>0.5)&(Y.train==0)))/N;TF
FT=(sum((result<=0.5)&(Y.train==1)))/N;FT
mean(result>0.5)



reconstruct<-function(ratio,BUZZ,NONBUZZ,buzzsize,seed){
	set.seed(seed);
	X=BUZZ[sample(1:dim(),),]



}


############
#svm
model=svm(X.train,Y.train,type="C-classification")
result.r=predict(model,X.train)
result.v=predict(model,X.valid)
N.t<-length(Y.train)
N.v<-length(Y.valid)
=(sum((result.r==1)&(Y.train==0)))/N.t;TF
FT=(sum((result.r==0)&(Y.train==1)))/N.t;FT
mean(result.r==1)

TF=(sum((result.v==1)&(Y.valid==0)))/N.v;TF
FT=(sum((result.v==0)&(Y.valid==1)))/N.v;FT
mean(result.v==1)
