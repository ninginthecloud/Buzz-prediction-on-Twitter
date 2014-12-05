library(neuralnet)
library(nnet)
library(e1071)
library(pROC)
load(file="E:\\UW\\autumn 2014\\STAT535\\project\\train_stand.RData")
#BUZZ
BUZZ=Data[Data$Y==1,]
NONBUZZ=Data[Data$Y!=1,]
dim(BUZZ)#856  78
dim(NONBUZZ)# 99144    78
##########
#FUNCTION
#ROC
roc<-function(prob,Y){
	N<-length(prob)
	location<-order(prob)
	result<-prob[location]
	Y.sort<-Y[location]
	TP<-NULL;
	FP<-NULL;
	for(i in 1:(N+1)){
		temp<-c(rep(0,i-1),rep(1,N-i+1))
		TP<-c(TP,sum((temp==1)&(Y.sort==1))/sum(Y.sort));
		FP<-c(FP,sum((temp==1)&(Y.sort==0))/sum(!Y.sort));
		}
	points<-cbind(c(result,1),TP,FP)
	return(points)
}
resampling<-function(group,ratio,BUZZ,NONBUZZ,seed){
	set.seed(seed);
	size.B=dim(BUZZ)[1];
	size.NB=dim(NONBUZZ)[1];
	Total<-size.B+size.NB
	sample.B=round(Total/(1+ratio));
	sample.NB=round(Total*ratio/(1+ratio));
	
	resamples<-lapply(1:group,function(i){rbind(BUZZ[sample.int(size.B,sample.B,replace=T),],NONBUZZ[sample.int(size.NB,sample.NB,replace=T),])})
	return(resamples)
}
#######################33
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
TF=(sum((result>0.5)&(Y.train==0)))/sum((Y.train==0));TF
FT=(sum((result<=0.5)&(Y.train==1)))/sum((Y.train==1));FT
mean(result>0.5)

rocresult<-roc(result,Y.train)




set.seed(535)
try<-Data[sample(1:100000,10000),]
mean(try$Y==1)# 0.0082
train<-Data[1:8000,];
mean(train$Y==1)#0.008125
t.BUZZ=train[train$Y==1,]
t.NONBUZZ=train[train$Y!=1,]
test<-Data[-(1:8000),];
mean(test$Y==1)# 0.008597826
sample.data<-resampling(10,1,t.BUZZ,t.NONBUZZ,seed=535)
length(sample.data)


for(i in 1:10){
	model=nnet(x=sample.data[[i]][,-78],y=sample.data[[i]][,78],size=11)	
}


############
#svm
sample.data<-resampling(5,1,t.BUZZ,t.NONBUZZ,seed=535)
s<-0;
TP1<-NULL;FP1<-NULL;
TP2<-NULL;FP2<-NULL;
for(i in 1:5){
	model=svm(x=sample.data[[i]][,-78],y=sample.data[[i]][,78],type="C-classification",kernel="radial",cost=100,gamma=0.1)
	temp=predict(model,sample.data[[i]][,-78],decision.values=TRUE)
	train=attr(temp,"decision.values")
	#TP
TP1<-c(TP1,sum((train>0)*(sample.data[[i]][,78]==1))/sum(sample.data[[i]][,78]));TP1# 0.863464
	#FP
FP1<-c(FP1,sum((train>0)*(sample.data[[i]][,78]==0))/sum(sample.data[[i]][,78]==0));FP1# 0.05082832

	fit=predict(model,test[,-78],decision.values = TRUE)
	s<-s+attr(fit,"decision.values")
	#TP
TP2<-c(TP2,sum((s/i>0)*(test[,78]==1))/sum(test[,78]));TP2
#FP
FP2<-c(FP2,sum((s/i>0)*(test[,78]==0))/sum(test[,78]==0));FP2
	#i<-i+1
}

#TP
sum((s/i>0)*(test[,78]==1))/sum(test[,78])# 0.863464
#FP
sum((s/5>0)*(test[,78]==0))/sum(test[,78]==0)# 0.05082832



X.train<-Data[1:80000,-78]
X.valid<-Data[80001:100000,-78]
Y.train<-Data[1:80000,78]
Y.valid<-Data[80001:100000,78]


model=svm(X.train,Y.train,type="C-classification",kernel="radial",cost=10,gamma=0.1)
#temp=predict(model,sample.data[[i]][,-78],decision.values=TRUE)
#train=attr(temp,"decision.values")
orginal<-predict(model,X.train,decision.values=TRUE)
valid<-predict(model,X.valid,decision.values=TRUE)
train.result<-attr(original,"decision.values")
valid.result<-attr(valid,"decision.values")
auc(Y.valid,valid.result)

a<-NULL;
valid.result<-0
for(i in 1:10){
location<-sample.int(80000,replace=TRUE)
X.new<-X.train[location,]
Y.new<-Y.train[location]
model=svm(X.new,Y.new,type="C-classification",kernel="radial",cost=10,gamma=0.1)
valid<-predict(model,X.valid,decision.values=TRUE)
valid.result<-valid.result+attr(valid,"decision.values")
a<-c(a,auc(Y.valid,valid.result/i))
print(a)
print(i)
i=i+1
}





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




############
#ROC
library(pROC)
prob<-read.table("f2.out")
#P<-prob[80001:100000,2]
#Y<-Data[80001:100000,78]
output=roc(yali,data.matrix(prob))
auc(output)
plot.roc(output)
ci.thresholds(Y,P)

mean(prob>=0.5)

Test<-read.table("test.txt")
write.table(Test,file="mytest.txt",row.names = FALSE,col.names = FALSE)
mytest<-read.table("mytest.txt")
total<-read.table("E:\\UW\\autumn 2014\\STAT535\\project\\Buzz-prediction-on-Twitter\\classification\\classification\\Twitter\\Relative_labeling\\sigma=1000\\Twitter-Relative-Sigma-1000.data",sep = ",")
yali<-Test[,1:10]
original<-total[,c(1:77,78)]

rowMatch <- function(A,B) {
# Rows in A that match the rows in B
# The row indexes correspond to A
    f <- function(...) paste(..., sep=":")
   if(!is.matrix(B)) B <- matrix(B, 1, length(B))
    a <- do.call("f", as.data.frame(A))
    b <- do.call("f", as.data.frame(B))
    match(b, a)
} 

location<-rep(NA,dim(yali)[1])
for(i in 1:100){
	location[i]<-rowMatch(original[,1:10],yali[i,])
	}
for(i in 9901:10000){
	location[i]<-rowMatch(original[,1:10],yali[i,])
	}

BUZZ<-total[total[,78]==1,]
#1177   78
location<-rep(NA,dim(BUZZ)[1])
for(i in 1:dim(BUZZ)[1]){
	location[i]<-rowMatch(Test[1:10],BUZZ[i,1:10])
	}
	
select.Test=which(!is.na(location))
select.yali=location[!is.na(location)]
yali<-rep(0,dim(Test)[1])
yali[select.yali]<-1

write.table(Test[select.yali,],"haha.txt",row.names=FALSE,col.names=FALSE)
write.table(yali,"yali.txt",row.names=FALSE,col.names=FALSE)
write.table(BUZZ[select.Test,],"BUZZ.txt",row.names=FALSE,col.names=FALSE)


	
	
	

#standardlize
Data<-NULL;
wkdata<-data.matrix(Test)
for(i in seq(1,77,7))
{
	temp<-wkdata[,i:(i+6)];
	Data<-cbind(Data,temp/max(temp))
}
write.table(Data,file="test_stand.txt",col.name=FALSE,row.names=FALSE)	
write.table(Data[1:80000,-78],file="X.txt",col.name=FALSE,row.names=FALSE)
write.table(Data[80001:100000,78],file="labels.txt",col.name=FALSE,row.names=FALSE)	







#deep and naive
deep<-read.table("multi.out")
naive<-read.table("simple.out")

deep<-sort(data.matrix(deep))
quantile(deep,0.95)#0.01669589 
naive<-sort(data.matrix(naive))
quantile(naive,0.95)#0.01559793
data1<-data.frame(deep=deep[19501:20000],shallow=naive[19501:20000])

library(reshape)
library(ggplot2)
y<-melt(as.matrix(data1))

p <- ggplot(y, aes(y=X1,x=X2)) + geom_tile(aes(fill=value)) + scale_fill_gradient(low="white", high="darkblue") + xlab("") + ylab("")+scale_y_discrete(breaks=seq(0,500,100), labels=c("0.00","0.20","0.40","0.60","0.80","1.00"))+theme(text = element_text(size=20))

pdf<-pdf("contrast.pdf",width=8,height=6)
p
dev.off()

p


opts(axis.title.x=theme_blank()) 

ggplot(data=data1,)




write.table(deep,file="deep.txt",col.name=FALSE,row.names=FALSE)
write.table(naive,file="naive.txt",col.name=FALSE,row.names=FALSE)	


boxplot(naive)
hist(data.matrix(naive))