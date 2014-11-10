#read data
wkdata<-read.table("E:\\UW\\autumn 2014\\STAT535\\project\\train.txt")
dim(wkdata)
str(wkdata)#100000 78
mean(wkdata==0)
#change name
name.h<-c("NCD","AI","AS(NA)","BL","NAC","AS(NAC)","CS","AT","NA","ADL","NAD")
name.f<-NULL;
for(start in name.h){
name.f<-c(name.f,paste(start,c(0:6),sep="_"))
}

colnames(wkdata)<-c(name.f,"Y")