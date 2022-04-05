library(rio)
library(tidyverse)
library(factoextra)
library(gridExtra)
pcadat<-read.csv("study4_subjectiveRatings_LONG.csv")
pcadat.w<-pcadat %>% pivot_wider(names_from="emotion",values_from="score")
pcadat.pc<-prcomp(pcadat.w %>% select(-time,-id),scale=TRUE)
emotePlot<-fviz_pca_var(pcadat.pc,label="var",col.ind=pcadat.w$anger)
# sort(pcadat.pc,decreasing=TRUE)
# pcadat.pc$x[,1]
pcadat.w$pc1<-pcadat.pc$x[,1]
pcadat.w$pc2<-pcadat.pc$x[,2]
time.v<-pcadat.w %>% filter(id==100)

samples<-sample(pcadat.w$id,4)
pcadat.pc.1<-prcomp(pcadat.w %>% filter(id==samples[1]) %>% select(-time,-id),scale=TRUE)
emotePlot.1<-fviz_pca_biplot(pcadat.pc.1,label="var",col.ind=time.v$time)
pcadat.pc.2<-prcomp(pcadat.w %>% filter(id==samples[2]) %>% select(-time,-id),scale=TRUE)
emotePlot.2<-fviz_pca_biplot(pcadat.pc.2,label="var",col.ind=time.v$time)
pcadat.pc.3<-prcomp(pcadat.w %>% filter(id==samples[3]) %>% select(-time,-id),scale=TRUE)
emotePlot.3<-fviz_pca_biplot(pcadat.pc.3,label="var",col.ind=time.v$time)
pcadat.pc.4<-prcomp(pcadat.w %>% filter(id==samples[4]) %>% select(-time,-id),scale=TRUE)
emotePlot.4<-fviz_pca_biplot(pcadat.pc.4,label="var",col.ind=time.v$time)
grid.arrange(emotePlot.1,emotePlot.2,emotePlot.3,emotePlot.4,nrow=2)
pcadat.pc.1
pcadat.pc.2

pcadat.w.s<-pcadat.w %>% group_by(time) %>% summarize(anger=mean(anger),contempt=mean(contempt),
                                                    disgust=mean(disgust),elation=mean(elation),
                                                    envy=mean(envy),fear=mean(fear),guilt=mean(guilt),
                                                    hope=mean(hope),interest=mean(interest),joy=mean(joy),
                                                    pride=mean(pride),relief=mean(relief),sadness=mean(sadness),
                                                    satisfaction=mean(satisfaction),shame=mean(shame),surprise=mean(surprise))

pcadat.pc.s<-prcomp(pcadat.w.s %>% select(-time),scale=TRUE)
emotePlot.s<-fviz_pca_biplot(pcadat.pc.s,label="var",col.ind=pcadat.w.s$time)

pcaplot.s<-ggplot(data=pcadat.w,aes(y=-pc2,x=time,color=-pc1))+
  geom_point()+
  ylab("Valence")

grid.arrange(emotePlot,pcaplot.s)
View(pcadat.w.s)
finalDat<-pcadat.w %>% group_by(time) %>% summarize(pc1=mean(pc1),
                                                    pc2=mean(pc2))
write.csv(pcadat.w %>% select(id,time,pc1,pc2),"subjectivityTimeCourse-ind.csv",row.names=FALSE)
write.csv(finalDat,"subjectivityTimeCourse.csv",row.names=FALSE)
