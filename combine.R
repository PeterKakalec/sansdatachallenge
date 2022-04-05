library(tidyverse)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

angerDat<-read.csv("FNL_EmotionCF_Anger_NNMF_SGD_Dilate60_v3.csv")
contemptDat<-read.csv("FNL_EmotionCF_Contempt_NNMF_SGD_Dilate60_v3.csv")
disgustDat<-read.csv("FNL_EmotionCF_Disgust_NNMF_SGD_Dilate60_v3.csv")
elationDat<-read.csv("FNL_EmotionCF_Elation_NNMF_SGD_Dilate60_v3.csv")
envyDat<-read.csv("FNL_EmotionCF_Envy_NNMF_SGD_Dilate60_v3.csv")
fearDat<-read.csv("FNL_EmotionCF_Fear_NNMF_SGD_Dilate60_v3.csv")
guiltDat<-read.csv("FNL_EmotionCF_Guilt_NNMF_SGD_Dilate60_v3.csv")
hopeDat<-read.csv("FNL_EmotionCF_Hope_NNMF_SGD_Dilate60_v3.csv")
interestDat<-read.csv("FNL_EmotionCF_Interest_NNMF_SGD_Dilate60_v3.csv")
joyDat<-read.csv("FNL_EmotionCF_Joy_NNMF_SGD_Dilate60_v3.csv")
prideDat<-read.csv("FNL_EmotionCF_Pride_NNMF_SGD_Dilate60_v3.csv")
reliefDat<-read.csv("FNL_EmotionCF_Relief_NNMF_SGD_Dilate60_v3.csv")
sadnessDat<-read.csv("FNL_EmotionCF_Sadness_NNMF_SGD_Dilate60_v3.csv")
satisfactionDat<-read.csv("FNL_EmotionCF_Satisfaction_NNMF_SGD_Dilate60_v3.csv")
shameDat<-read.csv("FNL_EmotionCF_Shame_NNMF_SGD_Dilate60_v3.csv")
surpriseDat<-read.csv("FNL_EmotionCF_Surprise_NNMF_SGD_Dilate60_v3.csv")

names(angerDat)<-str_replace_all(names(angerDat),"X","anger")
names(contemptDat)<-str_replace_all(names(contemptDat),"X","contempt")
names(disgustDat)<-str_replace_all(names(disgustDat),"X","disgust")
names(elationDat)<-str_replace_all(names(elationDat),"X","elation")
names(envyDat)<-str_replace_all(names(envyDat),"X","envy")
names(fearDat)<-str_replace_all(names(fearDat),"X","fear")
names(guiltDat)<-str_replace_all(names(guiltDat),"X","guilt")
names(hopeDat)<-str_replace_all(names(hopeDat),"X","hope")
names(interestDat)<-str_replace_all(names(interestDat),"X","interest")
names(joyDat)<-str_replace_all(names(joyDat),"X","joy")
names(prideDat)<-str_replace_all(names(prideDat),"X","pride")
names(reliefDat)<-str_replace_all(names(reliefDat),"X","relief")
names(sadnessDat)<-str_replace_all(names(sadnessDat),"X","sadness")
names(satisfactionDat)<-str_replace_all(names(satisfactionDat),"X","satisfactoin")
names(shameDat)<-str_replace_all(names(shameDat),"X","shame")
names(surpriseDat)<-str_replace_all(names(surpriseDat),"X","surprise")

names(angerDat)[1]<-"id"
names(contemptDat)[1]<-"id"
names(disgustDat)[1]<-"id"
names(elationDat)[1]<-"id"
names(envyDat)[1]<-"id"
names(fearDat)[1]<-"id"
names(guiltDat)[1]<-"id"
names(hopeDat)[1]<-"id"
names(interestDat)[1]<-"id"
names(joyDat)[1]<-"id"
names(prideDat)[1]<-"id"
names(reliefDat)[1]<-"id"
names(sadnessDat)[1]<-"id"
names(satisfactionDat)[1]<-"id"
names(shameDat)[1]<-"id"
names(surpriseDat)[1]<-"id"

names(angerDat)
names(contemptDat)
names(disgustDat)
names(elationDat)
names(envyDat)
names(fearDat)
names(guiltDat)
names(hopeDat)
names(interestDat)
names(joyDat)
names(prideDat)
names(reliefDat)
names(sadnessDat)
names(satisfactionDat)
names(shameDat)
names(surpriseDat)

combinedDat<-left_join(angerDat,contemptDat,by="id")
combinedDat<-left_join(combinedDat,disgustDat,by="id")
combinedDat<-left_join(combinedDat,elationDat,by="id")
combinedDat<-left_join(combinedDat,envyDat,by="id")
combinedDat<-left_join(combinedDat,fearDat,by="id")
combinedDat<-left_join(combinedDat,guiltDat,by="id")
combinedDat<-left_join(combinedDat,hopeDat,by="id")
combinedDat<-left_join(combinedDat,interestDat,by="id")
combinedDat<-left_join(combinedDat,joyDat,by="id")
combinedDat<-left_join(combinedDat,prideDat,by="id")
combinedDat<-left_join(combinedDat,reliefDat,by="id")
combinedDat<-left_join(combinedDat,sadnessDat,by="id")
combinedDat<-left_join(combinedDat,satisfactionDat,by="id")
combinedDat<-left_join(combinedDat,shameDat,by="id")
combinedDat<-left_join(combinedDat,surpriseDat,by="id")

table(substrRight(names(combinedDat), 8))

write.csv(combinedDat,"study4_subjectiveRatings_WIDE.csv",row.names=FALSE)

angerDat<-read.csv("FNL_EmotionCF_Anger_NNMF_SGD_Dilate60_v3.csv")
contemptDat<-read.csv("FNL_EmotionCF_Contempt_NNMF_SGD_Dilate60_v3.csv")
disgustDat<-read.csv("FNL_EmotionCF_Disgust_NNMF_SGD_Dilate60_v3.csv")
elationDat<-read.csv("FNL_EmotionCF_Elation_NNMF_SGD_Dilate60_v3.csv")
envyDat<-read.csv("FNL_EmotionCF_Envy_NNMF_SGD_Dilate60_v3.csv")
fearDat<-read.csv("FNL_EmotionCF_Fear_NNMF_SGD_Dilate60_v3.csv")
guiltDat<-read.csv("FNL_EmotionCF_Guilt_NNMF_SGD_Dilate60_v3.csv")
hopeDat<-read.csv("FNL_EmotionCF_Hope_NNMF_SGD_Dilate60_v3.csv")
interestDat<-read.csv("FNL_EmotionCF_Interest_NNMF_SGD_Dilate60_v3.csv")
joyDat<-read.csv("FNL_EmotionCF_Joy_NNMF_SGD_Dilate60_v3.csv")
prideDat<-read.csv("FNL_EmotionCF_Pride_NNMF_SGD_Dilate60_v3.csv")
reliefDat<-read.csv("FNL_EmotionCF_Relief_NNMF_SGD_Dilate60_v3.csv")
sadnessDat<-read.csv("FNL_EmotionCF_Sadness_NNMF_SGD_Dilate60_v3.csv")
satisfactionDat<-read.csv("FNL_EmotionCF_Satisfaction_NNMF_SGD_Dilate60_v3.csv")
shameDat<-read.csv("FNL_EmotionCF_Shame_NNMF_SGD_Dilate60_v3.csv")
surpriseDat<-read.csv("FNL_EmotionCF_Surprise_NNMF_SGD_Dilate60_v3.csv")

names(angerDat)<-str_replace_all(names(angerDat),"X","")
names(contemptDat)<-str_replace_all(names(contemptDat),"X","")
names(disgustDat)<-str_replace_all(names(disgustDat),"X","")
names(elationDat)<-str_replace_all(names(elationDat),"X","")
names(envyDat)<-str_replace_all(names(envyDat),"X","")
names(fearDat)<-str_replace_all(names(fearDat),"X","")
names(guiltDat)<-str_replace_all(names(guiltDat),"X","")
names(hopeDat)<-str_replace_all(names(hopeDat),"X","")
names(interestDat)<-str_replace_all(names(interestDat),"X","")
names(joyDat)<-str_replace_all(names(joyDat),"X","")
names(prideDat)<-str_replace_all(names(prideDat),"X","")
names(reliefDat)<-str_replace_all(names(reliefDat),"X","")
names(sadnessDat)<-str_replace_all(names(sadnessDat),"X","")
names(satisfactionDat)<-str_replace_all(names(satisfactionDat),"X","")
names(shameDat)<-str_replace_all(names(shameDat),"X","")
names(surpriseDat)<-str_replace_all(names(surpriseDat),"X","")

names(angerDat)[1]<-"id"
names(contemptDat)[1]<-"id"
names(disgustDat)[1]<-"id"
names(elationDat)[1]<-"id"
names(envyDat)[1]<-"id"
names(fearDat)[1]<-"id"
names(guiltDat)[1]<-"id"
names(hopeDat)[1]<-"id"
names(interestDat)[1]<-"id"
names(joyDat)[1]<-"id"
names(prideDat)[1]<-"id"
names(reliefDat)[1]<-"id"
names(sadnessDat)[1]<-"id"
names(satisfactionDat)[1]<-"id"
names(shameDat)[1]<-"id"
names(surpriseDat)[1]<-"id"

angerDat.L<-angerDat %>% pivot_longer(names_to="time",cols=2:2729,values_to="score")
angerDat.L$emotion<-"anger"
contemptDat.L<-contemptDat %>% pivot_longer(names_to="time",cols=2:2729,values_to="score")
contemptDat.L$emotion<-"contempt"
disgustDat.L<-disgustDat %>% pivot_longer(names_to="time",cols=2:2729,values_to="score")
disgustDat.L$emotion<-"disgust"
elationDat.L<-elationDat %>% pivot_longer(names_to="time",cols=2:2729,values_to="score")
elationDat.L$emotion<-"elation"
envyDat.L<-envyDat %>% pivot_longer(names_to="time",cols=2:2729,values_to="score")
envyDat.L$emotion<-"envy"
fearDat.L<-fearDat %>% pivot_longer(names_to="time",cols=2:2729,values_to="score")
fearDat.L$emotion<-"fear"
guiltDat.L<-guiltDat %>% pivot_longer(names_to="time",cols=2:2729,values_to="score")
guiltDat.L$emotion<-"guilt"
hopeDat.L<-hopeDat %>% pivot_longer(names_to="time",cols=2:2729,values_to="score")
hopeDat.L$emotion<-"hope"
interestDat.L<-interestDat %>% pivot_longer(names_to="time",cols=2:2729,values_to="score")
interestDat.L$emotion<-"interest"
joyDat.L<-joyDat %>% pivot_longer(names_to="time",cols=2:2729,values_to="score")
joyDat.L$emotion<-"joy"
prideDat.L<-prideDat %>% pivot_longer(names_to="time",cols=2:2729,values_to="score")
prideDat.L$emotion<-"pride"
reliefDat.L<-reliefDat %>% pivot_longer(names_to="time",cols=2:2729,values_to="score")
reliefDat.L$emotion<-"relief"
sadnessDat.L<-sadnessDat %>% pivot_longer(names_to="time",cols=2:2729,values_to="score")
sadnessDat.L$emotion<-"sadness"
satisfactionDat.L<-satisfactionDat %>% pivot_longer(names_to="time",cols=2:2729,values_to="score")
satisfactionDat.L$emotion<-"satisfaction"
shameDat.L<-shameDat %>% pivot_longer(names_to="time",cols=2:2729,values_to="score")
shameDat.L$emotion<-"shame"
surpriseDat.L<-surpriseDat %>% pivot_longer(names_to="time",cols=2:2729,values_to="score")
surpriseDat.L$emotion<-"surprise"

combinedDat.L<-rbind(angerDat.L,contemptDat.L,disgustDat.L,elationDat.L,envyDat.L,fearDat.L,
      guiltDat.L,hopeDat.L,interestDat.L,joyDat.L,prideDat.L,reliefDat.L,
      sadnessDat.L,satisfactionDat.L,shameDat.L,surpriseDat.L)
table(combinedDat.L$emotion)
write.csv(combinedDat.L,"study4_subjectRatings_LONG.csv", row.names=FALSE)
