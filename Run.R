
#checking and setting the working directory
getwd()
setwd("C:/Users/michi/Desktop/RRR/MA/converted_data/")

#installing all the necessary packages
install.packages("tidyverse")
install.packages("patchwork")
install.packages("pacman")
install.packages("jsonlite")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("rstatix")
install.packages("ggpubr")
install.packages("car")
install.packages("devtools")
install.packages("naniar")
install.packages("apaTables",dep=T)
install.packages("rmarkdown")
install.packages("lsmeans")
install.packages("emmeans")
install.packages("hrbrthemes")
install.packages("GGally")
install.packages("viridis")
install.packages("reshape2")
install.packages("sjstats")
install.packages("CGPfunctions")
install.packages("spatialEco")
install.packages("plotfunctions")
install.packages("pwr")
install.packages("ez")
devtools::install_github("hauselin/hausekeep") 
devtools::install_github("crsh/papaja")
devtools::install_github("singmann/afex@master")
packageVersion("afex")
if(!require(installr)) {
  install.packages("installr"); require(installr)} 
updateR()

library(plotfunctions)
library(spatialEco)
#for all the data manipulation/filtering/variable creation/plotting as well as for help loading data
library(tidyverse)
#package for combining the plots together into one image
library(patchwork)
# package that I use just to add the "theme_apa()" option for plots to make them APA style
library(papaja)
#These help with loading json files in R
library(pacman)
library(jsonlite)
#for statistical functions etc
library(rstatix)
#publicationready plots
library(ggpubr)
#rmarkwodn??
library(rmarkdown)
#sth for levene test
library(car)
#for MAD
library(hausekeep)
#repacingwith na
library(naniar)
#apatables
library(apaTables)
#for anova
library(afex)
#some more
library(lsmeans)
library(emmeans)
library(hrbrthemes)
library(GGally)
library(viridis)
library(reshape2)
library(dplyr)
library(sjstats)
library(CGPfunctions)
library(pwr)
library(ez)

#These load the functions that will help load the data and make the plot
source("00_load_data.R")
source("01_plot_data.R")

# folder info
fileFolder = "C:/Users/michi/Desktop/RRR/MA/converted_data/datafiles/"
files = "datafiles"
files = list.files("C:/Users/michi/Desktop/RRR/MA/converted_data/datafiles/")
#this is for payment
for (ParticipantN in 1:45){
  jatosFile <- paste(fileFolder,files[ParticipantN], sep = "")
  bonusEuros = bonusEurosEarned(jatosFile, ParticipantN)
  print(bonusEuros)}

#original data without ANY filtering except for rt >1800
(ParticipantN =1)
jatosFile <- paste(fileFolder,files[ParticipantN], sep = "")

Task1 <- loadTaskOne(jatosFile, ParticipantN)
colourKey <- createColourKey(Task1)
Task2 <- loadTaskTwo(jatosFile, colourKey, ParticipantN)
info <- loadinfo(jatosFile, PartipantN)
Task1 <- cbind(Task1, info)
Task2 <- cbind(Task2, info)

task1.1 <-  Task1
task2.1 <-  Task2

TaskOneAll <- task1.1
TaskTwoAll <- task2.1

for (ParticipantN in 2:40){
   jatosFile <- paste(fileFolder,files[ParticipantN], sep = "")
   Task1 <- loadTaskOne(jatosFile, ParticipantN)
   colourKey <- createColourKey(Task1)
   Task2 <- loadTaskTwo(jatosFile, colourKey, ParticipantN)
   info <- loadinfo(jatosFile, PartipantN)
   Task1 <- cbind(Task1, info)
   Task2 <- cbind(Task2, info)
   
   TaskOneAll <- rbind(TaskOneAll, Task1)
   TaskTwoAll <- rbind(TaskTwoAll, Task2)
}

# sample info
descriptivesample <- select(TaskTwoAll, PID, Age, Sex, hand)
#descriptivesample <-descriptivesample[-c(1)]
descriptivesample <- distinct(descriptivesample, PID, .keep_all = TRUE)
descriptivesample <-descriptivesample[-c(1)]

summary(descriptivesample)

TaskTwoAll$Age <- as.integer(TaskTwoAll$Age)

descriptivesdemo <- descriptivesample %>% 
   summarize(
      Mean = mean(Age)
      , Median = median(Age)
      , SD = sd(Age)
      , Min = min(Age)
      , Max = max(Age)
   )

table(descriptivesample$Sex)[names(table(descriptivesample$Sex)) == "Weiblich"] 
table(descriptivesample$hand)[names(table(descriptivesample$hand)) == "Linkshänder"] 

#minimum filtering (no non response trials)
 
TaskTwoAll <- TaskTwoAll%>%
   mutate(PID= ifelse(rt>1790,NA,PID))
TaskTwoAll <- na.omit(TaskTwoAll)

#one boxplot and histogram for original data

ggplot(TaskTwoAll, aes(x=valueCondition, y=rt, fill=valueCondition)) + 
   geom_boxplot(alpha=0.3) +
   stat_boxplot(geom="errorbar", width=0.3) +
   geom_point(size = 0.1) + 
   xlab("Condition") + ylab("Reaction times [ms]") +
   ylim(100, 1800) +
   theme_apa() +
   theme(legend.position="none") +
   scale_fill_brewer(palette="BuPu")
ggsave("boxplot_rt_original.png", width = 16, height = 9, units = "in")

ggplot(TaskTwoAll, aes(x=rt)) +
geom_histogram(binwidth = 1)+
geom_vline(aes(xintercept=mean(rt)), color="blue", linetype="dashed", size=1)+
   xlab("Reaction time [ms]") + ylab("Frequency") +
theme_apa()
ggsave("histogram_rt_original.png", width = 16, height = 9, units = "in")



# now WITH filtering (firstly the insufficient accuracy in task1, MAD outliers, <200 >1790)
#first person
(ParticipantN =1)
jatosFile <- paste(fileFolder,files[ParticipantN], sep = "")

Task1 <- loadTaskOne(jatosFile, ParticipantN)
colourKey <- createColourKey(Task1)
Task2 <- loadTaskTwo(jatosFile, colourKey, ParticipantN)
info <- loadinfo(jatosFile, PartipantN)
Task1 <- cbind(Task1, info)
Task2 <- cbind(Task2, info)

meanacctask1 <- mean(Task1[,2])


Task2 <- Task2%>%
   mutate(PID= ifelse(meanacctask1<0.6,NA,PID))


Task2<- replace_with_na_at(Task2, "rt", ~.x < 200 | .x > 1790)

sum(is.na(Task2$rt))
cleanRT <- outliersMAD(Task2[1:320,1],  MADCutOff = 2.5, replaceOutliersWith = NA,
                       showMADValues = FALSE, outlierIndices = FALSE, bConstant = 1.4826, digits = 2)

Task2new <- cbind(Task2, rt_clean = cleanRT) 
Task2 <- Task2new


task1.1 <-  Task1
task2.1 <-  Task2

# a frame for all

TaskOneAll <- task1.1
TaskTwoAll <- task2.1

#now the loop to add the rest --- 

for (ParticipantN in 2:40){
   jatosFile <- paste(fileFolder,files[ParticipantN], sep = "")
   Task1 <- loadTaskOne(jatosFile, ParticipantN)
   colourKey <- createColourKey(Task1)
   Task2 <- loadTaskTwo(jatosFile, colourKey, ParticipantN)
   info <- loadinfo(jatosFile, PartipantN)
   Task1 <- cbind(Task1, info)
   Task2 <- cbind(Task2, info)
   
   meanacctask1 <- mean(Task1[,2])
   Task2 <- Task2%>%
      mutate(PID= ifelse(meanacctask1<0.6,NA,PID))
   
   Task2<- replace_with_na_at(Task2, "rt", ~.x < 200 | .x > 1790)
   
   sum(is.na(Task2$rt))
   
   cleanRT <- outliersMAD(Task2[1:320,1],  MADCutOff = 2.5, replaceOutliersWith = NA,
                          showMADValues = FALSE, outlierIndices = FALSE, bConstant = 1.4826, digits = 2)
   
   Task2new <- cbind(Task2, rt_clean = cleanRT)
   Task2 <- Task2new
   
TaskOneAll <- rbind(TaskOneAll, Task1)
TaskTwoAll <- rbind(TaskTwoAll, Task2)
}

#info <- loadinfo(jatosFile, PartipantN)
#(ParticipantN =1)
#jatosFile <- paste(fileFolder,files[ParticipantN], sep = "")

#Create the data for task 1
#TaskOne1 <- loadTaskOne(jatosFile, ParticipantN)
#Use the data from task 1 to create a key for the colours-value associations for this participant
#colourKey <- createColourKey(TaskOne)
#Create the data for task 2 and add the information from task 1 about which colour is which value
#TaskTwo1 <- loadTaskTwo(jatosFile, colourKey, ParticipantN)



#FOR PLOTTING INDIVIDUAL TASK PERFORMANCE for all participants with the unfiltered

(ParticipantN = 40)
   jatosFile <- paste(fileFolder,files[ParticipantN], sep = "")
   Task1 <- loadTaskOne(jatosFile, ParticipantN)
   colourKey <- createColourKey(Task1)
   Task2 <- loadTaskTwo(jatosFile, colourKey, ParticipantN)
 #  info <- loadinfo(jatosFile, PartipantN)
 ##  Task1 <- cbind(Task1, info)
 # Task2 <- cbind(Task2, info)

screenParticipant1(Task1)
# and save the plot
ggsave("TaskOne40.png", width = 16, height = 9, units = "in")

screenParticipant2(Task2)#column blockcount and trialindex  was missing in the load function
# and save the plot
ggsave("TaskTwo40.png", width = 16, height = 9, units = "in")


# for some, it doesnt work because there are no NAs so again with another function that leaves out NA in task 22222
(ParticipantN = 40)
jatosFile <- paste(fileFolder,files[ParticipantN], sep = "")
Task1 <- loadTaskOne(jatosFile, ParticipantN)
colourKey <- createColourKey(Task1)
Task2 <- loadTaskTwo(jatosFile, colourKey, ParticipantN)


screenParticipant1(Task1)
ggsave("TaskOne40.png", width = 16, height = 9, units = "in")

screenParticipant3(Task2)
ggsave("TaskTwo40.png", width = 16, height = 9, units = "in")


# for some, it doesnt work because there are no NAs so again with another function that leaves out NA in task 11111
(ParticipantN = 40)
jatosFile <- paste(fileFolder,files[ParticipantN], sep = "")
Task1 <- loadTaskOne(jatosFile, ParticipantN)
colourKey <- createColourKey(Task1)
Task2 <- loadTaskTwo(jatosFile, colourKey, ParticipantN)


screenParticipant4(Task1)
ggsave("TaskOne40.png", width = 16, height = 9, units = "in")

screenParticipant2(Task2)
ggsave("TaskTwo40.png", width = 16, height = 9, units = "in")


#######
# NOW MORE FILTERING ON THE FILTERED DATA: INSUFFICIENT ACCURACY TASK2 AND BLOCKWISE ACCURACY
TaskTwoAll <- na.omit(TaskTwoAll) 

TaskTwoAll <- TaskTwoAll %>% 
   group_by(PID) %>% 
   mutate(averageacc = mean(correct), PID = ifelse(averageacc<0.6,NA, PID))    

sum(is.na(TaskTwoAll$PID))
#then divide by 280


TaskTwoAll <- TaskTwoAll %>% 
   group_by(blockCount, PID) %>% 
   mutate(blockaverage = mean(correct), blockCount = ifelse(blockaverage<0.6,NA,blockCount))

sum(is.na(TaskTwoAll$blockCount))
#then divide by 40


# deleting trials (rows) that have NA in them

TaskTwoAllnew <- na.omit(TaskTwoAll) 

#histogram new filtered data

ggplot(TaskTwoAllnew, aes(x=rt_clean)) +
   geom_histogram(binwidth = 1)+
   geom_vline(aes(xintercept=mean(rt)), color="blue", linetype="dashed", size=1)+
   xlab("Reaction time [ms]") + ylab("Frequency") +
   theme_apa()
ggsave("histogram_rt_filtered.png", width = 16, height = 9, units = "in")

#boxplot

ggplot(TaskTwoAllnew, aes(x=valueCondition, y=rt, fill=valueCondition)) + 
   geom_boxplot(alpha=0.3) +
   stat_boxplot(geom="errorbar", width=0.3) +
   geom_point(size = 0.1) + 
   xlab("Condition") + ylab("Reaction times [ms]") +
   ylim(100, 1800) +
   theme_apa() +
   theme(legend.position="none") +
   scale_fill_brewer(palette="BuPu")
ggsave("boxplot_rt_filtered.png", width = 16, height = 9, units = "in")


#descriptive data (without practice block) RT
descriptivesrt <- TaskTwoAllnew %>% group_by(valueCondition) %>%
  filter(correct>0)%>%
  filter(blockCount>0)%>%
   summarize(
      Mean = mean(rt_clean)
      , Median = median(rt_clean)
      , SD = sd(rt_clean)
      , Min = min(rt_clean)
      , Max = max(rt_clean)
   )
descriptivesrt[, -1] <- printnum(descriptivesrt[, -1])

#only the practice block RT
descriptivesrt2 <- TaskTwoAllnew %>% group_by(valueCondition) %>%
     filter(correct>0)%>%
     filter(blockCount<1)%>%
   summarize(
      Mean = mean(rt_clean)
      , Median = median(rt_clean)
      , SD = sd(rt_clean)
      , Min = min(rt_clean)
      , Max = max(rt_clean)
   )
descriptivesrt2[, -1] <- printnum(descriptivesrt2[, -1])

# without practice block accuracy
descriptivesacc <- TaskTwoAllnew %>% group_by(valueCondition) %>%
   filter(blockCount>0)%>%
   summarize(
      Mean = mean(correct)
      , Median = median(correct)
      , SD = sd(correct)
      , Min = min(correct)
      , Max = max(correct)
   )
descriptivesacc[, -1] <- printnum(descriptivesacc[, -1])

#only practice block accuracy
descriptivesacc2 <- TaskTwoAllnew %>% group_by(valueCondition) %>%
 filter(blockCount<1)%>%
   summarize(
      Mean = mean(correct)
      , Median = median(correct)
      , SD = sd(correct)
      , Min = min(correct)
      , Max = max(correct)
   )
descriptivesacc2[, -1] <- printnum(descriptivesacc2[, -1])





#for rt analyisis leavng out incorrect trials and practice block (0)

TaskTwoAllnewRT <- replace_with_na_at(TaskTwoAllnew, "correct", ~.x < 1)
TaskTwoAllnewRT <- na.omit(TaskTwoAllnewRT) 
view(TaskTwoAllnewRT)

TaskTwoAllnewRT1 <- replace_with_na_at(TaskTwoAllnew, "blockCount", ~.x < 1)
TaskTwoAllnewRT1 <- na.omit(TaskTwoAllnewRT) 

#for accuracy leaving out practice block
TaskTwoAllnew1 <- replace_with_na_at(TaskTwoAllnew, "blockCount", ~.x < 1)
TaskTwoAllnew1 <- na.omit(TaskTwoAllnew1) 

#testing normality assumption (shapiro wil test) its violated but thts not so bad...?
TaskTwoAllnewRT%>%
   filter(correct>0)%>%
   group_by(valueCondition)%>%
   shapiro_test(rt_clean)

#a plot to visualize the difference to normality....

ggqqplot(TaskTwoAllnewRT, "rt_clean", facet.by = "valueCondition")

#ggregating, factorziing and cpmutin of the anova...

anovazeugrt <- aggregate(rt_clean ~ PID + valueCondition,
                    data = TaskTwoAllnewRT1,
                    FUN = mean)

anovazeugrt$PID <- factor(anovazeugrt$PID)
anovazeugrt$valueCondition <- factor(anovazeugrt$valueCondition)


anovart<- aov(rt_clean ~ valueCondition+Error(PID/valueCondition), data=anovazeugrt)
summary(anovart)



#FOR APATABLE

options(contrasts = c("contr.helmert", "contr.poly"))
output <- aov(rt_clean~valueCondition, data = TaskTwoAllnewRT1)
apa.aov.table(output, filename = "anova_tableRT.doc")

#accuracy anova


anovazeugacc <- aggregate(correct ~ PID + valueCondition,
                         data = TaskTwoAllnew1,
                         FUN = mean)

view(anovazeugacc)


anovazeugacc$PID <- factor(anovazeugacc$PID)
anovazeugacc$valueCondition <- factor(anovazeugacc$valueCondition)


anovaacc <- aov(correct~valueCondition+Error(PID/valueCondition), data=anovazeugacc)
summary(anovaacc)

# table doesnt not work 
options(contrasts = c("contr.helmert", "contr.poly"))
output <- aov(correct~valueCondition, data = TaskTwoAllnew1)
apa.aov.table(output, filename = "anova_tableacc.doc")


##ggplottt 

ggplot(anovazeugrt, aes(x = valueCondition, y = rt_clean, fill = valueCondition)) +
   geom_boxplot() +
   stat_boxplot(geom="errorbar", width=0.3) +
   geom_point(aes(shape=".")) + 
   scale_fill_manual(values=c("lightsalmon2", "plum2", "palegreen3", "turquoise2", "Grey")) +
   xlab("Value condition") + ylab("Reaction time [ms]") +
   ylim(220, 470) +
   theme_apa()
ggsave("anovartall.png", width = 16, height = 9, units = "in")



ggplot(anovazeugrt2, aes(x = valueCondition, y = rt_clean)) +
   geom_boxplot() +
   stat_boxplot(geom="errorbar", width=0.2) +
   geom_point(aes(colour = factor(valueCondition), size = 0.5)) + 
   xlab("Value condition") + ylab("Reaction time [ms]") +
   ylim(220, 470) +
   theme_apa()
ggsave("anovartfirstblocks.png", width = 16, height = 9, units = "in")

  

ggplot(anovazeugacc, aes(x = valueCondition, y = correct, fill = valueCondition)) +
   geom_boxplot() +
   stat_boxplot(geom="errorbar", width=0.3) +
   geom_point(aes(shape=".")) + 
   scale_fill_manual(values=c("lightsalmon2", "plum2", "palegreen3", "turquoise2", "Grey")) +
   xlab("Value condition") + ylab("Accuracy") +
   ylim(0.6, 1) +
   theme_apa()
ggsave("anovaaccuracy.png", width = 16, height = 9, units = "in")




#testing homogenity of variances,  aggregating, faotrizing and compiuting the two way with blockcount


leveneTest(rt_clean ~ valueCondition*blockCount, data = anovazeugrtblocks)


anovazeugrtblocks <- aggregate(rt_clean ~ PID + valueCondition + blockCount,
                         data = TaskTwoAllnewRT,
                         FUN = mean)

anovazeugrtblocks$PID <- factor(anovazeugrtblocks$PID)
anovazeugrtblocks$valueCondition <- factor(anovazeugrtblocks$valueCondition)
anovazeugrtblocks$blockCount <- factor(anovazeugrtblocks$blockCount)

anovablocksrt <- aov(rt_clean~valueCondition*blockCount+
                     Error(PID/(valueCondition*blockCount)),
                   data=anovazeugrtblocks)
summary(anovablocksrt)




pwc <- anovazeugrtblocks %>%
   pairwise_t_test(
      rt_clean ~ blockCount, paired = FALSE,
      p.adjust.method = "bonferroni"
   )
view(pwc)

####### 



#anovablocksinteraction <- apa.2way.table(
 #  valueCondition,
 #  blockCount,
 #  rt_clean,
#   anovazeugrtblocks,
#   filename = NA,
#   table.number = NA,
#   show.conf.interval = FALSE,
#   show.marginal.means = FALSE,
#   landscape = TRUE
#)

writeClipboard(as.character(anovablocksinteraction))


anovazeugaccblocks <- aggregate(correct ~ PID + valueCondition + blockCount,
                               data = TaskTwoAllnew,
                               FUN = mean)

anovazeugaccblocks$PID <- factor(anovazeugaccblocks$PID)
anovazeugaccblocks$valueCondition <- factor(anovazeugaccblocks$valueCondition)
anovazeugaccblocks$blockCount <- factor(anovazeugaccblocks$blockCount)

anovablocksacc <- aov(correct~valueCondition*blockCount+
                        Error(PID/(valueCondition*blockCount)),
                     data=anovazeugaccblocks)
summary(anovablocksacc)


pwc2 <- anovazeugaccblocks %>%
   pairwise_t_test(
      correct ~ blockCount, paired = FALSE,
      p.adjust.method = "bonferroni"
   )
view(pwc2)

# ERROR MODELL IST SINGULÄR?

#hier without interaction (for theother plot also)
anovazeugrtblocks1 <- aggregate(rt_clean ~ PID + blockCount,
                         data = TaskTwoAllnewRT,
                         FUN = mean)

view(anovazeugrtblocks1)


anovazeugrtblocks1$PID <- factor(anovazeugrtblocks1$PID)
anovazeugrtblocks1$blockCount<- factor(anovazeugrtblocks1$blockCount)


anovartblocks1<- aov(rt_clean ~ blockCount+Error(PID/blockCount), data=anovazeugrtblocks1)
summary(anovartblocks1)

#apa table
options(contrasts = c("contr.helmert", "contr.poly"))
output <- aov(rt_clean~blockCount, data = TaskTwoAllnewRT)
apa.aov.table(output, filename = "1wayanovablocks.doc")


mns <- tapply(anovazeugrt$rt_clean,
              list(anovazeugrt$valueCondition), mean)
ses <- tapply(anovazeugrt$rt_clean,
              list(anovazeugrt$valueCondition), sd)/
   sqrt(tapply(anovazeugrt$rt_clean,
               list(anovazeugrt$valueCondition), length))
bar <- barplot(mns, beside = TRUE, xpd = FALSE, ylim = c(250, 420),
               xlim = c(0, 5), xlab="Value condition", ylab="Reaction time [ms]", width = 0.75, col = c("rosybrown3"))
arrows(bar, mns + 1.96 * ses,
       bar, mns - 1.96 * ses,
       angle = 90, code = 3, length = 0.2) 

#1way accuracy for plot
anovazeugaccblocks1 <- aggregate(correct ~ PID + blockCount,
                                data = TaskTwoAllnew,
                                FUN = mean)

anovazeugaccblocks1$PID <- factor(anovazeugaccblocks1$PID)
anovazeugaccblocks1$blockCount <- factor(anovazeugaccblocks1$blockCount)

anovablocksacc1 <- aov(correct~blockCount+
                         Error(PID/blockCount),
                      data=anovazeugaccblocks1)
summary(anovablocksacc1)

options(contrasts = c("contr.helmert", "contr.poly"))
output <- aov(correct~blockCount, data = TaskTwoAllnewRT)
apa.aov.table(output, filename = "1wayanovablockscorr.doc")

#plot
ggplot(anovazeugaccblocks1, aes(x = blockCount, y = correct)) +
   geom_boxplot() +
   stat_boxplot(geom="errorbar", width=0.3) +
   geom_point(aes(shape=".")) +  
   xlab("Block Count") + ylab("Accuracy") +
   ylim(0.6, 1) +
   theme_apa()
ggsave("anovaaccblocks.png", width = 12, height = 9, units = "in")


ggplot(anovazeugrtblocks1, aes(x = blockCount, y = rt_clean)) +
   geom_boxplot() +
   stat_boxplot(geom="errorbar", width=0.3) +
   geom_point(aes(shape=".")) + 
   xlab("Block Count") + ylab("Mean RT [ms]") +
   ylim(200, 600) +
   theme_apa()
ggsave("anovartblocks.png", width = 12, height = 9, units = "in")


#visualization for anovablocks RT


#blockplotdata <- anovazeugrtblocks %>%
 #  group_by(valueCondition, blockCount)%>%
 #  summarize(
 #     Mean = mean(rt_clean))
#blockplotdata[, -1] <- printnum(blockplotdata[, -1])

# Plot

#ggparcoord(blockplotdata2,
#           columns = 2:9, groupColumn = 1, order = "anyClass",
#           showPoints = TRUE, 
#           title = "Parallel Coordinate Plot for the Iris Data",
#           alphaLines = 0.3
#) + 
 #  scale_color_viridis(discrete=TRUE) +
#   theme_ipsum()+
#   theme(
#      plot.title = element_text(size=10)
#   )


#anovazeugrtblocks %>%
#   ggplot( aes(x=blockCount, y=rt_clean, group=valueCondition, color=valueCondition)) +  
 #  geom_line()



#was ist das?
TaskTwoAllnewRT <- TaskTwoAllnewRT %>% 
  group_by(blockCount, valueCondition) %>% 
   mutate(blockaverage = mean(rt_clean))

#das mit linien

ggplot(TaskTwoAllnewRT, aes(y = blockaverage, x = blockCount, label = blockCount, fill = valueCondition, colour = valueCondition)) +
 # geom_segment(aes(x = 0, y = blockaverage, xend = blockCount, yend = blockaverage), color = "grey50", size = 0) +
   geom_point(size = 3) +
   geom_line()+
   xlab("Block Count")+
   ylab("Mean RT [ms]")+
   theme_apa()
ggsave("blockaveragert.png", width = 16, height = 9, units = "in")
#   geom_text(nudge_x = 1.5, angle = 0)



ggplot(anovazeugrtblocks1, aes(x = blockCount, y = rt_clean)) +
   geom_boxplot() +
   stat_boxplot(geom="errorbar", width=0.2) +
   geom_point(aes(colour = factor(blockCount))) + 
   xlab("Block count") + ylab("Mean RT [ms]") +
   ylim(220, 470) +
   theme_apa()
ggsave("anovablocksss.png", width = 16, height = 9, units = "in")

# ok wtf wenn ich das mit der interaktionsanova rechne kommen kleinere fhelerbaken als wennich mit der normalen anova mache. bei der ist aber eig auc signifikant....

mns <- tapply(anovazeugrtblocks$rt_clean,
              list(anovazeugrtblocks$blockCount), mean)
ses <- tapply(anovazeugrtblocks$rt_clean,
              list(anovazeugrtblocks$blockCount), sd)/
   sqrt(tapply(anovazeugrtblocks$rt_clean,
               list(anovazeugrtblocks$blockCount), length))
bar <- barplot(mns, beside = TRUE, xpd = FALSE, ylim = c(250, 550),
               xlim = c(0, 8), width = 0.75,xlab="Block number", ylab="Reaction time [ms]", col = c("rosybrown3"))
arrows(bar, mns + 1.96 * ses,
       bar, mns - 1.96 * ses,
       angle = 90, code = 3, length = 0.15)
#legend("top", legend = c("RT in ms"),
#       col = c("skyblue"), pch = 16)

#visualisation for anovablocks accuracy (only blocks and accuracy, wothout valuecondition) DOES NOT FUNCTION ANYMORE???

mns <- tapply(anovazeugaccblocks$correct,
              list(anovazeugaccblocks$blockCount), mean)
ses <- tapply(anovazeugaccblocks$correct,
              list(anovazeugaccblocks$blockCount), sd)/
   sqrt(tapply(anovazeugaccblocks$correct,
               list(anovazeugaccblocks$blockCount), length))
bar <- barplot(mns, beside = TRUE, xpd = FALSE, ylim = c(0.8, 1),
               xlim = c(0, 8), width = 0.75, xlab="Block number", ylab="Accuracy",col = c("wheat3"))
arrows(bar, mns + 1.96 * ses,
       bar, mns - 1.96 * ses,
       angle = 90, code = 3, length = 0.15)
#legend("top", legend = c("accuracy"),
#       col = c("wheat3"), pch = 16)

#now the distractabiltiy 2 way anova

TaskTwoAllnewRT1 <- TaskTwoAllnewRT1 %>% 
   mutate(medirt= median(rt_clean), Distractability = ifelse(rt_clean<medirt,"low","high")) 



anovazeugdistract <- aggregate(rt_clean ~ PID + valueCondition + Distractability,
                               data = TaskTwoAllnewRT1,
                               FUN = mean)

anovazeugdistract$PID <- factor(anovazeugdistract$PID)
anovazeugdistract$valueCondition <- factor(anovazeugdistract$valueCondition)
anovazeugdistract$Distractability <- factor(anovazeugdistract$Distractability)



leveneTest(rt_clean ~ valueCondition*Distractability, data = anovazeugdistract)



anovadistract <- aov(rt_clean~valueCondition*Distractability+
                      Error(PID/valueCondition),
                   data=anovazeugdistract)
summary(anovadistract)

#apa table
options(contrasts = c("contr.helmert", "contr.poly"))
output <- aov(rt_clean~valueCondition*Distractability, data = TaskTwoAllnewRT1)
apa.aov.table(output, filename = "2wayanovadistractability.doc")
##############################test
Task2ttest <-  replace_with_na_at(TaskTwoAllnewRT1, "Distractability", ~.x == "high")
Task2ttest <- na.omit((Task2ttest))

t.test(Task2ttest$rtCosts, mu= 0, alt= "less")

#mns <- tapply(anovazeugdistract$rt_clean,
#              list(anovazeugdistract$valueCondition, anovazeugdistract$Distractability), mean)
#ses <- tapply(anovazeugdistract$rt_clean,
#              list(anovazeugdistract$valueCondition,  anovazeugdistract$Distractability), sd)/
 #  sqrt(tapply(anovazeugdistract$rt_clean,
  #             list(anovazeugdistract$valueCondition, anovazeugdistract$Distractability), length))
#bar <- barplot(mns, beside = TRUE, xpd = FALSE, ylim = c(250, 450),
 #              xlim = c(0, 10), xlab="Distractability [>/< Median RT]", ylab="Reaction time [ms]", width = 0.85, col = c("goldenrod2", "lightsteelblue1", "darksalmon", "yellow4", "gray60"))
#arrows(bar, mns + 1.96 * ses,
 #      bar, mns - 1.96 * ses,
  #     angle = 90, code = 3, length = 0.15)
#legend("right", legend = c("low loss", "low reward", "neutral", "none", "high reward"),
 #      col = c("goldenrod2", "lightsteelblue1", "darksalmon", "yellow4", "gray60" ), pch = 16)
#theoretisch noch den in der mitte auslassen


#histogram for median rt

hist(TaskTwoAllnewRT1$rt_clean, breaks = seq(floor(min(TaskTwoAllnewRT1$rt_clean)), ceiling(max(TaskTwoAllnewRT1$rt_clean))),
     main="Distribution of Reaction times", xlab="Reaction time [ms]")

#another version



meanrt2 <- TaskTwoAllnewRT1%>%
      group_by(valueCondition)%>%
      filter(rt>1)%>%
   summarise(mean=mean(rt_clean))
view(meanrt2)




####


#making the difference variable for all values

TaskTwoAllnewRT1 <- TaskTwoAllnewRT1 %>% 
   group_by(PID, valueCondition) %>% 
   mutate(averages = mean(rt_clean), neutralaverage = ifelse(valueCondition == "Neutral", mean(averages), 99999))


TaskTwoAllnewRT1 <- TaskTwoAllnewRT1 %>% 
   group_by(PID) %>% 
   mutate(neutralaverage = ifelse(valueCondition != "Neutral", min(neutralaverage), neutralaverage))

TaskTwoAllnewRT1$rtCosts <- (TaskTwoAllnewRT1$rt_clean - TaskTwoAllnewRT1$neutralaverage)

#a valuecondition variable with only loss and reward 

TaskTwoAllnewRT1 <- TaskTwoAllnewRT1 %>% 
   mutate(LossOrReward = ifelse(valueCondition == "Negative", "Negative",
                                ifelse(valueCondition == "Low", "Low Reward", NA)))


TaskTwoAllnewRT1ttest <- na.omit(TaskTwoAllnewRT1)

#hypothesis 3 loss aversion t test
tdata <- aggregate(rtCosts ~ PID + LossOrReward,
                    data = TaskTwoAllnewRT1ttest,
                    FUN = mean)


tdata$PID <- factor(tdata$PID)
tdata$LossOrReward<- factor(tdata$LossOrReward)

t.test(rtCosts ~ LossOrReward, data = tdata,
       alternative = "two.sided", paired = T)  

tdata %>%
   group_by(LossOrReward) %>%
   get_summary_stats(rtCosts, type = "mean_sd")

mean(tdata$rtCosts)




mns <- tapply(tdata$rtCosts,
              list(tdata$LossOrReward), mean)
ses <- tapply(tdata$rtCosts,
              list(tdata$LossOrReward), sd)/
   sqrt(tapply(tdata$rtCosts,
               list(tdata$LossOrReward), length))
bar <- barplot(mns, beside = TRUE, xpd = FALSE, ylim = c(-10, 10),
               xlim = c(0, 2), width = 0.75,xlab="Condition", ylab="Distractor costs [ms]", col = c("lightsalmon2", "palegreen3"))
arrows(bar, mns + 1.96 * ses,
       bar, mns - 1.96 * ses,
       angle = 90, code = 3, length = 0.15)
#plot  


ggplot(tdata, aes(x = LossOrReward, y = rtCosts)) +
   geom_boxplot() +
   stat_boxplot(geom="errorbar", width=0.2) +
   geom_point(aes(colour = factor(LossOrReward), shape = ".")) + 
   xlab("Condition") + ylab("RT costs [ms]") +
   ylim(-10, 10) +
   theme_apa()
ggsave("rtcoststtest.png", width = 10, height = 9, units = "in")


#linearity
#making the value vrialbe with numbers
TaskTwoAllnewRT1 <- TaskTwoAllnewRT1%>%
                  mutate(valuepoints = ifelse(valueCondition == "Negative", NA,
                               ifelse(valueCondition == "Neutral", 0,
                                      ifelse(valueCondition == "Low", 2,
                                             ifelse(valueCondition == "None", NA, 10))))) 

TaskTwoAllnewRT1$valuepoints <-as.integer(TaskTwoAllnewRT1$valuepoints)
TaskTwoAllnewRT1$rt_clean <-as.integer(TaskTwoAllnewRT1$rt_clean)
#with means?

trendcheck <- aggregate(rt_clean ~ valuepoints + PID,
                               data = TaskTwoAllnewRT1,
                               FUN = mean)

#linearity teast OK ICH KRIEGE NICHT GEPLOTTET; OBJEKT IS NOT FOUND: 



fit <- lm(valuepoints ~ log(rt_clean), TaskTwoAllnewRT1)
summary(fit)



apa.aov.table(fit, filename = "fittingtable.doc")

#with means
fit <- lm(valuepoints ~ log(rt_clean), trendcheck)
summary(fit)

#gut
trend.line(trendcheck$valuepoints, trendcheck$rt_clean, type = "linear", plot = TRUE)
ggsave("linearityfitting.png",width = 12, height = 9, units = "in")

trend.line(trendcheck$valuepoints, trendcheck$rt_clean, type = "logarithmic", plot = TRUE)


ggplot(aes(x=valuepoints,y=rt_clean),data=trendcheck)+
   geom_point()+
   geom_line()+
   xlab("Condition (in value points)") + ylab(" mean RT [ms]") +
   theme_apa()
ggsave("linearityplot.png",width = 12, height = 9, units = "in")

ggplot(aes(x=valuepoints,y=rt_clean),data=trendcheck)+
   geom_point()+
   geom_line()+
   geom_smooth(method=lm)+
   xlab("Condition (in value points)") + ylab(" mean RT [ms]") +
   theme_apa()
ggsave("linearityplot2.png",width = 16, height = 9, units = "in")



typeof("valuepoints")
as.integer("valuepoints")


ggplot(TaskTwoAllnewRT1, aes(valuepoints, averages)) +
   geom_point() + 
   geom_smooth(method=lm) +
   theme_apa()
ggsave("linearity.png",width = 16, height = 9, units = "in")




#gender anova 



anovazeuggender <- aggregate(rt_clean ~ PID + valueCondition + Sex,
                               data = TaskTwoAllnewRT1,
                               FUN = mean)

anovazeuggender$PID <- factor(anovazeuggender$PID)
anovazeuggender$valueCondition <- factor(anovazeuggender$valueCondition)
anovazeuggender$Sex <- factor(anovazeuggender$Sex)

anovagender <- aov(rt_clean~valueCondition*Sex+
                        Error(PID/valueCondition),
                     data=anovazeuggender)
summary(anovagender)

anovagenderone<- aov(rt_clean~Sex,
                     data=anovazeuggender)

summary(anovagenderone)


ezANOVA(
   TaskTwoAllnewRT1
   , rt_clean
   , PID
   , within = valueCondition
   , between = Sex
   , type = 2
   , white.adjust = FALSE
   , detailed = TRUE
   , return_aov = TRUE
)

install.packages("sjPlot")
library(sjPlot)

#no post hoc only main effct sex sig?? is it

#apa table
options(contrasts = c("contr.helmert", "contr.poly"))
output <- aov(rt_clean~valueCondition*Sex, data = TaskTwoAllnewRT1)
apa.aov.table(output, filename = "2waygenderrt.doc")

apa.2way.table(
   Sex,
   valueCondition,
   rt_clean,
   anovazeuggender,
   filename = "aptablesex2wayrt.doc",
   table.number = NA,
   show.conf.interval = FALSE,
   show.marginal.means = FALSE,
   landscape = TRUE
)
#fürgender noch???

ggplot(tdata, aes(x = LossOrReward, y = Sex)) +
   geom_boxplot() +
   stat_boxplot(geom="errorbar", width=0.2) +
   geom_point(aes(colour = factor(LossOrReward))) + 
   xlab("Condition") + ylab("RT costs [ms]") +
   ylim(-10, 10) +
   theme_apa()
ggsave("rtcosts.png", width = 16, height = 9, units = "in")

#gender with difference variable?+++++++++++++


anovazeuggender2 <- aggregate(rtCosts~ PID + LossOrReward + Sex,
                             data = TaskTwoAllnewRT1,
                             FUN = mean)

anovazeuggender2$PID <- factor(anovazeuggender2$PID)
anovazeuggender2$LossOrReward <- factor(anovazeuggender2$LossOrReward)
anovazeuggender2$Sex <- factor(anovazeuggender2$Sex)

anovagender2 <- aov(rtCosts~LossOrReward*Sex+
                      Error(PID/LossOrReward),
                   data=anovazeuggender2)
summary(anovagender2)
# t test female less than zero
Task2ttestg <-  replace_with_na_at(TaskTwoAllnewRT1, "Sex", ~.x == "Männlich")
Task2ttestg <- na.omit((Task2ttestg))

t.test(Task2ttestg$rtCosts, mu= 0, alt= "less")

#no need for a ost hoc test

descriptivesgender <- anovazeuggender2 %>% group_by(Sex) %>%
   
   summarize(
      Mean = mean(rtCosts)
      , SD = sd(rtCosts))
descriptivesgender[, -1] <- printnum(descriptivesgender[, -1])

Plot2WayANOVA(rtCosts~LossOrReward*Sex,
              dataframe = anovazeuggender2,
              confidence=.95,
              plottype = "line",
              errorbar.display = "CI",
              xlab = "Condition",
              ylab = "rtcostss",
              title = NULL,
              subtitle = NULL,
              interact.line.size = 2,
              ci.line.size = 1,
              mean.label = FALSE,
              mean.ci = TRUE,
              mean.size = 4,
              mean.shape = 23,
              mean.color = "darkred",
              mean.label.size = 3,
              mean.label.color = "black",
              offset.style = "none",
              overlay.type = NULL,
              posthoc.method = "scheffe",
              show.dots = TRUE,
              PlotSave = FALSE,
              ggtheme = ggplot2::theme_bw(),
              package = "RColorBrewer",
              palette = "Dark2",
              ggplot.component = theme_apa())
            ggsave("intwractionsex.png", width = 16, height = 9, units = "in")

            
            descriptivesrtgender <- TaskTwoAllnewRT1 %>% group_by(LossOrReward, Sex) %>%
               #  filter(correct>0)%>%
               #  filter(blockCount<2)%>%
               summarize(
                  Mean = mean(rtCosts)
                  , Median = median(rtCosts)
                  , SD = sd(rtCosts)
                  , Min = min(rtCosts)
                  , Max = max(rtCosts)
               )
            descriptivesrtgender[, -1] <- printnum(descriptivesrtgender[, -1])
            
            #apa table
            options(contrasts = c("contr.helmert", "contr.poly"))
            output <- aov(rtCosts~LossOrReward*Sex, data = TaskTwoAllnewRT1)
            apa.aov.table(output, filename = "2wayanovadsexcosts.doc")
            
  #ok the descrrpitive data i guess...          
            apa.2way.table(
               Sex,
               LossOrReward,
               rtCosts,
               anovazeuggender2,
               filename = "aptable.doc",
               table.number = NA,
               show.conf.interval = FALSE,
               show.marginal.means = FALSE,
               landscape = TRUE
            )
 #distractabltiy           
            
            
            anovazeugdistract2 <- aggregate(rtCosts ~ PID + LossOrReward + Distractability,
                                            data = TaskTwoAllnewRT1,
                                            FUN = mean)
            
            anovazeugdistract2$PID <- factor(anovazeugdistract2$PID)
            anovazeugdistract2$LossOrReward <- factor(anovazeugdistract2$LossOrReward)
            anovazeugdistract2$Distractability <- factor(anovazeugdistract2$Distractability)
            
            anovadistract2 <- aov(rtCosts~LossOrReward*Distractability+
                                     Error(PID),
                                  data=anovazeugdistract2)
            summary(anovadistract2)
            
            
            
            
            
            options(contrasts = c("contr.helmert", "contr.poly"))
            output <- aov(rtCosts~ PID + LossOrReward*Distractability, data = TaskTwoAllnewRT1)
            apa.aov.table(output, filename = "anova_distract_costs2.doc")
            
            
            
Plot2WayANOVA(rtCosts~LossOrReward*Distractability,
                          dataframe = anovazeugdistract2,
                          confidence=.95,
                          plottype = "line",
                          errorbar.display = "CI",
                          xlab = "Condition",
                          ylab = "RT costs [ms]",
                          title = NULL,
                          subtitle = NULL,
                          interact.line.size = 2,
                          ci.line.size = 1,
                          mean.label = FALSE,
                          mean.ci = TRUE,
                          mean.size = 4,
                          mean.shape = 23,
                          mean.color = "darkred",
                          mean.label.size = 3,
                          mean.label.color = "black",
                          offset.style = "none",
                          overlay.type = NULL,
                          posthoc.method = "scheffe",
                          show.dots = TRUE,
                          PlotSave = FALSE,
                          ggtheme = ggplot2::theme_bw(),
                          package = "RColorBrewer",
                          palette = "Dark2",
                          ggplot.component = theme_apa())
            ggsave("intwractiondistract.png", width = 16, height = 9, units = "in")
            
            apa.2way.table(
               Distractability,
               LossOrReward,
               rtCosts,
               anovazeugdistract2,
               filename = "aptable2.doc",
               table.number = NA,
               show.conf.interval = FALSE,
               show.marginal.means = FALSE,
               landscape = TRUE)
 
            writeClipboard(as.character(pwc))
            writeClipboard(as.character(pwc), format = 1)
writeClipboard(as.character(descriptivesrtgender))
writeClipboard(as.character(descriptivesrt))
writeClipboard(as.character(descriptivesacc))
writeClipboard(as.character(descriptivesrt2))

ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 3, method="circle", colors = c("blue", "white", "red"), outline.color = "gray", show.legend = TRUE, show.diag = FALSE, title="Correlogram of loan variables")