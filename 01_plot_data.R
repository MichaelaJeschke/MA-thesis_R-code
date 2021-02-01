screenParticipant1 <- function(my_data) {
#Calculate mean RT for all trials while removing non-responses (trials where rt == NA)
meanRTtotal <- my_data %>%
  summarise(mean = mean(rt, na.rm = TRUE), n=n(), sd=sd(rt, na.rm = TRUE))
#Calculate mean accuracy for all trials while removing non-responses (trials where rt == NA)
Accuracytotal <- my_data %>%
  summarise(Accuracy = mean(correct, na.rm = TRUE))

#Create plot showing the speed accuracy tradeoff for this participant 
#I do this by creating 20 bins based on trial rt (the fastest 5% of trials in bin 1, the next fastest 5% in bin 2 etc.) and calculating acc for each bin
SAtradeoff <- my_data %>%
  filter(!is.na(rt)) %>%
  mutate(RT_Ventile = ntile(rt,20)) %>%
  group_by(RT_Ventile) %>%
  summarise(Avg_Acc= mean(correct), Avg_RT = mean(rt)) %>%
  ggplot(., aes(x=Avg_RT,y=Avg_Acc)) +
  xlim(0,1400) +
  xlab("Average RT [ms]") +
  ylab("Average Accuracy") +
  geom_point() +
  theme_apa()

#Create a plot showing accuracy across the experiment blocks
#This creates a stacked bar chart, one bar per block showing the proportion of correct, incorrect, and non-responses
AccuracyPlot <- my_data %>%
  mutate(responseType = ifelse(rt>740,"No Response",ifelse(correct==0,"Incorrect","Correct")), responseType = factor(responseType, levels=c("No Response","Incorrect", "Correct"))) %>%
  group_by(blockCount,responseType) %>%
  summarise(Total = n()) %>%
  ggplot(., aes(x=blockCount, y=Total, fill = responseType)) + geom_bar(position = "fill",stat="identity") +
  scale_fill_manual(values=c("Gray", "Red", "Green4")) +
  geom_hline(yintercept = 0.6,color="yellow") +
  geom_hline(yintercept = Accuracytotal$Accuracy,color="black") +
  xlab("Block count") +
  ylab("Proportion") +
  theme(legend.position = "None", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#Create a pie chart to show the proportion of non-responses to responses for the whole expriment --- i changed that a bit ...
NonRespPie <- my_data %>%
  mutate(Response = ifelse(is.na(rt),"other",ifelse(rt<740, "Resp.", "No Resp.")),  Response = factor(Response, levels=c("other", "Resp.", "No Resp."))) %>%
  group_by(Response) %>%
  summarise(n = n()) %>%
  ggplot(., aes(x="",y=n, fill=Response)) +
  scale_fill_manual(values=c("cornflowerblue", "darkgoldenrod3")) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme(legend.position = "bottom",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())


#Creates a plot of the response time distribution for this participant
#A yellow line marks 250ms because faster than this would be considered random button pushing
#A black line marks their mean RT
RTDist <- my_data %>%
  mutate(correctB = ifelse(correct==1,"C","I"))%>%
  filter(!is.na(rt)) %>%
  ggplot(., aes(x=rt, color = correctB, fill = correctB)) + 
  geom_histogram(binwidth = 25, position = "dodge", alpha= 0.3) + 
  scale_color_manual(values=c("green2", "red")) +
  scale_fill_manual(values=c("green2", "red")) +
  xlab("Reaction time [ms]") +
  ylab("Frequency") +
  geom_vline(xintercept=meanRTtotal$mean, color="black") +
  geom_vline(xintercept = 200,color="yellow") +
  coord_cartesian(xlim = c(0,1400)) +
  theme(legend.position="None",
        panel.background = element_blank())

#Creates a plot showing their RT for every trial across the experiment
RTTimeline <- my_data %>%
  ggplot(., aes(x=trial_index, y=rt)) + 
  geom_line() +
  ylim(0,1400) +
  xlab("Trial index") +
  ylab("Reaction time [ms]") +
  theme_apa()

#Create a layout for a plot to combine all of the above plots
layout <- "
BE
CA
DD
"
#Combine all above plots into one image
NonRespPie + AccuracyPlot + RTDist + RTTimeline + SAtradeoff +
  plot_layout(design = layout)
}

screenParticipant2 <- function(my_data) {
#Calculate mean RT for all trials while removing non-responses (trials where rt == NA)
meanRTtotal <- my_data %>%
  summarise(mean = mean(rt, na.rm = TRUE), n=n(), sd=sd(rt, na.rm = TRUE))
#Calculate mean accuracy for all trials while removing non-responses (trials where rt == NA)
  Accuracytotal <- my_data %>%
    summarise(Accuracy = mean(correct, na.rm = TRUE))
 
#Create plot showing the speed accuracy tradeoff for this participant 
  #I do this by creating 20 bins based on trial rt (the fastest 5% of trials in bin 1, the next fastest 5% in bin 2 etc.) and calculating acc for each bin
  SAtradeoff <- my_data %>%
    filter(!is.na(rt)) %>%
    mutate(RT_Ventile = ntile(rt,20)) %>%
    group_by(RT_Ventile) %>%
    summarise(Avg_Acc= mean(correct), Avg_RT = mean(rt)) %>%
    ggplot(., aes(x=Avg_RT,y=Avg_Acc)) +
    xlim(0,1400) +
    xlab("Average RT [ms]") +
    ylab("Average Accuracy") +
    geom_point() +
    theme_apa()
  
  #Create a plot showing accuracy across the experiment blocks
  #This creates a stacked bar chart, one bar per block showing the proportion of correct, incorrect, and non-responses # letzte wasich emacht habe war bei factor die letzten beiden vertauschen
  
AccuracyPlot <- my_data %>%
    mutate(responseType = ifelse(rt>1790,"No Response",ifelse(correct==0,"Incorrect","Correct")), responseType = factor(responseType, levels=c("No Response","Incorrect", "Correct"))) %>%
    group_by(blockCount,responseType) %>%
    summarise(Total = n()) %>%
    ggplot(., aes(x=blockCount, y=Total, fill = responseType)) + geom_bar(position = "fill",stat="identity") +
    scale_fill_manual(values=c( "Gray", "Red", "Green4")) +
    geom_hline(yintercept = 0.6,color="yellow") +
    geom_hline(yintercept = Accuracytotal$Accuracy,color="black") +
    xlab("Block count") +
    ylab("Proportion") +
    theme(legend.position = "None", 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"))


  
  #Create a pie chart to show the proportion of non-responses to responses for the whole expriment --- 
  NonRespPie <- my_data %>%
    mutate(Response = ifelse(is.na(rt),"other",ifelse(rt<1790, "Resp.", "No Resp.")),  Response = factor(Response, levels=c("other", "Resp.", "No Resp."))) %>%
    group_by(Response) %>%
    summarise(n = n()) %>%
    ggplot(., aes(x="",y=n, fill=Response)) +
    scale_fill_manual(values=c("cornflowerblue", "darkgoldenrod3")) +
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0) +
    theme(legend.position = "bottom",
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank())
  
  
  #Creates a plot of the response time distribution for this participant
  #A yellow line marks 250ms because faster than this would be considered random button pushing
  #A black line marks their mean RT
  RTDist <- my_data %>%
    mutate(correctB = ifelse(correct==1,"C","I"))%>%
    filter(!is.na(rt)) %>%
    ggplot(., aes(x=rt, color = correctB, fill = correctB)) + 
    geom_histogram(binwidth = 25, position = "dodge", alpha= 0.3) + 
    scale_color_manual(values=c("green2", "red")) +
    scale_fill_manual(values=c("green2", "red")) +
    geom_vline(xintercept=meanRTtotal$mean, color="black") +
    geom_vline(xintercept = 200,color="yellow") +
    coord_cartesian(xlim = c(0,1400)) +
    xlab("Reaction time [ms]") +
    ylab("Frequency") +
    theme(legend.position="None",
          panel.background = element_blank())
  
  #Creates a plot showing their RT for every trial across the experiment
  RTTimeline <- my_data %>%
    ggplot(., aes(x=trial_index, y=rt)) + 
    geom_line() +
    ylim(0,1400) +
    xlab("Trial index") +
    ylab("Reaction time [ms]") +
    theme_apa()
  
  
  #Create a layout for a plot to combine all of the above plots
  layout <- "
BE
CA
DD
"
  #Combine all above plots into one image
  NonRespPie + AccuracyPlot + RTDist + RTTimeline + SAtradeoff +
    plot_layout(design = layout)
}


screenParticipant3 <- function(my_data) {
  #Calculate mean RT for all trials while removing non-responses (trials where rt == NA)
  meanRTtotal <- my_data %>%
    summarise(mean = mean(rt, na.rm = TRUE), n=n(), sd=sd(rt, na.rm = TRUE))
  #Calculate mean accuracy for all trials while removing non-responses (trials where rt == NA)
  Accuracytotal <- my_data %>%
    summarise(Accuracy = mean(correct, na.rm = TRUE))
  
  #Create plot showing the speed accuracy tradeoff for this participant 
  #I do this by creating 20 bins based on trial rt (the fastest 5% of trials in bin 1, the next fastest 5% in bin 2 etc.) and calculating acc for each bin
  SAtradeoff <- my_data %>%
    filter(!is.na(rt)) %>%
    mutate(RT_Ventile = ntile(rt,20)) %>%
    group_by(RT_Ventile) %>%
    summarise(Avg_Acc= mean(correct), Avg_RT = mean(rt)) %>%
    ggplot(., aes(x=Avg_RT,y=Avg_Acc)) +
    xlim(0,1400) +
    xlab("Average RT [ms]") +
    ylab("Average Accuracy") +
    geom_point() +
    theme_apa()
  
  #Create a plot showing accuracy across the experiment blocks
  #This creates a stacked bar chart, one bar per block showing the proportion of correct, incorrect, and non-responses # letzte wasich emacht habe war bei factor die letzten beiden vertauschen
  
  AccuracyPlot <- my_data %>%
    mutate(responseType = ifelse(correct==0,"Incorrect","Correct"), responseType = factor(responseType, levels=c("Incorrect", "Correct"))) %>%
    group_by(blockCount,responseType) %>%
    summarise(Total = n()) %>%
    ggplot(., aes(x=blockCount, y=Total, fill = responseType)) + geom_bar(position = "fill",stat="identity") +
    scale_fill_manual(values=c("Red", "Green4")) +
    geom_hline(yintercept = 0.6,color="yellow") +
    geom_hline(yintercept = Accuracytotal$Accuracy,color="black") +
    xlab("Block count") +
    ylab("Proportion") +
    theme(legend.position = "None", 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"))
  
  
  
  #Create a pie chart to show the proportion of non-responses to responses for the whole expriment --- 
  NonRespPie <- my_data %>%
    mutate(Response = ifelse(is.na(rt),"other", "Resp."),  Response = factor(Response, levels=c("other", "Resp."))) %>%
    group_by(Response) %>%
    summarise(n = n()) %>%
    ggplot(., aes(x="",y=n, fill=Response)) +
    scale_fill_manual(values=c("cornflowerblue", "darkgoldenrod3")) +
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0) +
    theme(legend.position = "bottom",
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank())
  
  
  #Creates a plot of the response time distribution for this participant
  #A yellow line marks 250ms because faster than this would be considered random button pushing
  #A black line marks their mean RT
  RTDist <- my_data %>%
    mutate(correctB = ifelse(correct==1,"C","I"))%>%
    filter(!is.na(rt)) %>%
    ggplot(., aes(x=rt, color = correctB, fill = correctB)) + 
    geom_histogram(binwidth = 25, position = "dodge", alpha= 0.3) + 
    scale_color_manual(values=c("green2", "red")) +
    scale_fill_manual(values=c("green2", "red")) +
    geom_vline(xintercept=meanRTtotal$mean, color="black") +
    geom_vline(xintercept = 200,color="yellow") +
    coord_cartesian(xlim = c(0,1400)) +
    xlab("Reaction time [ms]") +
    ylab("Frequency") +
    theme(legend.position="None",
          panel.background = element_blank())
  
  #Creates a plot showing their RT for every trial across the experiment
  RTTimeline <- my_data %>%
    ggplot(., aes(x=trial_index, y=rt)) + 
    geom_line() +
    ylim(0,1400) +
    xlab("Trial index") +
    ylab("Reaction time [ms]") +
    theme_apa()
  
  
  #Create a layout for a plot to combine all of the above plots
  layout <- "
BE
CA
DD
"
  #Combine all above plots into one image
  NonRespPie + AccuracyPlot + RTDist + RTTimeline + SAtradeoff +
    plot_layout(design = layout)
}


screenParticipant4 <- function(my_data) {
  #Calculate mean RT for all trials while removing non-responses (trials where rt == NA)
  meanRTtotal <- my_data %>%
    summarise(mean = mean(rt, na.rm = TRUE), n=n(), sd=sd(rt, na.rm = TRUE))
  #Calculate mean accuracy for all trials while removing non-responses (trials where rt == NA)
  Accuracytotal <- my_data %>%
    summarise(Accuracy = mean(correct, na.rm = TRUE))
  
  #Create plot showing the speed accuracy tradeoff for this participant 
  #I do this by creating 20 bins based on trial rt (the fastest 5% of trials in bin 1, the next fastest 5% in bin 2 etc.) and calculating acc for each bin
  SAtradeoff <- my_data %>%
    filter(!is.na(rt)) %>%
    mutate(RT_Ventile = ntile(rt,20)) %>%
    group_by(RT_Ventile) %>%
    summarise(Avg_Acc= mean(correct), Avg_RT = mean(rt)) %>%
    ggplot(., aes(x=Avg_RT,y=Avg_Acc)) +
    xlim(0,1400) +
    xlab("Average RT [ms]") +
    ylab("Average Accuracy") +
    geom_point() +
    theme_apa()
  
  #Create a plot showing accuracy across the experiment blocks
  #This creates a stacked bar chart, one bar per block showing the proportion of correct, incorrect, and non-responses # letzte wasich emacht habe war bei factor die letzten beiden vertauschen
  
  AccuracyPlot <- my_data %>%
    mutate(responseType = ifelse(correct==0,"Incorrect","Correct"), responseType = factor(responseType, levels=c("Incorrect", "Correct"))) %>%
    group_by(blockCount,responseType) %>%
    summarise(Total = n()) %>%
    ggplot(., aes(x=blockCount, y=Total, fill = responseType)) + geom_bar(position = "fill",stat="identity") +
    scale_fill_manual(values=c("Red", "Green4")) +
    geom_hline(yintercept = 0.6,color="yellow") +
    geom_hline(yintercept = Accuracytotal$Accuracy,color="black") +
    xlab("Block count") +
    ylab("Proportion") +
    theme(legend.position = "None", 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"))
  
  
  
  #Create a pie chart to show the proportion of non-responses to responses for the whole expriment --- 
  NonRespPie <- my_data %>%
    mutate(Response = ifelse(is.na(rt),"other","Resp."),  Response = factor(Response, levels=c("other", "Resp."))) %>%
    group_by(Response) %>%
    summarise(n = n()) %>%
    ggplot(., aes(x="",y=n, fill=Response)) +
    scale_fill_manual(values=c("cornflowerblue", "darkgoldenrod3")) +
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0) +
    theme(legend.position = "bottom",
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank())
  
  
  #Creates a plot of the response time distribution for this participant
  #A yellow line marks 250ms because faster than this would be considered random button pushing
  #A black line marks their mean RT
  RTDist <- my_data %>%
    mutate(correctB = ifelse(correct==1,"C","I"))%>%
    filter(!is.na(rt)) %>%
    ggplot(., aes(x=rt, color = correctB, fill = correctB)) + 
    geom_histogram(binwidth = 25, position = "dodge", alpha= 0.3) + 
    scale_color_manual(values=c("green2", "red")) +
    scale_fill_manual(values=c("green2", "red")) +
    geom_vline(xintercept=meanRTtotal$mean, color="black") +
    geom_vline(xintercept = 200,color="yellow") +
    coord_cartesian(xlim = c(0,1400)) +
    xlab("Reaction time [ms]") +
    ylab("Frequency") +
    theme(legend.position="None",
          panel.background = element_blank())
  
  #Creates a plot showing their RT for every trial across the experiment
  RTTimeline <- my_data %>%
    ggplot(., aes(x=trial_index, y=rt)) + 
    geom_line() +
    ylim(0,1400) +
    xlab("Trial index") +
    ylab("Reaction time [ms]") +
    theme_apa()
  
  
  #Create a layout for a plot to combine all of the above plots
  layout <- "
BE
CA
DD
"
  #Combine all above plots into one image
  NonRespPie + AccuracyPlot + RTDist + RTTimeline + SAtradeoff +
    plot_layout(design = layout)
}