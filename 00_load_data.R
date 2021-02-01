bonusEurosEarned <- function(loadFile, ParticipantN) {
  points <- read_file(loadFile) %>% 
    # ... separate out the different jatos components
    str_split('\n') %>% 
    first() %>% 
    # ... select the 5th component (task 1)
    nth(5) %>% 
    # ... remove some extra information form the end of the file
    str_split(',\"context\":') %>%
    first() %>%
    first() %>%
    str_remove(fixed("{\"data\":")) %>%
    # ... parse JSON into a data.frame
    map_dfr(fromJSON, flatten=T) %>%
    filter(count_Search_display == 255) %>%
    mutate(pointsTotal = orangetotal + lilactotal + greentotal + turquoisetotal) %>%
    select(pointsTotal) %>%
    pluck(1)
  
  money = round(points * (3/672),2)
  return(money)
}





loadTaskOne <- function(loadFile, ParticipantN) {
#Get task 1 data
# Read the text file from JATOS ...
Task1 <- read_file(loadFile) %>% 
  # ... separate out the different jatos components
  str_split('\n') %>% 
  first() %>% 
  # ... select the 5th component (task 1)
  nth(5) %>% 
  # ... remove some extra information form the end of the file
  str_split(',\"context\":') %>%
  first() %>%
  first() %>%
  str_remove(fixed("{\"data\":")) %>%
  # ... parse JSON into a data.frame
  map_dfr(fromJSON, flatten=T) %>%
  # ... create a column which stores the colour that was shown on each trial
  mutate(colour = str_remove_all(paste(stim1,stim2,stim3,stim4,stim5,stim6,sep=""), "greyDistr.bmp"),
         colour = str_remove(colour, ".bmp"),
         colour = substr(colour,1,nchar(colour)-1)) %>%
  # ... create column that has the value for each colour
  mutate(value = ifelse(colour == "Green",greenvalcor+greenvalinc,
                        ifelse(colour == "Orange", orangevalcor+orangevalinc,
                               ifelse(colour == "Lilac", lilacvalcor+lilacvalinc,turquoisevalcor+turquoisevalinc)))) %>%
  # ... create column that turns the value number into condidtion name
  mutate(valueCondition = ifelse(value == -2,"Negative",
                                 ifelse(value == 0, "Neutral",
                                        ifelse(value == 2, "Low","High")))) %>%
  mutate(Task = "LearnTask", PID = ParticipantN) %>%
  filter(practice == "no") %>%
  mutate(rt = response_time, blockCount = count_block_loop, trial_index = row_number()) %>%
  select(c("rt", "correct", "colour","value", "valueCondition", "Task", "PID", "blockCount", "trial_index"))


#Task1 <- Task1 %>% select(-value)
return(Task1)
}

createColourKey <- function(Task1) {
# Create a key for this participant that shows which colour is assigned to each value condition
colourKey = Task1 %>%
  group_by(colour) %>%
  summarise(value = mean(value)) %>%
  mutate(valueCondition = ifelse(value == -2,"Negative",
                                 ifelse(value == 0, "Neutral",
                                        ifelse(value == 2, "Low","High"))))
}

loadTaskTwo <- function(loadFile, colourKey, ParticipantN) {
#Get task 2 data
# Read the text file from JATOS
Task2 <- read_file(loadFile) %>% 
  # ... separate out the different jatos components
  str_split('\n') %>% 
  first() %>% 
  # ... select the 7th component (task 2)
  nth(7) %>% 
  # ... remove some extra information form the end of the file
  str_split(',\"context\":') %>%
  first() %>%
  first() %>%
  str_remove(fixed("{\"data\":")) %>%
  # ... parse JSON into a data.frame
  map_dfr(fromJSON, flatten=T) %>%
  mutate(distractor_colour = ifelse(distractor_colour=="red","Orange",
                                    ifelse(distractor_colour == "blue", "Turquoise",
                                           ifelse(distractor_colour == "green", "Green",
                                                  ifelse(distractor_colour == "lilac", "Lilac", "Grey"))))) %>%
  mutate(rt = response_time, blockCount = count_block_loop, Task = "ContextTask", PID = ParticipantN, colour = distractor_colour,  trial_index = row_number()) %>%
  select(c("rt", "blockCount","correct", "colour", "Task", "PID", "trial_index" ))

# attach the value conditions assigned in task 1, to the data in task 2 using hte colour key
Task2$valueCondition = "None"
for (row in 1:length(Task2$correct)) {
  Task2Cond = Task2$colour[row]
  condition = colourKey$valueCondition[colourKey$colour==Task2$colour[row]]
  Task2$valueCondition[row] = ifelse(Task2Cond=="Grey","None",condition)
}

return(Task2)
}


#i tried to make a function to load the info 

loadinfo <- function(loadFile, ParticipantN) {
info <- read_file(loadFile) %>% 
  str_split('\n') %>% 
  first() %>% 
  nth(4) %>% 
  map_dfr(fromJSON, flatten=T) %>%
  select(c("Age", "Sex", "hand"))

return(info)
}