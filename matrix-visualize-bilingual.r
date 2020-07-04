library(tidyverse)
library(likert)
library(xtable)
library(sjPlot)
library(HH)
library(data.table)
library(wesanderson)
library(here)
library(knitr)


### Loading data
raw <- read.csv(here::here("data", "Mid-TN-STEM-Hub-bilingual.csv")) %>%
    filter(Finished == TRUE)


### Gathering columns
data <- raw %>%
    dplyr::select(AGREE_1:AGREE_6) %>%
    gather(measure, response) 


### Creating the contingency table
contingencytable <- table(data$measure, data$response) %>% as.data.frame.matrix() 

# Reorder columns
contingencytable <- contingencytable %>% 
    data.table::setcolorder(c("Disagree",
                              "Neither agree nor disagree",
                              "Agree",
                              "Strongly agree"))

# Changing row name
rownames(contingencytable) <- c("The workshop was an \neffective use of my time",
                                "The workshop was understand-\nable, clear, and professional" ,
                                "The workshop leaders \nwere knowledgeable",
                                "The workshop was relevant \nto my professional context",
                                "The workshop addressed \na need I have as an educator",
                                "The workshop will help me to \nimprove my professional practice")

# adding rownames to columns
contingencytable <- tibble::rownames_to_column(contingencytable, var = "Measure")





### Visualization

likert(Measure ~ ., data=contingencytable, ylab=NULL,
       ReferenceZero=2, as.percent=TRUE,
       positive.order=FALSE, 
       main = list("Valuing and Leveraging Emerging Bilingual \nStudents’ Ways of Communicating About the Natural World \nWorkshop Evaluation",x=unit(.5, "npc")), 
       sub= list("Agreement level",x=unit(.5, "npc")), 
       xlim=c(-10,0,20,40,60,80,100))
