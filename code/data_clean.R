pacman::p_load(tidyverse, rvest, stringr, data.table)

full_poll <- read_csv("data/full_poll.csv")

states <- read_csv("data/states.csv")

states <- states %>% 
  filter(abbrev != "DC") 

poll <- data.frame(
  q_id = double(),
  question = character(),
  choices = character(),
  pct_voters = double(),
  Trump = double(),
  Biden = double(),
  state = character(),
  options = integer()
)

for(i in states$state) {
options <- full_poll %>% 
    filter(state == i) %>% 
    group_by(q_id) %>% 
    summarize(options = n())

partial_poll <- full_poll %>% 
  filter(state == i) %>% 
  left_join(options, by = "q_id")

poll <- rbind(poll, partial_poll)
  
}

# FIX SOME QIDS IN POLL

poll[31873:31876,8] <- 4
poll[31877,8] <- 2
poll[31879,8] <- 3

poll[31877,1] <- 11
poll[31877,2] <- "When did you vote?"

poll[31879,1] <- 12
poll[31879,2] <- "Which type of vote did you cast?"

poll[31882,1] <- 13
poll[31882,2] <- "Is this election your first time ever voting, or not?"

poll[27434,1] <- 123
poll[31882,2] <- "What is your racial or ethnic heritage? And the size of the place where you live?"
poll[27428:27433,8] <- 6




######################################################
# SELECT QUESTIONS FOR MAP
######################################################

#Remove redundant questions, questions with lots of NAs, or those with obvious voting patterns

top_qs <- poll %>% 
  mutate(unique_q = paste0(question,"_",options)) %>% 
  group_by(q_id, unique_q, state) %>% 
  summarize(optioncount = n(), na_count = sum(is.na(Trump))) %>% 
  group_by(q_id, unique_q) %>% 
  summarize(count = n(), optioncount = sum(optioncount), na_count = sum(na_count)) %>% 
  group_by(unique_q) %>% 
  summarize(count = sum(count), optioncount = sum(optioncount), na_count = sum(na_count)) %>% 
  mutate(pctna = na_count/optioncount) %>% 
  filter(pctna < .50, 
         count > 40,
         grepl("Thinking about the presidential", unique_q) == FALSE,
         grepl("Thinking about voting", unique_q) == FALSE,
         grepl("Do you think Donald Trump", unique_q) == FALSE,
         grepl("Are you the mother or father of any children under the age of 18?_3", unique_q) == FALSE,
         grepl("Do you have a favorable", unique_q) == FALSE,
         grepl("Do you approve or disapprove", unique_q) == FALSE,
         grepl("Do you consider yourself", unique_q) == FALSE,
         grepl("Do you favor", unique_q) == FALSE,
         grepl("Do you think corruption", unique_q) == FALSE,
         grepl("Do you think the Trump administration", unique_q) == FALSE,
         grepl("Do you think the condition of the nation’s economy is:_4", unique_q) == FALSE,
         grepl("Do you think the coronavirus in the United States is:_3", unique_q) == FALSE,
         grepl("House of Representatives", unique_q) == FALSE,
         grepl("Generally speaking, would you say things in this country", unique_q) == FALSE,
         grepl("labor union", unique_q) == FALSE,
         grepl("heritage? And the size", unique_q) == FALSE,
         grepl("Have you missed out on a major event, like a wedding or funeral, because of the coronavirus pandemic", unique_q) == FALSE,
         grepl("Have you or someone in your household lost a job or income because of the coronavirus pandemic?", unique_q) == FALSE,
         unique_q != "Do you consider yourself to be a liberal, moderate, or conservative?_5",
         unique_q != "How important is it to you for the next president to shake up the political system?_2",
         unique_q != "How serious a problem is racism in policing?_4",
         unique_q != "How serious a problem is racism in U.S. society?_4",
         unique_q != "What is your age?_2",
         unique_q != "What is your age?_6",
         unique_q != "What is your present religion, if any?_5",
         unique_q != "What was your total household income in 2019?_2",
         unique_q != "What was your total household income in 2019?_3",
         unique_q != "Which best describes your level of education?_2",
         unique_q != "Which of the following best describes the area where you live?_4",
         unique_q != "Do you or does any other member of your household own a handgun, rifle, shotgun, or any other kind of firearm?_3",
         unique_q != "How old are you? And what is your gender?_5",
         unique_q != "What is your present religion, if any?_4",
         unique_q != "What is your racial or ethnic heritage? And how old are you?_7",
         unique_q != "What is your racial or ethnic heritage? And how old are you?_8",
         unique_q != "Thinking about the criminal justice system in the United States, would you say it:_4",
         unique_q != "What is your racial or ethnic heritage, level of education and gender?_9",
         grepl("comes close", unique_q) == FALSE,
         grepl("Roe", unique_q) == FALSE,
         grepl("Overall, do you approve or disapprove of the way Donald Trump", unique_q) == FALSE,
         grepl("Would you describe your vote", unique_q) == FALSE,
         grepl("shake up", unique_q) == FALSE,
         grepl("interference", unique_q) == FALSE,
         grepl("higher priority", unique_q) == FALSE,
         grepl("What is your political party", unique_q) == FALSE,
         grepl("Which one of the following statements", unique_q) == FALSE,
         grepl("Hillary", unique_q) == FALSE,
         grepl("first time", unique_q) == FALSE,
         grepl("first time", unique_q) == FALSE,
         grepl("Which type of vote did", unique_q) == FALSE,
         grepl("Would you say Joe Biden", unique_q) == FALSE,
         grepl("Would you say Donald Trump", unique_q) == FALSE,
         grepl("a more or less active role solving", unique_q) == FALSE) %>% 
  select(unique_q)

# INDEX TOP QUESTIONS

top_qs <- poll %>% 
  mutate(unique_q = paste0(question,"_",options)) %>% 
  filter(state == "california") %>% 
  select(unique_q) %>% 
  unique() %>% 
  rowid_to_column("q_id") %>% 
  filter(unique_q %in% top_qs$unique_q)

top_qs[20,]$q_id <- 6 # move marital question higher

top_qs <- top_qs %>% 
  arrange(q_id) %>% 
  select(-q_id) %>% 
  rowid_to_column("q_id") 



# FILTER MAIN POLL TABLE WITH TOP QS AND CORRECT INDEX
poll <- poll %>% 
  mutate(unique_q = paste0(question,"_",options)) %>% 
  filter(unique_q %in% top_qs$unique_q) %>% 
  select(-q_id) %>% 
  left_join(top_qs, by = "unique_q") %>% 
  select(-unique_q, -options) %>% 
  left_join(states, by = "state")

poll <- poll %>% 
  mutate(question = ifelse(question == "Are you:", "Marital Status", question))



fwrite(poll, "data/poll_viz.csv")

