pacman::p_load(tidyverse, rvest, jsonlite, stringr, DataExplorer, data.table, beepr, tictoc)


tic()
###################################################
# READ STATE CSV AND INITIATE LOOP
###################################################

states <- read_csv("data/states.csv")

state_loop <- states %>% 
  filter(abbrev != "DC") %>% 
  select(state) #couldn't find the poll for DC


# INITIATE EMPTY DATA FRAME FOR LOOP 

full_poll <- data.frame(
  result_id = double(),
  q_id = double(),
  question = character(),
  choices = character(),
  pct_voters = double(),
  Trump = double(),
  Biden = double(),
  state = character()
)

###################################################
# INITIATE LOOP
###################################################

for (i in state_loop$state) {

###################################################
# GENERATE URL
###################################################
state <- i

url <- paste0("https://www.nytimes.com/interactive/2020/11/03/us/elections/ap-polls-",state,".html")

###################################################
# GET POLL QUESTIONS
###################################################

questions <- read_html(url) %>% 
  html_nodes(".g-ques") %>% 
  html_text() %>% 
  as.data.frame()

colnames(questions) <- c("question")

questions <- rowid_to_column(questions, "q_id")

###################################################
# GET POLL CHOICES
###################################################


# SCRAPE DATA
choices <- read_html(url) %>% 
  html_nodes(".g-cat") %>% 
  html_text() %>% 
  as.data.frame() 

colnames(choices) <- c("choices")


# ASSIGN QUESTION ID TO EACH CHOICE

choices <- choices %>% 
  filter(choices != "") %>% 
  mutate(pct_voters = as.numeric(unlist(str_extract_all(choices, '\\d+(?=%)')))/100) %>%
  mutate(pct_voters = ifelse(grepl("<", choices) == TRUE,0,pct_voters)) %>% 
  mutate(choices = str_trim(str_extract(choices, '.*(?=[\\s|<|>]\\d+%)'))) %>%
  mutate(q_id = NA)


# The following loop works for all states except a few. 
# Making a few adjustments to the data
# so these states work in the loop and then fix values after loop complete.
if(state == "rhode-island") {
  choices[242,2] <- .54 # original 0
  choices[245,2] <- .54 # original 0
  choices[254,2] <- .18 # original 0.15
  choices[323,2] <- .02 # original 0
  choices[334,2] <- .05 # original 0.01
}

if(state == "oregon") {
  choices[38,2] <- .1 # original 0
}

if(state == "idaho") {
  choices[20,2] <- 0.04 #original .03
}

if(state == "wyoming") {
  choices[19,2] <- 0.26 #original .24
}

if(state == "south-dakota") {
  choices[19,2] <- 0.12 #original 0.1
}

if(state == "north-dakota") {
  choices[20,2] <- 0.04 #original 0
}

if(state == "wisconsin") {
  choices[19,2] <- 0.25 #original 0.22
}

if(state == "hawaii") {
  choices[19,2] <- 0.3 #original 0.28
}

if(state == "oklahoma") {
  #choices[19,2] <- 0.3 #original 0.28
}

if(state == "south-carolina") {
  choices[346,2] <- .03 #original 0
  choices[356,2] <- .02 #original 0
  
}

# ASSIGN UNIQUE ID TO EACH QUESTION

index <- 1

pct <- 0

for (row in 1:nrow(choices)) {
  pct <- pct + choices[row,]$pct_voters
  
  if (pct <= 1.03) {
    choices[row,]$q_id <- index
    
  } else {
    index <- index +1
    choices[row,]$q_id <- index
    pct <- choices[row,]$pct_voters

  }
  
  
}

# FIX DUMMY VALUES

if(state == "rhode-island") {
  choices[242,2] <- .0 # fixing adjusted values
  choices[245,2] <- .0 # fixing adjusted values
  choices[254,2] <- .15 # fixing adjusted values
  choices[323,2] <- .00 # fixing adjusted values
  choices[334,2] <- .01 # fixing adjusted values
}

if(state == "oregon") {
  choices[39,2] <- .00 # original 0
  choices[39,3] <- 10
  choices[41,3] <- 11
  choices[42,3] <- 11
  choices[44,3] <- 12
  
  
}

if(state == "idaho") {
  choices[20,2] <- 0.03 
}

if(state == "wyoming") {
  choices[19,2] <- 0.24 
}

if(state == "south-dakota") {
  choices[19,2] <- 0.1
}

if(state == "north-dakota") {
  choices[20,2] <- 0.00 
}

if(state == "wisconsin") {
  choices[19,2] <- 0.22 
}

if(state == "hawaii") {
  choices[19,2] <- 0.28 
}

if(state == "oklahoma") {
  choices[245,3] <- 85 
}

if(state == "south-carolina") {
  choices[346,2] <- 0 
  choices[356,2] <- 0
  
}

###################################################
# JOIN QUESTIONS AND CHOICES ON UNIQUE QUESTION ID
###################################################

poll <- questions %>% 
  left_join(choices, by = "q_id")

poll <- rowid_to_column(poll, "result_id")

###################################################
# SCRAPE ANSWERS
###################################################

results <- read_html(url) %>% 
  html_nodes(".g-top") %>% 
  html_text() %>% 
  as.data.frame()

colnames(results) <- c("results")


#USE REGEX TO EXTRACT NUMERIC VALUES
results <- results %>% 
  mutate(results = str_trim(results)) %>% #remove trailing spaces
  filter(results != "Donald Trump") %>% #filter out headers
  filter(results != "Joseph R Biden Jr.") %>% #filter out headers
  mutate(results = ifelse(is.na(str_extract(results, '.*(?=%)')) == FALSE, 
                          str_extract(results, '.*(?=%)'),
                          results)) %>% #remove % character
  mutate(results = ifelse(results == "—", NA, results)) %>% 
  mutate(candidate = NA) %>%  #add candidate row
  mutate(result_id = NA)



# ASSIGN CANDIDATE NAMES TO EACH POLL RESULT (ALTERNATING)

candidate <- "Trump"
id <- 1
counter <- 0

for (row in 1:nrow(results)) {
  
  if(candidate == "Trump") {
    results[row,]$candidate <- candidate 
    candidate <- "Biden"
    results[row,]$result_id <- id
  } else {
    results[row,]$candidate <- candidate 
    candidate <- "Trump"
    results[row,]$result_id <- id
    id <- id + 1
  }
  
}

results <- results %>% 
  pivot_wider(names_from = candidate, values_from = results)


###################################################
# CREATE FULL DATA FRAME FOR STATE
###################################################

poll <- poll %>% 
  left_join(results, by = "result_id") %>% 
  mutate(state = state) 

poll$Trump <- as.numeric(poll$Trump)
poll$Biden <- as.numeric(poll$Biden)

###################################################
# BIND TO FULL POLL DATAFRAME
###################################################

full_poll <- rbind(full_poll, poll)

Sys.sleep(3) # Delay 5 seconds between each scrape

}

full_poll <- full_poll %>% 
  select(-result_id)

###################################################
# SAVE DATA FRAME
###################################################

fwrite(full_poll, "data/full_poll.csv")

beep(2) # Beep upon completion

toc() #204 sec elapsed