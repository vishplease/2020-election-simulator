library(shiny)
library(shinycssloaders)
library(tidyverse)
library(data.table)
library(usmap)
library(maptools)

poll <- read_csv("data/poll_viz.csv")

states <- read_csv("data/states.csv")

poll$state <- gsub("-", " ", poll$state)

states <- states %>% 
    select(abbrev, lat, lon) %>% 
    filter(abbrev != "DC")

#poll <- poll %>% 
   # left_join(states, by = "abbrev")

# FACTOR QUESTIONS
question_vector <- poll %>% 
    group_by(q_id, question) %>% 
    summarize() 

function(input, output, session) {
    
    ####################################################
    # POPULATE DROP DOWN 1
    ####################################################
    
    updateSelectInput(session, "question1_update_input", choices = question_vector$question)
    
    ####################################################
    # POPULATE DROP DOWN 2
    ####################################################
    
    observeEvent(c(input$question1_update_input),
                 {
                     
                     # GENERATE CHOICES BASED ON QUESTION SELECTION
                     options <- poll %>% 
                         filter(question == input$question1_update_input) %>% 
                         select(choices) %>% 
                         unique() %>% 
                         pull(choices)
                     
                     # UPDATE DROP DOWN WITH CHOICES
                     updateSelectInput(session, "choices1_update_input", choices = options)
                     
                 })
    
    ####################################################
    # OUTPUT MAP 1
    ####################################################
    
    output$map1 <- renderPlot({
        
        req(input$question1_update_input, input$choices1_update_input)
        
        ########################################################
        # GENERATE PLOT
        ########################################################
        
         q_select <- input$question1_update_input # SELECT QUESTION
        
        choice_select <- input$choices1_update_input #SELECT CHOICE
        
        # q_select <- "What is your age?"
        # choice_select <- "18-29"
        
        plot <- poll %>% 
            filter(question == q_select, choices == choice_select) %>% 
            mutate(winner = ifelse(is.na(Trump) == FALSE, ifelse(Trump-Biden > 1, "Trump", "Biden"), "Unknown")) %>% 
            mutate(winner = factor(winner, levels = c("Trump", "Unknown", "Biden"))) %>% 
            mutate(margin = ifelse(is.na(Trump) == FALSE, Biden-Trump, NA)) %>% 
            mutate(marginfactor = case_when(margin > 20 ~ "Biden >20%",
                                            margin > 10 ~ "Biden 11-20%",
                                            margin > 0 ~ "Biden 1-10%",
                                            margin >= -10 ~ "Trump 1-10%",
                                            margin >= -20 ~ "Trump 11-20%",
                                            margin < -20 ~ "Trump >20%",
                                            TRUE ~ "Unknown"))
        
        plot$marginfactor <- factor(plot$marginfactor, levels = c("Trump >20%", 
                                                                  "Trump 11-20%",
                                                                  "Trump 1-10%",
                                                                  "Unknown", 
                                                                  "Biden 1-10%",
                                                                  "Biden 11-20%", 
                                                                  "Biden >20%"))
        
        
        ####################################################
        # MAP
        ####################################################
        
        
        map <- plot %>%
            group_by(state, marginfactor, votes, pct_voters) %>%
            summarize() %>% 
            plot_usmap(data = ., exclude = c("DC"), values = "marginfactor", labels = TRUE) +
            scale_fill_manual("Margin",
                              values = c("Trump >20%" = "#FF0803", 
                                         "Trump 11-20%" = "#FF5B57",
                                         "Trump 1-10%" = "#FFADAB",
                                         "Unknown" = "grey", 
                                         "Biden 1-10%" = "#84B4FC",
                                         "Biden 11-20%" = "#6178F4", 
                                         "Biden >20%" = "#1A00EF")) +
            labs(title = q_select,
                 subtitle = choice_select) +
            theme(legend.position = c(1, .5),
                  plot.title = element_text(hjust = 0.5, size = 15),
                  plot.subtitle = element_text(hjust = 0.5, size = 12))
        
        map$layers[[2]]$aes_params$size <- 2
        
        map
        
        }) 
    
    ####################################################
    # OUTPUT BAR CHART 1
    ####################################################    
    
    output$barchart1 <- renderPlot({
        
        
        ########################################################
        # GENERATE PLOT
        ########################################################
        
        q_select <- input$question1_update_input # SELECT QUESTION
        
        choice_select <- input$choices1_update_input #SELECT CHOICE 
        
        
        plot <- poll %>% 
            filter(question == q_select, 
                   choices == choice_select) %>% 
            mutate(winner = ifelse(is.na(Trump) == FALSE, ifelse(Trump-Biden > 1, "Trump", "Biden"), "Unknown")) %>% 
            mutate(winner = factor(winner, levels = c("Trump", "Unknown", "Biden")))
        
        plot %>% 
            group_by(winner) %>% 
            summarize(votes = sum(votes)) %>% 
            ggplot(aes(y = "results", x = votes, fill = winner)) +
            geom_bar(stat = "identity", position = "stack") +
            theme_minimal() +
            scale_fill_manual("winner", values = c("Trump" = "#FF0803", "Biden" = "#1A00EF", "Unknown" = "grey")) +
            labs(title = "Electoral Votes If Only This Group Voted",
                 subtitle = "Allocates full votes from each state to candidate who polled higher with specified group") +
            theme(legend.position = "none",
                  axis.title.y = element_blank(),
                  axis.title.x = element_blank(),
                  axis.text.y = element_blank(),
                  panel.grid.major.y = element_blank(),
                  plot.title = element_text(hjust = 0.5, size = 15),
                  plot.subtitle = element_text(hjust = 0.5, size = 10)
            ) +
            geom_text(aes(label = paste0(as.character(winner), "\n", votes)), size = 3, color = "white", position = position_stack(vjust = 0.5))
        
        
    }, height = 100)
}
