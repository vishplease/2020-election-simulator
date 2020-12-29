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
        
        plot <- poll %>% 
            filter(question == q_select,
                   choices == choice_select) %>% 
            mutate(winner = ifelse(is.na(Trump) == FALSE, ifelse(Trump-Biden > 0, "Trump", "Biden"), "Unknown")) %>% 
            mutate(winner = factor(winner, levels = c("Trump", "Unknown", "Biden"))) %>% 
            select(-Biden, -Trump)
        
        ####################################################
        # MAP
        ####################################################
        
        
        map <- plot %>%
            group_by(state, winner, votes) %>%
            summarize() %>% 
            plot_usmap(data = ., exclude = c("DC"), values = "winner", labels = TRUE) +
            theme_minimal() +
            scale_fill_manual("Candidate", values = c("Trump" = "tomato", "Biden" = "dodgerblue3", "Unknown" = "grey")) +
            labs(title = q_select,
                 subtitle = choice_select) +
            theme(legend.position = "right",
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.title.y = element_blank(),
                  axis.title.x = element_blank(),
                  axis.text.y = element_blank(),
                  axis.text.x = element_blank(),
                  plot.title = element_text(hjust = 0.5),
                  plot.subtitle = element_text(hjust = 0.5))
        
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
            scale_fill_manual("winner", values = c("Trump" = "tomato", "Biden" = "dodgerblue3", "Unknown" = "grey")) +
            labs(title = "Electoral Votes") +
            theme(legend.position = "none",
                  axis.title.y = element_blank(),
                  axis.title.x = element_blank(),
                  axis.text.y = element_blank(),
                  panel.grid.major.y = element_blank(),
                  plot.title = element_text(hjust = 0.5)
            ) +
            geom_text(aes(label = paste0(as.character(winner), "\n", votes)), size = 3, color = "white", position = position_stack(vjust = 0.5))
        
        
    }, height = 100)
}
