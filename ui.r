ui <- fluidPage(
  
  titlePanel("2020 Election Simulator"),
  
  sidebarLayout(
    
    ########################################
    # INPUTS IN SIDEBAR
    ########################################
    sidebarPanel(
      
      h5("This RShiny app uses AP exit poll data scraped from NY Times to simulate how the 2020 US election would turn out if only a certain group voted"),
      
      selectInput(inputId = "question1_update_input",
                  label = "Choose Exit Poll Question", 
                  choices = NULL),
      selectInput(inputId = "choices1_update_input",
                  label = "Choose Poll Response", 
                  choices = NULL)
      
    ),
    
    ########################################
    # PLOTS IN MAIN PANEL
    ########################################
    
    mainPanel(
      h4("This map highlights the specified group's political preferences in each state and the candidate's corresponding margin of victory"),
      hr(),
      shinycssloaders::withSpinner(plotOutput("map1"), type = 4),
      shinycssloaders::withSpinner(plotOutput("barchart1"), type = 0),
      hr(),
      print(HTML("Developed in R by Vishal Chandawarkar <br/> <a href='http://www.vishplease.com/'>www.vishplease.com</a>"))
      
    )
    
  )
  
)