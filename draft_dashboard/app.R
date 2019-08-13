#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny); library(DT)

df <- read.csv("C:/Users/montg/Documents/active_projects/fantasy_football/projections.csv")
df <- dplyr :: arrange(df, (rank))
drafted <- df[0,]
load("C:/Users/montg/Documents/active_projects/fantasy_football/player_distributions.RData")


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Fantasy Draft Dashboard"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        numericInput("draft_pick", label = h3("Select Draft Pick"), value = 1, min = 1, max = 12),
        textOutput("pick_list"),
        uiOutput("drafted")
      ),
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Main",   DT :: dataTableOutput("main_tab")),
                    tabPanel("Drafted Tab", DT :: dataTableOutput("drafted_tab"), 
                             uiOutput("undrafted"))
                  )
        # DT :: dataTableOutput("main_tab"),
        # DT :: dataTableOutput("drafted_tab")
        # 
        
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   

df <- reactiveVal(df)
drafted <- reactiveVal(drafted)

output$drafted <- renderUI({
  req(length(input$main_tab_cell_clicked) > 0)
  actionButton("drafted_button", "Drafted!")
})

output$undrafted <- renderUI({
  req(length(input$drafted_tab_cell_clicked) > 0)
  actionButton("undrafted_button", "Undrafted!")
})

observeEvent(input$undrafted_button, {
  q = df()
  poop = drafted()
  if (!is.null(input$drafted_tab_rows_selected)) {
    q <- rbind (df(), poop[as.numeric(input$drafted_tab_rows_selected),])
    q <- arrange(q, rank)

    print(tail((q)))
    
    poop <- poop[-as.numeric(input$drafted_tab_rows_selected),]
    
  } 
  df(q)
  drafted(poop)
})


observeEvent(input$drafted_button, {
  t = df()
  poop = drafted()
  if (!is.null(input$main_tab_rows_selected)) {
    poop <- rbind (poop, t[as.numeric(input$main_tab_rows_selected),])
    t <- t[-as.numeric(input$main_tab_rows_selected),]

  }
  df(t)
  drafted(poop)
#  print(head(df()))
})

output$main_tab <- renderDT({
  datatable(df(), selection = 'single', options = list(dom = 't'))
})

output$drafted_tab <- renderDT ({
  
  datatable(drafted(), selection = 'single', options = list(dom = 't'))
})


draft_picks <- eventReactive( input$draft_pick, {
  i <- input$draft_pick
  round <- 1
  picks <- c()
  for (n in seq(1:14)){
    if (round %% 2 != 0){
      picks[n] <- ((round-1) * 12) + i 
      round <- round + 1
    }
    else{
      picks[n] <- picks[n - 1] + 2*(12-i) + 1
      round <- round + 1
    }


    
  }
  return(picks)
})

output$pick_list <- renderText({draft_picks()})


}

# Run the application 
shinyApp(ui = ui, server = server)

