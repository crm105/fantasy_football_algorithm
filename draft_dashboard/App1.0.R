#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

setwd("C:/Users/montg/Documents/active_projects/fantasy_football")
library(shiny); library(DT); library(dplyr); library(Oarray)

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
counter <- reactiveValues(countervalue = 1)

output$drafted <- renderUI({
  req(length(input$main_tab_cell_clicked) > 0)
  actionButton("drafted_button", "Drafted!")
})

output$undrafted <- renderUI({
  req(length(input$drafted_tab_cell_clicked) > 0)
  actionButton("undrafted_button", "Undrafted!")
})

observeEvent(input$undrafted_button, {
  counter$countervalue <- counter$countervalue - 1
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
  counter$countervalue <- counter$countervalue + 1
  t = df()
  poop = drafted()
  if (!is.null(input$main_tab_rows_selected)) {
    poop <- rbind (poop, t[as.numeric(input$main_tab_rows_selected),])
    t <- t[-as.numeric(input$main_tab_rows_selected),]

  }
  df(t)
  drafted(poop)
# print(head(df()))
})

output$main_tab <- renderDT({
  datatable(df(), selection = 'single')
})

output$drafted_tab <- renderDT ({
  
  datatable(drafted(), selection = 'single')
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


#Step 1: Calculate player probabilities at each pick.
player_probabilities <- observeEvent(c(draft_picks(), df()),{
  proj <- df()
round <- 1


probabilities <- list()
for (i in names(player_dist)){
  pick_probs <- c()
  a <- 1
  for (j in draft_picks()){
    
    prob <- integrate(approxfun (density (player_dist[[i]]), rule = 2, method = "constant"),subdivisions=2000, lower = 0, upper = j, abs.tol = .0002)
    pick_probs[a] <- prob$value
    a <- a + 1
  }
  probabilities[[i]] <- pick_probs 
}


#print(probabilities[[1]])



#Step 2: Combine Player Probabilities with Projections
#print(head(proj))



df_prob <- data.frame(matrix(ncol = length(draft_picks()) + 1, nrow = 1))
x <- c("name", "rd1", "rd2", "rd3", "rd4", "rd5", "rd6", "rd7", "rd8", "rd9", "rd10", "rd11", "rd12", "rd13", "rd14")
colnames(df_prob) <- x

for (i in 1:length(probabilities)){
 
   df_prob <- rbind(df_prob, c(names(probabilities[i]), probabilities[[i]]))
  
}
df_prob <- na.omit(df_prob)
df_prob <- merge(proj[,c("points", "name", "pos")], df_prob)
df_prob[,2:16] <- sapply( df_prob[,2:16], as.numeric )
df_prob$name <- as.character(df_prob$name)

drop_players <- c("CINCINNATI", "OAKLAND")
df_prob <- df_prob[!(df_prob$name %in% drop_players),]
df_prob <- arrange(df_prob, desc(points))



round <- 1

vorp_frame <- data.frame(matrix(ncol = 5, nrow = 1))
x <- c( "DST", "QB", "RB", "TE", "WR")
colnames(vorp_frame) <- x
agg_points <- list()
agg_pots <- list()

for (i in draft_picks()){
  expected_pos_points <- c()
  for (j in unique(df_prob$pos)) {
    
    #prob_take <- Oarray :: Oarray(0 : nrow(df_prob[df_prob$pos == j,])     , offset = 0)
    prob_take <- c(0)
    prob_available <- c()
    player <- c()
    points <- c()
    
    for (q in 1:nrow(df_prob[df_prob$pos == j,])) {
      prob_available[q] <- (1 - as.numeric(df_prob[ df_prob$pos == j, 3 + round][q]))
      prob_take[q] <- prob_available[q] * (1 - sum (prob_take)) 
      player[q] <- df_prob[df_prob$pos == j, "name"][q - 1]
      points[q] <- prob_take[q] * df_prob[df_prob$pos == j, 'points'][q]
      
      
    } 
    
    expected_pos_points[j] <- sum(points) 
  }
  vorp_frame <- rbind (vorp_frame, expected_pos_points)
  round <- round + 1
  
}
vorp_frame <- na.omit(vorp_frame)
print(counter$countervalue)



})






}

# Run the application 
shinyApp(ui = ui, server = server)

