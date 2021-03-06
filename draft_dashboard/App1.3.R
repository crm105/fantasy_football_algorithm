#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

setwd("C:/Users/montg/Documents/active_projects/fantasy_football_algorithm")
library(shiny); library(DT); library(dplyr); library(Oarray)

df <- read.csv("C:/Users/montg/Documents/active_projects/fantasy_football_algorithm/projections.csv")
df <- select(df, name, everything(), -X, -id, -avg_type, -first_name, -last_name)
df <- dplyr :: arrange(df, (rank))
df$opportunity_cost <- 0
df$vorp <- 0
df <- df[df$rank < 175,]
drafted <- df[1,]; drafted$opportunity_cost <- 0 ; drafted$vorp <- 0; drafted <- drafted[-1,]
adp <- read.csv('adp.csv')

df <- df[df$name %in% adp[,'name'],]
load("C:/Users/montg/Documents/active_projects/fantasy_football_algorithm/player_distributions.RData")
load("C:/Users/montg/Documents/active_projects/fantasy_football_algorithm/mock_distributions.RData")

player_dist <- player_dist[names(player_dist) %in% df$name] 
adp <- adp[adp$name %in% df$name,] 
player_dist <- player_dist[names(player_dist) %in% df$name] 



# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Fantasy Draft Dashboard"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        numericInput("draft_pick", label = h3("Select Draft Pick"), value = 1, min = 1, max = 12),
        textOutput("pick_list"),
        textOutput("round_number"),
        textOutput("pick_number"),
        uiOutput("drafted")
      ),
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Main",   DT :: dataTableOutput("main_tab")),
                    tabPanel("Drafted Tab", DT :: dataTableOutput("drafted_tab"), 
                             uiOutput("undrafted"))
                  )

      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
df <- reactiveVal(df)
drafted <- reactiveVal(drafted)
counter <- reactiveValues(countervalue = 1, round = 1)


output$drafted <- renderUI({
  req(length(input$main_tab_cell_clicked) > 0)
  actionButton("drafted_button", "Drafted!")
})

output$undrafted <- renderUI({
  req(length(input$drafted_tab_cell_clicked) > 0)
  actionButton("undrafted_button", "Undrafted!")
})
# 
observeEvent(input$undrafted_button, {
  counter$countervalue <- counter$countervalue - 1
  q = df()
  took = drafted()
  if (!is.null(input$drafted_tab_rows_selected)) {
    q <- rbind (df(), took[as.numeric(input$drafted_tab_rows_selected),])
    q <- arrange(q, rank)

    print(tail((q)))

    took <- took[-as.numeric(input$drafted_tab_rows_selected),]

  }
  df(q)
  drafted(took)
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
                       

      observeEvent(draft_picks(),{
  counter$countervalue <- counter$countervalue - 1
})
#Step 1: Calculate player probabilities at each pick.
player_probabilities <- observeEvent(c(draft_picks(),  input$drafted_button),{
  counter$countervalue <- counter$countervalue + 1
  counter$round <- ceiling (counter$countervalue / 12 )
  proj <-  isolate(df())
  took = isolate (drafted())

  
  if (!is.null(input$main_tab_rows_selected)) {
    took <- rbind (took, proj[as.numeric(input$main_tab_rows_selected),])
    proj <- proj[-as.numeric(input$main_tab_rows_selected),]
    df(proj)
    
    
  }

probabilities <- list()
for (i in names(mock_dist)){
  pick_probs <- c()
  p <- isolate (draft_picks())
    
    # prob_expert <- integrate(approxfun (density (player_dist[[i]]), rule = 2, method = "constant"),subdivisions=2000, lower = 0, upper = p[counter$round + 1], abs.tol = .0002)
    # prob_adp <- pnorm(p[counter$round +1 ], mean = adp[adp$name == i, "AVG"], sd = adp[adp$name == i, "Std.Dev"]  )
    # print(i)
    prob_mock <- integrate(approxfun ( (mock_dist[[i]]), rule = 2, method = "constant"),subdivisions=4000, lower = 0, upper = p[counter$round + 1], abs.tol = .002)
    probabilities[[i]] <- prob_mock$value
    }
    



#probabilities[[i]] <-  (1/10 *prob_expert$value) + (2/10  * prob_adp) + (7/10 * prob_mock$value) }



#Step 2: Combine Player Probabilities with Projections


df_prob <- data.frame(matrix(ncol = 2, nrow = 1))
x <- c("name",paste("rd", as.character(counter$round, sep = "")))
colnames(df_prob) <- x

for (i in 1:length(probabilities)){
 
   df_prob <- rbind(df_prob, c(names(probabilities[i]), probabilities[[i]]))
  
}
df_prob <- na.omit(df_prob)
df_prob <- merge(proj[,c("points", "name", "pos")], df_prob)
df_prob[,2] <- sapply( df_prob[,2], as.numeric )
df_prob[,4] <- sapply( df_prob[,4], as.numeric )
df_prob$name <- as.character(df_prob$name)

drop_players <- c("CINCINNATI", "OAKLAND")
df_prob <- df_prob[!(df_prob$name %in% drop_players),]
df_prob <- arrange(df_prob, desc(points))



round <- counter$round  

agg_points <- list()


  expected_pos_points <- c()
  for (j in unique(df_prob$pos)) {
    
    prob_take <- c(0)
    prob_available <- c()
    player <- c()
    points <- c()
    
    for (q in 1:nrow(df_prob[df_prob$pos == j,])) {
      prob_available[q] <- (1 - as.numeric(df_prob[ df_prob$pos == j, 4][q]))
      prob_take[q] <- prob_available[q] * (1 - sum (prob_take)) 
      player[q] <- df_prob[df_prob$pos == j, "name"][q - 1]
      points[q] <- prob_take[q] * df_prob[df_prob$pos == j, 'points'][q]
      
      
    } 
    
    expected_pos_points[j] <- sum(points)
  }
  vorp_frame <-  data.frame (cbind (c("QB", "RB", "WR", "TE", "DST"),( unname (expected_pos_points))))
  colnames(vorp_frame) <- c("pos", "opportunity_cost")
  vorp_frame$opportunity_cost <- as.numeric(as.character( vorp_frame$opportunity_cost))


  proj <- merge(proj[, !(colnames(proj) %in% c("vorp", "opportunity_cost"))], vorp_frame)
  proj$vorp <- proj$points - proj$opportunity_cost
  proj <- arrange(proj, desc(vorp))
  

  drafted(took)
  df(proj)
  
  
  


})





output$pick_list <- renderText({draft_picks()})
output$round_number <- renderText({counter$round})
output$pick_number <- renderText({counter$countervalue})


}

# Run the application 
shinyApp(ui = ui, server = server)

