rm(list=ls())
setwd("D:/Code Projects/TFT stats/Reroll_Calculator")
library(shiny)
source("tft_reroll_calcs.R")

# APP TO DO
#   - Baseline calculator: probability of hitting "all" or "any" for arbitrary # of units (we'll cap at 5 for now), 
#       with options to show plots of shops or gold  (just show all for now)
#   - Level or Roll: compares expected gold expenditure for condition whether you level or roll to determine better option
#   - Alter pool size and reroll probabilities 
# 

# Define UI for app that draws a histogram ----
ui <- navbarPage("TFT Reroll Calculator (10.13)",
  tabPanel("Scenario Parameters",
      fluidRow(
        column(3, selectInput("condition", "Hit Condition", choices=list("Any"='any', "All"='all'))),
        column(3, selectInput("player_lvl", "Player Level", choices=list("1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                                                              "6"=6, "7"=7, "8"=8, "9"=9)))
      ),
      h4("# Other Units out of Tier Pools"),
      fluidRow(
        column(2, numericInput("other_out_1", "1", 0, min=0, step=1)),
        column(2, numericInput("other_out_2", "2", 0, min=0, step=1)),
        column(2, numericInput("other_out_3", "3", 0, min=0, step=1)),
        column(2, numericInput("other_out_4", "4", 0, min=0, step=1)),
        column(2, numericInput("other_out_5", "5", 0, min=0, step=1))
      ),
      h4("Units to Hit"),
      tags$div(id="unitrow_1",
      fluidRow( #TODO: Make numeric options dynamically adjust to other choices (to limit picking out of bounds)
        column(3, selectInput("unitlvl_1", "Unit Tier", choices=list("1"=1, "2"=2, "3"=3, "4"=4, "5"=5))),
        column(3, numericInput("base_own_1", "Copies You Own", 0, min=0, step=1)),
        column(3, numericInput("others_own_1", "Copies Others Own", 0, min=0, step=1)),
        column(3, numericInput("copies_wanted_1", "Total Copies Wanted", 1, min=1, step=1)))
      ),
      fluidRow(column(1, offset=9, actionButton('addUnitBtn', "Add Unit")),
               column(1, style='padding-left:20px', actionButton('removeUnitBtn', "Remove Unit")))
  ),
  tabPanel("Plot",
      sidebarPanel(
        h3("Plot Settings"),
        radioButtons("distribution", "Distribution", choices=list("PDF"="pdf", "CDF"="cdf")), 
        radioButtons("x_by", "X-Axis Value", choices=list("Shops", "Gold"), selected='Shops')
        ),
      mainPanel(
        plotOutput("plot")
      )
  ),
  tabPanel("Level or Reroll?",
           selectInput("placeholder", "placeholder", choices=list("1"=1))),
  tabPanel("Change Pool Size/Probabilities",
           selectInput("placeholder", "placeholder", choices=list("1"=1)))
)

server <- function(input, output){
  # ====== Dynamic UI =======
  unit_rows <- c("unitrow_1")
  observeEvent(input$addUnitBtn, {
    row_num <- input$addUnitBtn + 1
    row_num_id <- paste0("unitrow_", row_num)
    #print(paste0("#unitrow_", unit_rows[length(unit_rows)]))
    insertUI(
      selector= paste0("#", unit_rows[length(unit_rows)]),
      where="afterEnd",
      ui= tags$div(id = row_num_id,
        fluidRow( #TODO: Make numeric options dynamically adjust to other choices (to limit picking out of bounds)
        column(3, selectInput(paste0("unitlvl_", row_num), "Unit Tier", choices=list("1"=1, "2"=2, "3"=3, "4"=4, "5"=5))),
        column(3, numericInput(paste0("base_own_", row_num), "Copies You Own", 0, min=0, step=1)),
        column(3, numericInput(paste0("others_own_", row_num), "Copies Others Own", 0, min=0, step=1)),
        column(3, numericInput(paste0("copies_wanted_",row_num), "Total Copies Wanted", 1, min=1, step=1)))
      )
    )
    unit_rows <<- c(unit_rows, row_num_id)
  })
  
  observeEvent(input$removeUnitBtn, {
    # Never remove the first row 
    if(length(unit_rows) > 1){
      removeUI(
        selector = paste0("#", unit_rows[length(unit_rows)])
      )
      unit_rows <<- unit_rows[-length(unit_rows)]
    }
    
  })
  # ====== Dynamic base probabilities/pool size =======
  
  # ====== Parameters for Matrix Computation =======
  lookingFor <- reactive({c(input$copies_wanted_1)})
  ordered_ret <- reactive({getOrderedPermutations(lookingFor(), input$condition)})
  ordered_perms <- reactive({ordered_ret()[[1]]})
  absorb_cutoff <- reactive({ordered_ret()[[2]]})
  num_taken_other <- reactive({c(input$other_out_1, input$other_out_2, input$other_out_3, input$other_out_4,
                           input$other_out_5)
                              })
  num_taken <- reactive({c(input$others_own_1)})
  unit_lvls <- reactive({c(as.numeric(input$unitlvl_1))})
  initial_state <- reactive({paste0(c(input$base_own_1), collapse="")})
  
  one_slot_transition_mat <- reactive({
    player_lvl <- as.numeric(input$player_lvl)
    createOneSlotMatrix(ordered_perms(), absorb_cutoff(), player_lvl, unit_lvls(), num_taken(), num_taken_other())
  })
  distribution_data <- reactive({
    generateDistributionData(one_slot_transition_mat(), absorb_cutoff(), initial_state())
  })
  # ========= Outputs =============
  output$plot <- renderPlot({
    plotPDF(distribution_data(), input$x_by)
  })
}

options(shiny.reactlog=T)
shinyApp(ui, server)
