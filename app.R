rm(list=ls())
library(shiny)
library(shinycssloaders)
source("tft_reroll_calcs.R")

# APP TO DO
#   - Baseline calculator: probability of hitting "all" or "any" for arbitrary # of units (we'll cap at 5 for now), 
#       with options to show plots of shops or gold  (just show all for now)
#   - Level or Roll: compares expected gold expenditure for condition whether you level or roll to determine better option
#   - Alter pool size and reroll probabilities 
# 

# Define UI for app that draws a histogram ----
ui <- navbarPage("TFT Reroll Calculator (10.13) by HARVEST GOLEM",
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
      h4("Units to Hit (Limit 5)"),
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
        withSpinner(plotOutput("plot"))
      )
  ),
  tabPanel("Level or Reroll?",
           h1("SOON (TM)")),
  tabPanel("Change Pool Size/Probabilities",
           h1("SOON (TM)"))
)

server <- function(input, output){
  # ====== Dynamic UI =======
  # Adapt # units choice range depending on other parameters 
  
  
  # Add unit rows 
  unit_rows <- c("unitrow_1")
  observeEvent(input$addUnitBtn, {
    # Cap at 5 units for now: matrix calculation gets too slow after that 
    if (length(unit_rows) < 5){
      row_num <- input$addUnitBtn + 1
      row_num_id <- paste0("unitrow_", row_num)
      insertUI(
        selector= paste0("#", unit_rows[length(unit_rows)]),
        where="afterEnd",
        ui= tags$div(id = row_num_id,
                     fluidRow( #TODO: Make numeric options dynamically adjust to other choices (to limit picking out of bounds)
                       column(3, selectInput(paste0("unitlvl_", row_num), "Unit Tier", choices=list("1"=1, "2"=2, "3"=3, "4"=4, "5"=5))),
                       column(3, numericInput(paste0("base_own_", row_num), "Copies You Own", 0, min=0, step=1)),
                       column(3, numericInput(paste0("others_own_", row_num), "Copies Others Own", 0, min=0, step=1)),
                       column(3, numericInput(paste0("copies_wanted_",row_num), "Copies Wanted", 1, min=1, step=1)))
        )
      )
      unit_rows <<- c(unit_rows, row_num_id)
    }

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
  # Include calls to action button clicks so the values update appropriately whenever rows added/deleted
  lookingFor <- reactive({
    ret_looking <- c()
    addbtn <- input$addUnitBtn
    rembtn <- input$removeUnitBtn
    for(row_ids in unit_rows){
      row_num <- sub("unitrow_", "", row_ids)
      wanted_id <- paste0("copies_wanted_", row_num)
      wanted_i <- input[[wanted_id]]
      validate(
        need(wanted_i, "Please finish filling out the scenario parameters.")
      )
      ret_looking <- c(ret_looking, wanted_i)
    }
    ret_looking
  })
  num_taken <- reactive({
    ret_num_taken <- c()
    addbtn <- input$addUnitBtn
    rembtn <- input$removeUnitBtn
    for(row_ids in unit_rows){
      row_num <- sub("unitrow_", "", row_ids)
      others_own_id <- paste0("others_own_", row_num)
      others_own_i <- input[[others_own_id]]
      validate(
        need(others_own_i, "Please finish filling out the scenario parameters.")
      )
      ret_num_taken <- c(ret_num_taken, others_own_i)
    }
    ret_num_taken
  })
  unit_lvls <- reactive({
    ret_unit_lvls <- c()
    addbtn <- input$addUnitBtn
    rembtn <- input$removeUnitBtn
    for(row_ids in unit_rows){
      row_num <- sub("unitrow_", "", row_ids)
      unit_lvl_id <- paste0("unitlvl_", row_num)
      unit_lvl_i <- as.numeric(input[[unit_lvl_id]])
      validate(
        need(unit_lvl_i, "Please finish filling out the scenario parameters.")
      )
      ret_unit_lvls <- c(ret_unit_lvls, unit_lvl_i)
    }
    ret_unit_lvls
  })
  initial_state <- reactive({
    ret_initial_state <- c()
    addbtn <- input$addUnitBtn
    rembtn <- input$removeUnitBtn
    for(row_ids in unit_rows){
      row_num <- sub("unitrow_", "", row_ids)
      initial_state_id <- paste0("base_own_", row_num)
      initial_state_i <- input[[initial_state_id]]
      validate(
        need(initial_state_i, "Please finish filling out the scenario parameters.")
      )
      ret_initial_state <- c(ret_initial_state, initial_state_i)
    }
    ret_initial_state
  })
  ordered_ret <- reactive({getOrderedPermutations(lookingFor(), input$condition)})
  ordered_perms <- reactive({ordered_ret()[[1]]})
  absorb_cutoff <- reactive({ordered_ret()[[2]]})
  num_taken_other <- reactive({
    other_ret <- c(input$other_out_1, input$other_out_2, input$other_out_3, input$other_out_4,
                   input$other_out_5)
    validate(
      need(!anyNA(other_ret), "Please finish filling out the scenario parameters.")
    )
    other_ret
   })
  error_check <- reactive({ # Error handling function (don't generate matrix until this is passed!)
    player_lvl <- as.numeric(input$player_lvl)
    validate_list <- validateScenario(player_lvl, num_taken_other(), unit_lvls(), num_taken(), lookingFor(), initial_state())
    validate_status <- validate_list[[1]]
    validate_msg <- validate_list[[2]]
    validate(
      need(validate_status, validate_msg)
    )
    })
  one_slot_transition_mat <- reactive({
    error_check()
    player_lvl <- as.numeric(input$player_lvl)
    createOneSlotMatrix(ordered_perms(), absorb_cutoff(), player_lvl, unit_lvls(), num_taken(), num_taken_other(), initial_state())
  })
  distribution_data <- reactive({
    error_check()
    generateDistributionData(one_slot_transition_mat(), absorb_cutoff())
  })
  # ========= Outputs =============
  output$plot <- renderPlot({
    distribution <- input$distribution
    if(distribution == "pdf"){
      plotPDF(distribution_data(), input$x_by)
    }
    else{
      plotCDF(distribution_data(), input$x_by)
    }
    
  })
}

options(shiny.reactlog=T)
shinyApp(ui, server)
