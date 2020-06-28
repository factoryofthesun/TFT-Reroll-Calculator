library(shiny)
library(ggplot2)

num<-c(1,2,3,4,5)
let<-c("A","B","C","D","E")
date<-c("2015-5-1","2015-6-1","2015-7-1","2015-8-1","2015-9-1")
df <- data.frame(num,let,date)

ui <- fluidPage(
  titlePanel(title=h4("Races", align="center")),
  sidebarPanel( 
    sliderInput("num", "Number:",min = 0, max = 5,step=1,value=c(1,2))),
  mainPanel(plotOutput("plot2")))

server <- function(input,output){
  
  dat <- reactive({
    test <- df[df$num %in% seq(from=min(input$num),to=max(input$num),by=1),]
    print(test)
    test
  })
  
  output$plot2<-renderPlot({
    ggplot(dat(),aes(x=date,y=num))+geom_point(colour='red')},height = 400,width = 600)}
shinyApp(ui, server)