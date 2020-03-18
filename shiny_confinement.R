library(shiny)

ui <- fluidPage(
  title = 'Algues Vertes',
  sidebarLayout(
    sidebarPanel(
      selectInput('variable','Variable choisie',
                  choices = colnames(df_sphinx[2:101]),
                  selected =colnames(df_sphinx[2])),
      
      conditionalPanel(
        condition = "input.variable == 'connaissance_service_plav'",
        selectInput('type_exp',"Degré d'inclusion dans BVAV",
                    choices= c(unique(df_sphinx$type_exp_bv),'Toutes'),
                    selected ='Toutes')
      )
      
    ),
    mainPanel(
      tableOutput("table"),
      plotOutput("plot"))
  )
)

server <- function(input, output, session) {
  table_reac <- reactive({
    if ((input$variable == 'connaissance_service_plav')&(input$type_exp=='Toutes')){df_sphinx$connaissance_service_plav}
    else if ((input$variable=='')
             data <- data.frame(table(df_sphinx[,input$variable]))
             colnames(data) <- c(input$variable,'Fréquence')
             data
  })
    plot <- reactive({
      data <- data.frame(table(df_sphinx[,input$variable]))
      colnames(data) <- c(input$variable,'Fréquence')
      ggplot(data,aes(x=!!as.name(input$variable), y = `Fréquence`)) +
        geom_col() +
        theme_minimal()
    })
    output$table <- renderTable({table_reac()})
    output$plot <- renderPlot({plot()})
}


shinyApp(ui, server)

