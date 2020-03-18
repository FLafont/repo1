library(shiny)
library(ggplot2)

ui <- fluidPage(
  title = 'Algues Vertes',
  sidebarLayout(
    sidebarPanel(
      
      selectInput('bvav','Choix du BVAV',
                  choices = c(names(df_bv),'Tous'),
                  selected = 'Tous'),
      
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
  
  ### Choix du data.frame selon le bassin versant
  df_used <- reactive({
    if (input$bvav %in% names(df_bv)){df_used <- df_bv[[input$bvav]]}
    else if (input$bvav =='Tous'){df_used <- df_sphinx}
  })
  
  table_reac <- reactive({
    
    
    ### Filtre sans créer les data frames en avance
    ### selon la part de la surface agricole dans le bassin versant
    if ((input$variable == 'connaissance_service_plav')&(input$type_exp=='Toutes')){data <- data.frame(table(df_used()[,input$variable]))}
    else if ((input$variable=='connaissance_service_plav')&(input$type_exp %in% df_used()$type_exp_bv)){
      data <- data.frame(table(filter(df_used(),type_exp_bv == input$type_exp)[,input$variable]))}
    else {data <- data.frame(table(df_used()[,input$variable]))}
    # data <- data.frame(table(df_used()[,input$variable]))
    colnames(data) <- c(input$variable,'Fréquence')
    data
  })
  plot <- reactive({
    if ((input$variable == 'connaissance_service_plav')&(input$type_exp=='Toutes')){data <- data.frame(table(df_used()[,input$variable]))}
    else if ((input$variable=='connaissance_service_plav')&(input$type_exp %in% df_used()$type_exp_bv)){
      data <- data.frame(table(filter(df_used(),type_exp_bv == input$type_exp)[,input$variable]))}
    else {data <- data.frame(table(df_used()[,input$variable]))}
    # data <- data.frame(table(df_used()[,input$variable]))
    colnames(data) <- c(input$variable,'Fréquence')
    ggplot(data,aes(x=!!as.name(input$variable), y = `Fréquence`)) +
      geom_col() +
      theme_minimal()
  })
  output$table <- renderTable({table_reac()})
  output$plot <- renderPlot({plot()})
}


shinyApp(ui, server)


