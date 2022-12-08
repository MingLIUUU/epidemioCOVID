library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel(tags$h1(tags$b("epidemioCOVID:"),"alinment sequence site Visualization")),



  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

    tags$p("To visualize the selected site of your alignmnet, upload the FASTA
         format multiple sequence file. Enter the start anf the end position of
         the site you want to visulize."),
      # Input: Select a file ----
      fileInput("file1", "Choose FASTA File",
                multiple = TRUE,
                accept = ".fasta"),
      tags$p("Enter the start and end position)"),
      textAreaInput("sta", "start position"),
      textAreaInput("end", "end position" ),
      actionButton("start", "Start"),


    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Selected site Plot ----
      plotOutput(outputId = "Plot")

    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

  v <- reactiveValues(dothing = FALSE)

  observeEvent(input$start, {
    v$dothing <- input$start
  })

  output$plot <- renderPlot({
    if (v$dothing == FALSE) return()
    else{
      epidemioCOVID::siteVisual(input$file1$datapath,
                                 input$sta,
                                 input$end)
    }
  })


}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
