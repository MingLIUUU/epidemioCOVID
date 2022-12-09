
if (interactive()) {
  library(shiny)
  library(ggplot2)
}
# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel(tags$h1(tags$b("epidemioCOVID:"),
                     "Sequences Alignment Visualization")),



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
      tags$p("select the range of start and end position detailed site visualization"),

      # Input: Specification of range within an interval ----
      sliderInput("range", "Range(%):",
                min = 1, max = 100,
                value = c(1,10)),

      actionButton("start", "Start")
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Selected site Plot ----
      plotOutput("plotWhole"),
      plotOutput("plotSite")

    )
  )
)

# Define server logic required to draw two plots ----
server <- function(input, output) {

  v <- reactiveValues(dothing = FALSE)

  observeEvent(input$start, {
    v$dothing <- input$start
  })

  observeEvent(is.null(input$file1$datapath), {
    v$dothing <- FALSE
  })

  output$plotWhole <- renderPlot({
    if (v$dothing == FALSE) return(NULL)
    else{
      seqs <- epidemioCOVID::readFASTA(input$file1$datapath)
      msaSet <- epidemioCOVID::preAlign(seqs, refseq = fakeref)
      msa <- epidemioCOVID::alignMSA(msaSet)
      epidemioCOVID::msaPlot(msa)
    }
  })

  output$plotSite <- renderPlot({
    if (v$dothing == FALSE) return(NULL)
    else {
      seqs <- epidemioCOVID::readFASTA(input$file1$datapath)
      len <- min(nchar(seqs$seq)) * 0.01
      start = round(len * input$range[1], digits = 0)
      end = round(len * input$range[2], digits = 0)
      epidemioCOVID::siteVisual(input$file1$datapath,
                                 start = start,
                                 end = end)
    }
  })



}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
