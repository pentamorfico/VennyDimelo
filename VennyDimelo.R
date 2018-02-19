library(shiny)
library(VennDiagram)

ui <-
  navbarPage(
    HTML(paste0("<a href=", shQuote("https://github.com/pentamorfico/VennyDimelo"), ">", "Link to Github", "</a>")),
    windowTitle = "VennyDimelo",
    inverse = TRUE,
    tabPanel(
      "VennyDimelo",
  fluidPage(
    fluidRow(
      column(
        4,
        wellPanel(
          p("String together combinations by joining them
            with an ampersand (&)."),
          splitLayout(
            cellWidths = c("50%","50%"),
            textInput(inputId='title1', label='Name of Sets', value="Set 1"),
            textInput(inputId='set1', label='Inputs', value="p53")
          ),
          splitLayout(
            cellWidths = c("50%","50%"),
            textInput(inputId='title2', label=NULL, value="Set 1"),
            textInput(inputId='set2', label=NULL, value="Cas9")),
          splitLayout(
            textInput(inputId='color1', label='Color of Set 1', width='50%', value="cadetblue"),
            textInput(inputId='color2', label='Color of Set 2', width='50%', value="brown1")
          )
          )
    ),
    column(
      5,
      plotOutput("venn", height = "600px")
    ),
    column(
      2,
      sliderInput("opacity1", "Opacity of Set 1", min = 0, max = 1, value = 0.4,
                  width = "100%"),
      sliderInput("opacity2", "Opacity of Set 2", min = 0, max = 1, value = 0.4,
                  width = "100%"),
      numericInput(
        inputId = "width",
        label = "Width (inches)",
        value = 6,
        width = "100%"
      ),
     numericInput(
       inputId = "height",
       label = "Height (inches)",
        value = 4,
        width = "100%"
      ),
    downloadButton("download_plot", "Save plot"),
    radioButtons(
      "savetype",
      NULL,
      list("pdf", "png"),
      inline = TRUE
    ))
      )
    )
    )
  )

server <- function(input, output)
  {
  set1 <- reactive(
    (strsplit((input$set1), split=" ",fixed=TRUE)[[1]])
  )
  set2 <- reactive(
    (strsplit((input$set2), split=" ",fixed=TRUE)[[1]])
  )

  title1 <- "Set 1"
  title2 <- "Set 2"
  output$venn <- renderPlot({
    VennDiagram::draw.pairwise.venn(
      area1=length(set1()),
      area2=length(set2()),
      cross.area=length(intersect(set1(),set2())),
      category=c(input$title1,input$title2),
      cat.cez=1.25, cat.pos=c(-30,20), cat.dist=0.04, cex=2,
      fill=c(input$color1,input$color2),
      alpha=c(input$opacity1,input$opacity2),
      lwd=3,fontface="bold",
      cat.fontface="bold")
  })

  # Download the plot
  output$download_plot <- downloadHandler(
    filename = function(){
      paste0("venn-", Sys.Date(), ".", input$savetype)
    },
    content = function(file) {
      switch(input$savetype,
             pdf = pdf(file,
                       width = input$width,
                       height = input$height),
             png = png(file, type = "cairo",
                       width = input$width,
                       height = input$height,
                       units = "in",
                       res = 300))
      print(euler_plot())
      dev.off()
    }
  )

}

shinyApp(ui=ui, server=server)


