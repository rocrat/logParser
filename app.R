#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Log File Parser (v0.0.2)"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        fileInput("logfile", "Upload Log File", accept = ".log",
                  multiple = FALSE),
        checkboxInput("allMoves", "Inlcude all Z Movements?", value = FALSE),
        downloadButton("getRes", "Download Results")
      ),

      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel("Table",
                   DT::dataTableOutput("datTable")
          ),
          tabPanel("Plots",
                   uiOutput('timePlotSized')
          )
        )
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  dat <- reactiveValues(data = NULL)

  observeEvent(input$logfile,{
    dat$data <- getZ(input$logfile$datapath,
                       include.all.moves = input$allMoves)
  })

  output$datTable <- DT::renderDataTable(dat$data)
  output$getRes <- downloadHandler(
    filename = function(){
      paste0(gsub("\\.log$", "", input$logfile$name), "_Z_Values.csv")
    },
    content = function(file){

      write.csv(dat$data, file = file, row.names = FALSE)
    }
  )

  output$timeplot <- renderPlotly({
    req(dat$data)
    p <- dat$data %>%
      filter(types == "Absolute") %>%
      tidyr::pivot_longer(c(movetime, imagetime), names_to = "Event_Type",
                          names_pattern = "(move|image)time", values_to = "Time") %>%
      mutate(stackImage = paste0(stack, ":", image)) %>%
      ggplot(aes(x = Time, y = distances, text = stackImage)) +
      geom_point(aes(color = Event_Type)) +
      geom_line(aes(group = chip)) +
      facet_wrap(~chip, ncol = 1) +
      ylab("Distance (Absolute)") +
      theme_bw()
    plotly::ggplotly(p)
  })

  output$timePlotSized <- renderUI({
    height = paste0(length(unique(dat$data$chip))*300, "px")
    plotlyOutput('timeplot', width = "100%", height = height)
  })
}

getZ <- function(file, include.all.moves = FALSE) {
  lines <- readLines(con = file)
  movetimes <- c()
  speeds <- c()
  currents <- c()
  distances <- c()
  types <- c()
  imagetimes <- c()
  chip <- c()
  stack <- c()
  image <- c()

  for(i in 1:length(lines)){
    if(grepl("Z Stage Move", lines[i])) {
      movetimes <- c(movetimes, gsub("(\\d{4}\\-\\d{2}\\-\\d{2} \\d{2}:\\d{2}:\\d{2}\\.\\d{4}).*", "\\1", lines[i]))
      speeds <- c(speeds, as.numeric(gsub(".*speed: (\\d+\\.\\d+),.*", "\\1", lines[i])))
      currents <- c(currents, as.numeric(gsub(".*current: (\\d+\\.*\\d*).*", "\\1", lines[i])))
      distances <- c(distances, as.numeric(gsub(".*distance: (\\-*\\d+\\.*\\d*).*", "\\1", lines[i])))
      types <- c(types, gsub(".*type ([A-Za-z]+).*", "\\1", lines[i]))
    }
    if(grepl("Chip \\d+ \\- stack\\d+:", lines[i])){
      if(grepl("added (Red|Amber) image \\d+", lines[i]) &
         as.numeric(gsub(".*image (\\d+)", "\\1", lines[i])) > 1) next;
      if(grepl("Saving \\d+ raw images", lines[i])) next;
      if(length(speeds) - 1 < length(chip)) {
        # Repeat previous z-stage location since did not move from previous image
        numImages <- length(chip) - length(speeds) + 1
        movetimes <- c(movetimes, rep(tail(movetimes, 1), numImages))
        speeds <- c(speeds, rep(tail(speeds, 1), numImages))
        currents <- c(currents, rep(tail(currents, 1), numImages))
        distances <- c(distances, rep(tail(distances, 1), numImages))
        types <- c(types, rep(tail(types, 1), numImages))
      }
      if(length(speeds) - 1 > length(chip)) {
        lengthDiff <- length(speeds) - length(chip) - 1
        imagetimes <- c(imagetimes, rep(NA, lengthDiff))
        chip <- c(chip, rep(NA, lengthDiff))
        stack <- c(stack, rep(NA, lengthDiff))
        image <- c(image, rep(NA, lengthDiff))
      }
      imagetimes <- c(imagetimes, gsub("(\\d{4}\\-\\d{2}\\-\\d{2} \\d{2}:\\d{2}:\\d{2}\\.\\d{4}).*", "\\1", lines[i]))
      chip <- c(chip, gsub(".*(Chip \\d+).*", "\\1", lines[i]))
      stack <- c(stack, gsub(".*\\- (stack\\d+).*", "\\1", lines[i]))
      image <- c(image, gsub(".*added ([A-Za-z]+ image \\d+)", "\\1", lines[i]))
    }
  }
  # Fix lengths if they don't match----
  if(length(speeds) > length(chip)){
    lengthDiff <- length(speeds) - length(chip)
    imagetimes <- c(imagetimes, rep(NA, lengthDiff))
    chip <- c(chip, rep(NA, lengthDiff))
    stack <- c(stack, rep(NA, lengthDiff))
    image <- c(image, rep(NA, lengthDiff))
  }

  if(length(chip) > length(speeds)){
    lengthDiff <- length(chip) - length(speeds)
    movetimes <- c(movetimes, rep(NA, lengthDiff))
    speeds <- c(speeds, rep(NA, lengthDiff))
    currents <- c(currents, rep(NA, lengthDiff))
    distances <- c(distances, rep(NA, lengthDiff))
    types <- c(types, rep(NA, lengthDiff))
  }

  resdf <- data.frame(MoveID = 1:length(speeds),
                      movetime = suppressWarnings(as.POSIXct(movetimes)),
                      speeds  = speeds,
                      currents = currents,
                      distances = distances,
                      types = types,
                      imagetime = suppressWarnings(as.POSIXct(imagetimes)),
                      chip = chip,
                      stack = stack,
                      image = image)
  if(!include.all.moves){
    resdf <- resdf[!is.na(resdf$chip), ]
  }
  return(resdf)
}




# Run the application
shinyApp(ui = ui, server = server)
