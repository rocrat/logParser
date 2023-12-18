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
    titlePanel("Log File Parser (v0.0.4)"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        fileInput("logfile", "Upload Log File", accept = ".log",
                  multiple = FALSE),
        uiOutput("startsTable"),
        checkboxInput("allMoves", "Inlcude all Z Movements?", value = FALSE),
        downloadButton("getRes", "Download Results"),
        width = 2
      ),

      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel("Table",
                   DT::dataTableOutput("datTable")
          ),
          tabPanel("Z-Movement Plots",
                   uiOutput('timePlotSized')
          ),
          tabPanel("Focus Value Plots",
                   fluidRow(
                     column(1,
                            uiOutput("stackSelector")
                     ),
                     column(11,
                            tabsetPanel(
                              tabPanel("Cold Images",
                                fluidRow(
                                  column(12,
                                         plotlyOutput("focusPlot",
                                                      height = "900px")
                                         )
                                )
                              ),
                              tabPanel("Hot Images",
                                       fluidRow(
                                         column(12,
                                                plotlyOutput("focusPlotHot",
                                                             height = "900px")
                                         )
                                       )
                              )
                            )

                     )
                   ),
          )
        ),
        width = 10
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  dat <- reactiveValues(data = NULL,
                        focus = NULL,
                        starts = NULL)

  observeEvent(input$logfile, {
    dat$starts <- getStarts(input$logfile$datapath)
  })

  observe({
    shiny::validate(
      need(!is.null(dat$starts), ""),
      need(input$startList, "")
    )
    lines <- readLines(input$logfile$datapath)

    subdata <- dat$starts
    subdata$include <- dat$starts$starts %in% input$startList
    uselines <- c()
    for(i in 1:nrow(subdata)){
      if(subdata$include[i]){
        if(i < nrow(subdata)){
          uselines <- c(uselines, subdata$lineNums[i]:(subdata$lineNums[i+1]-1))
        } else {
          uselines <- c(uselines, subdata$lineNums[i]:length(lines))
        }
      }
    }
    sublines <- lines[uselines]

    zres <- getZ(sublines,
                 include.all.moves = input$allMoves)
    dat$data <- zres$resdf
    dat$focus <- zres$focusdf

  })

  output$stackSelector <- renderUI({
    radioButtons("selectedStack", "Select a SubArray Index", choices = unique(dat$focus$stack))
  })

  output$datTable <- DT::renderDataTable({
    DT::datatable(dat$data, options = list(paging = FALSE, scrollY = "400px"))
  })

  output$startsTable <- renderUI({
    shiny::validate(
      need(input$logfile$datapath, "")
    )
    checkboxGroupInput("startList", "Select Run Start Time(s)",
                       choices = dat$starts$starts,
                       selected = dat$starts$starts)
  })

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
    if(input$allMoves){
      p <- dat$data %>%
        filter(is.na(types) | types == "Absolute") %>%
        tidyr::pivot_longer(c(movetime, imagetime), names_to = "Event_Type",
                            names_pattern = "(move|image)time", values_to = "Time") %>%
        mutate(stackImage = paste0(row, ":", col, ":", image)) %>%
        ggplot(aes(x = Time, y = distances, text = stackImage)) +
        geom_point(aes(color = Event_Type)) +
        geom_line(aes(group = chip)) +
        facet_wrap(~chip, ncol = 1) +
        ylab("Distance (Absolute)") +
        theme_bw()
    } else {
      p <- dat$data %>%
        tidyr::pivot_longer(c(movetime, imagetime), names_to = "Event_Type",
                            names_pattern = "(move|image)time", values_to = "Time") %>%
        mutate(stackImage = paste0(row, ":", col, ":", image)) %>%
        ggplot(aes(x = Time, y = distances, text = stackImage)) +
        geom_point(aes(color = Event_Type)) +
        geom_line(aes(group = chip)) +
        facet_wrap(~chip, ncol = 1) +
        ylab("Distance (Absolute)") +
        theme_bw()
    }

    plotly::ggplotly(p)
  })

  output$timePlotSized <- renderUI({
    height = paste0(length(unique(dat$data$chip))*300, "px")
    plotlyOutput('timeplot', width = "100%", height = height)
  })

  output$focusPlot <- renderPlotly({
    shiny::validate({
      need(input$selectedStack, "")
    })
    plotdf <- dat$focus %>%
      filter(stack == input$selectedStack,
             imageType != "HotBlue") %>%
      mutate(timeSide = paste0(focusTime, ";", side))

    plotdf %>%
      ggplot(aes(x = zvals, y = fvals, group = timeSide)) +
      geom_point(aes(color = timeSide)) +
      geom_line() +
      facet_grid(imageType ~ chip) +
      guides(color = "none") +
      geom_vline(data = plotdf %>%
                   select(imageType, chip, focusZ, focusTime, timeSide) %>%
                   distinct(),
                 aes(xintercept = focusZ, color = timeSide)) +
      ylab("Focus Value") +
      xlab("Z Position")
  })

  output$focusPlotHot <- renderPlotly({
    shiny::validate({
      need(input$selectedStack, "")
    })
    plotdf <- dat$focus %>%
      filter(stack == input$selectedStack,
             imageType == "HotBlue") %>%
      mutate(timeSide = paste0(focusTime, ";", side))

    plotdf %>%
      ggplot(aes(x = zvals, y = fvals, group = timeSide)) +
      geom_point(aes(color = focusTime)) +
      geom_line() +
      facet_wrap(~ focusTime) +
      guides(color = "none") +
      geom_vline(data = plotdf %>%
                   select(imageType, chip, focusZ, focusTime, timeSide) %>%
                   distinct(),
                 aes(xintercept = focusZ, color = timeSide)) +
      ylab("Focus Value") +
      xlab("Z Position")
  })
}

getZ <- function(lines, include.all.moves = FALSE) {

  # lines <- readLines(con = file)
  movetimes <- c()
  speeds <- c()
  currents <- c()
  distances <- c()
  types <- c()
  imagetimes <- c()
  chip <- c()
  Row <- c()
  Col <- c()
  stack <- c()
  image <- c()
  focusList <- list()

  for(i in 1:length(lines)){
    if(grepl("Z Stage Move", lines[i])) {
      movetimes <- c(movetimes, gsub("(\\d{4}\\-\\d{2}\\-\\d{2} \\d{2}:\\d{2}:\\d{2}\\.\\d{4}).*", "\\1", lines[i]))
      speeds <- c(speeds, as.numeric(gsub(".*speed: (\\d+\\.\\d+),.*", "\\1", lines[i])))
      currents <- c(currents, as.numeric(gsub(".*current: (\\d+\\.*\\d*).*", "\\1", lines[i])))
      distances <- c(distances, as.numeric(gsub(".*distance: (\\-*\\d+\\.*\\d*).*", "\\1", lines[i])))
      types <- c(types, gsub(".*type ([A-Za-z]+).*", "\\1", lines[i]))
      imagetimes <- c(imagetimes, NA)
      chip <- c(chip, NA)
      Row <- c(Row, NA)
      Col <- c(Col, NA)
      image <- c(image, NA)
    }

    if(grepl("Focus values", lines[i])) {
      if(grepl("Peak Found", lines[i]) & grepl("Peak Found", lines[i + 1])){
        # Grab the two lines of focus values and then move foward until a chip is found and record the chip and stack
        vals1 <- stringr::str_split(gsub(".*values\\s(.+)\\sPeak\\:.*", "\\1", lines[i]),
                                    pattern = ",", simplify = TRUE)[1:5]
        zvals1 <- as.numeric(gsub("(\\d+\\.\\d+)\\s\\:.*", "\\1", vals1))
        fvals1 <- as.numeric(gsub(".*\\:\\s(\\d+\\.\\d+)", "\\1", vals1))
        peak1 <- as.numeric(gsub(".*Peak\\:(\\d+\\.\\d+).*", "\\1", lines[i]))

        vals2 <- stringr::str_split(gsub(".*values\\s(.+)\\sPeak\\:.*", "\\1", lines[i+1]),
                                    pattern = ",", simplify = TRUE)[1:5]
        zvals2 <- as.numeric(gsub("(\\d+\\.\\d+)\\s\\:.*", "\\1", vals2))
        fvals2 <- as.numeric(gsub(".*\\:\\s(\\d+\\.\\d+)", "\\1", vals2))
        peak2 <- as.numeric(gsub(".*Peak\\:(\\d+\\.\\d+).*", "\\1", lines[i+1]))

        chipnum <- NULL
        j <- i + 1
        while(is.null(chipnum)){
          j <- j + 1
          if(grepl("Focus found", lines[j])) {
            chipnum <- gsub(".*Chip\\:\\s(\\d).*", "\\1", lines[j])
            imageType <- gsub(".*Plane\\:\\s([A-Za-z]+).*", "\\1", lines[j])
            stack <- gsub(".*Index\\:\\s(\\d+).*", "\\1", lines[j])
            focusVal <- as.numeric(gsub(".*Focus Z\\:\\s(\\d+\\.\\d+).*", "\\1", lines[j]))
            focusTime <- gsub("(\\d{4}\\-\\d{2}\\-\\d{2} \\d{2}:\\d{2}:\\d{2}\\.\\d{4}).*", "\\1", lines[j])
          }
        }

        #Put everything together
        focusdf <- data.frame(zvals = c(zvals1, zvals2),
                              fvals = c(fvals1, fvals2),
                              side = rep(c("Left", "Right"), each = length(zvals1)),
                              peak = rep(c(peak1, peak2), each = length(zvals1)),
                              chip = chipnum,
                              imageType = imageType,
                              stack = stack,
                              focusZ = focusVal,
                              focusTime = focusTime)

        focusList[[length(focusList) + 1]] <- focusdf

      } else {
        next;
      }
    }

    if(grepl("Chip \\d, Row \\d, Column", lines[i])) {
      movetimes <- c(movetimes, NA)
      speeds <- c(speeds, NA)
      currents <- c(currents, NA)
      distances <- c(distances, as.numeric(gsub(".*Focus (\\d+\\.\\d+),.*", "\\1", lines[i])))
      types <- c(types, NA)
      imagetimes <- c(imagetimes, gsub("(\\d{4}\\-\\d{2}\\-\\d{2} \\d{2}:\\d{2}:\\d{2}\\.\\d{4}).*", "\\1", lines[i]))
      chip <- c(chip, gsub(".*(Chip \\d+).*", "\\1", lines[i]))
      Row <- c(Row, gsub(".*(Row \\d+).*", "\\1", lines[i]))
      Col <- c(Col, gsub(".*(Column \\d+).*", "\\1", lines[i]))
      image <- c(image, paste0(gsub(".*DEBUG\\|([A-Za-z\\-]+):.*", "\\1", lines[i]),
                               ":",
                               gsub(".*Focus \\d+\\.\\d+, ([A-Za-z]+)\\s+$", "\\1", lines[i])))
    }

  # Fix lengths if they don't match----
  # if(length(speeds) > length(chip)){
  #   lengthDiff <- length(speeds) - length(chip)
  #   imagetimes <- c(imagetimes, rep(NA, lengthDiff))
  #   chip <- c(chip, rep(NA, lengthDiff))
  #   stack <- c(stack, rep(NA, lengthDiff))
  #   image <- c(image, rep(NA, lengthDiff))
  # }
  #
  # if(length(chip) > length(speeds)){
  #   lengthDiff <- length(chip) - length(speeds)
  #   movetimes <- c(movetimes, rep(NA, lengthDiff))
  #   speeds <- c(speeds, rep(NA, lengthDiff))
  #   currents <- c(currents, rep(NA, lengthDiff))
  #   distances <- c(distances, rep(NA, lengthDiff))
  #   types <- c(types, rep(NA, lengthDiff))
  }

  resdf <- data.frame(MoveID = 1:length(speeds),
                      movetime = suppressWarnings(as.POSIXct(movetimes)),
                      speeds  = speeds,
                      currents = currents,
                      distances = distances,
                      types = types,
                      imagetime = suppressWarnings(as.POSIXct(imagetimes)),
                      chip = chip,
                      row = Row,
                      col = Col,
                      image = image)
  if(!include.all.moves){
    resdf <- resdf[!is.na(resdf$chip), ]
  }
  return(list("resdf" = resdf,
              "focusdf" = dplyr::bind_rows(focusList)))
}

getStarts <- function(file) {
  lines <- readLines(con = file)
  starts <- c()
  i <- 0
  while(length(starts) == 0){
    i <- i + 1
    if(grepl("(\\d{4}\\-\\d{2}\\-\\d{2} \\d{2}:\\d{2}:\\d{2}\\.\\d{4})", lines[i])){
      starts <- c(gsub("(\\d{4}\\-\\d{2}\\-\\d{2} \\d{2}:\\d{2}:\\d{2}\\.\\d{4}).*", "\\1", lines[i]))
      lineNums <- c(i)
    }
  }


  for(i in 1:length(lines)) {
    if(grepl("Collection", lines[i])){
      starts <- c(starts, gsub("(\\d{4}\\-\\d{2}\\-\\d{2} \\d{2}:\\d{2}:\\d{2}\\.\\d{4}).*", "\\1", lines[i+3]))
      lineNums <- c(lineNums, i)
    } else {
      next;
    }
  }
  # pstarts <- as.POSIXct(starts)
  # dates <- as.Date.POSIXct(pstarts)
  # times <- format(pstarts, format = "%H:%M:%S")
  data.frame(starts = starts,
             lineNums = lineNums)


}


# Run the application
shinyApp(ui = ui, server = server)
