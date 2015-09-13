#' @export
ggexclude <- function(df, plotExpr) {

  ui <- dialogPage(
    plotOutput("plot", height = "100%",
      brush = brushOpts("brush", resetOnNew = TRUE)
    ),
    statusbar = actionButton("undo", "Undo", class = "btn-xs"),
    buttons = list(
      actionButton("reset", "Reset", class = "btn-xs"),
      actionButton("done", "Done", class = "btn-primary btn-xs")
    )
  )

  server <- function(input, output, session) {
    v <- reactiveValues(
      undoStack = list(),
      excluded = rep(FALSE, nrow(df))
    )

    filtered <- reactive({
      df[!v$excluded,]
    })

    observeEvent(input$brush, {
      brushed <- brushedPoints(df, input$brush, allRows = TRUE)
      rownums <- which(brushed$selected_ & !v$excluded)
      v$undoStack <- c(v$undoStack, list(rownums))
      v$excluded <- brushed$selected_ | v$excluded
    })

    observeEvent(input$undo, {
      if (length(v$undoStack) > 0) {
        popped <- tail(v$undoStack, 1)
        v$undoStack <- head(v$undoStack, -1)

        v$excluded[popped[[1]]] <- FALSE
      }
    })

    observeEvent(input$reset, {
      v$undoStack <- list()
      v$excluded <- FALSE
    })

    observeEvent(input$done, {
      stopApp(filtered())
    })

    output$plot <- renderPlot({
      plotExpr %+% filtered()
    })
  }

  runApp(shinyApp(ui, server,
    options = list(launch.browser = getOption("viewer", TRUE))
  ))
}

#' @examples
#' ggexclude(cars, ggplot(, aes(speed, dist)) + geom_point())
#'
