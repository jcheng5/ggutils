#' @import ggplot2 shiny
NULL

#' @export
gghover <- function(plotExpr) {
  # See below for definition of dialogPage function
  ui <- dialogPage(
    plotOutput("plot", hover = "hover", height = "100%")
  )

  server <- function(input, output, session) {
    # Show the plot... that's important.
    output$plot <- renderPlot(plotExpr)

    # The part of the data frame that is currently brushed (or
    # NULL if no brush is active)
    near <- reactive({
      if (is.null(input$hover))
        return(NULL)
      else
        nearPoints(plotExpr$data, input$hover, maxpoints = 1)
    })

    # Show a message giving instructions, or showing how many
    # rows are selected
    output$msg <- renderText({
      if (is.null(near()) || nrow(near()) == 0)
        return("")

      paste0(names(cars), ": ", unlist(near()), collapse = ", ")
    })

    # When the Done button is clicked, return the brushed
    # rows to the caller.
    observeEvent(input$done, {
      stopApp()
    })
  }

  runApp(shinyApp(ui, server), launch.browser = getOption("viewer", TRUE))
}

#' Brush ggplot2
#'
#' Call ggbrush with a ggplot2 object, and the dimensions which
#' should be brushed (try "xy" for scatter, "x" for histogram).
#' The plot will show in RStudio Viewer or your web browser, and
#' any observations selected by the user will be returned.
#' @export
ggbrush <- function(plotExpr, direction = c("xy", "x", "y")) {

  # See below for definition of dialogPage function
  ui <- dialogPage(
    plotOutput("plot", brush = brushOpts(id = "brush", direction = direction),
      width = "100%", height = "100%" # Fill the dialog
    )
  )

  server <- function(input, output, session) {
    # Show the plot... that's important.
    output$plot <- renderPlot(plotExpr)

    # The part of the data frame that is currently brushed (or
    # NULL if no brush is active)
    brushed <- reactive({
      if (is.null(input$brush))
        return(NULL)
      else
        brushedPoints(plotExpr$data, input$brush)
    })

    # Show a message giving instructions, or showing how many
    # rows are selected
    output$msg <- renderText({
      if (is.null(brushed()))
        return("Click and drag to select, then press Done \u27f6")

      count <- nrow(brushed())
      sprintf("%d %s selected",
        count,
        ifelse(count == 1, "observation", "observations")
      )
    })

    # When the Done button is clicked, return the brushed
    # rows to the caller.
    observeEvent(input$done, {
      stopApp(brushed())
    })
  }

  runApp(shinyApp(ui, server), launch.browser = getOption("viewer", TRUE))
}

# Helper function to present Shiny controls in a dialog-like layout
dialogPage <- function(outputControl, statusbarHeight = 40,
  statusbar = textOutput("msg", inline = TRUE),
  buttons = list(
    actionButton("done", "Done", class = "btn btn-primary btn-xs pull-right")
  )) {
  bootstrapPage(
    tags$style("
      html, body { width: 100%; height: 100%; overflow: none; }
      #dialogMainOutput { position: absolute; top: 10px; left: 10px; right: 10px; }
      #dialogControls {
        position: absolute; bottom: 0px; left: 0px; right: 0px;
        padding: 10px 10px 0 10px;
        background-color: #444; color: white;
      }"
    ),
    tags$div(id = "dialogMainOutput",
      style = sprintf("bottom:%dpx;", statusbarHeight),
      outputControl
    ),
    tags$div(id = "dialogControls",
      style = sprintf("height:%dpx;", statusbarHeight),
      statusbar,
      buttons
    )
  )
}

#' @examples
#'
#' p <- ggplot(diamonds, aes(carat, price)) + geom_point() + facet_wrap(~cut)
#' ggbrush(p)
#'
#' p <- ggplot(diamonds, aes(x=carat)) + geom_bar()
#' ggbrush(p, direction = "x")
