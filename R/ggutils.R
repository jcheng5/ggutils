#' @import ggplot2 shiny
NULL

#' Zoom in on ggplots
#'
#' Zoom in on a ggplot object by dragging a selection with the mouse.
#'
#' @param plotExpr A ggplot2 plot object
#' @return The final zoom bounds (or \code{NULL} if unzoomed)
#'
#' @examples
#' \donttest{
#' library(ggplot2)
#' p <- ggplot(cars, aes(speed, dist)) + geom_point()
#' ggzoom(p)
#'
#' # Also works for one-dimensional visualizations
#' p2 <- ggplot(cars, aes(dist)) + geom_histogram()
#' gzoom(p2)
#' }
#' @export
ggzoom <- function(plotExpr) {
  dimensions <- paste(intersect(c("x", "y"), names(plotExpr$mapping)), collapse = "")

  # See below for definition of dialogPage function
  ui <- dialogPage(
    plotOutput("plot", height = "100%", # Fill the dialog
      brush = brushOpts(id = "brush", direction = dimensions,
        resetOnNew = TRUE
      )
    ),
    statusbar = actionButton("reset", "Unzoom", class = "btn-xs")
  )

  server <- function(input, output, session) {
    v <- reactiveValues(bounds = NULL, result = NULL)

    # Show the plot... that's important.
    output$plot <- renderPlot({
      p <- plotExpr
      if (!is.null(v$bounds)) {
        if (!is.null(v$bounds$x)) {
          p <- p + scale_x_continuous(limits = v$bounds$x)
        }
        if (!is.null(v$bounds$y)) {
          p <- p + scale_y_continuous(limits = v$bounds$y)
        }
      }
      p
    })

    observeEvent(input$brush, {
      bounds <- NULL
      results <- NULL
      if (!is.null(input$brush$xmin)) {
        bounds <- c(bounds, list(x = c(input$brush$xmin, input$brush$xmax)))
        if (!is.null(input$brush$mapping$x)) {
          results <- c(results, list())
          results[[input$brush$mapping$x]] <- bounds$x
        }
      }
      if (!is.null(input$brush$ymin)) {
        bounds <- c(bounds, list(y = c(input$brush$ymin, input$brush$ymax)))
        if (!is.null(input$brush$mapping$y)) {
          results <- c(results, list())
          results[[input$brush$mapping$y]] <- bounds$y
        }
      }
      v$bounds <- bounds
      v$results <- results
    })

    observeEvent(input$reset, {
      v$bounds <- NULL
    })

    # The part of the data frame that is currently brushed (or
    # NULL if no brush is active)
    brushed <- reactive({
      str(input$brush)
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
      stopApp(v$results)
    })
  }

  runApp(shinyApp(ui, server), launch.browser = getOption("viewer", TRUE))
}

#' Identify points on a two-dimensional ggplot2
#' @export
ggidentify <- function(plotExpr, threshold = 5, maxpoints = 1, ...) {
  if (is.null(plotExpr$mapping$x) || is.null(plotExpr$mapping$y)) {
    stop("Only 2D plots are currently supported, sorry")
  }

  # See below for definition of dialogPage function
  ui <- dialogPage(
    plotOutput("plot", hover = "hover", click = "click", height = "100%")
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
        nearPoints(plotExpr$data, input$hover, threshold = threshold,
          maxpoints = 1)
    })

    observeEvent(input$click, {
      pts <- nearPoints(plotExpr$data, input$click,
        threshold = threshold, maxpoints = maxpoints, ...)

      if (!is.null(pts) && nrow(pts) > 0)
        stopApp(pts)
    })

    # Show a message giving instructions, or showing how many
    # rows are selected
    output$msg <- renderText({
      if (is.null(near()) || nrow(near()) == 0)
        return("")

      nearest <- near()

      name <- row.names(nearest)
      values <- paste0(names(nearest), ": ", sapply(nearest, as.character), collapse = ", ")
      paste0(
        name, ". ", values
      )
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
#' Call ggbrush with a ggplot2 object. The plot will show in RStudio Viewer or
#' your web browser, and any observations selected by the user will be returned.
#' @export
ggbrush <- function(plotExpr) {
  dimensions <- paste(intersect(c("x", "y"), names(plotExpr$mapping)), collapse = "")

  # See below for definition of dialogPage function
  ui <- dialogPage(
    plotOutput("plot", brush = brushOpts(id = "brush", direction = dimensions),
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
        overflow: hidden;
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
