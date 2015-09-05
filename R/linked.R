#' Linked display of multiple ggplot objects
#'
#' @examples
#' \dontrun{
#' gglinked(
#'   iris,
#'   ggplot(, aes(Sepal.Length, Sepal.Width)) + geom_point(aes(color = selected_)),
#'   ggplot(, aes(Petal.Length, Petal.Width)) + geom_point(aes(color = selected_)),
#'   ggplot(, aes(Species)) + geom_histogram(aes(fill = selected_)),
#'   ggplot(, aes(Species)) + geom_histogram(aes(fill = selected_))
#' )
#'
#' gglinked(
#'   mpg,
#'   ggplot(, aes(cyl, cty)) + geom_point(aes(color = selected_)),
#'   ggplot(, aes(class, cty)) + geom_point(aes(color = selected_))
#' )
#' }
#'
#' @export
gglinked <- function(df, ...) {
  plots <- list(...)
  nplots <- length(plots)

  if (nplots == 0) {
    stop("No plots provided")
  }
  plotDivs <- lapply(1:nplots, function(n) {
    dimensions <- paste(intersect(c("x", "y"), names(plots[[n]]$mapping)), collapse = "")

    tags$div(class = sprintf("plotdiv plotdiv-%d-%d", nplots, n),
      plotOutput(paste0("plot", n), height = "100%",
        brush = brushOpts("brush", direction = dimensions)
      )
    )
  })

  ui <- dialogPage(tags$div(class = "plot-container",
    tags$style(type="text/css", "
      .recalculating { transition: opacity 250ms ease 1500ms; }
      .plot-container {
        position: relative;
        width: 100%;
        height: 100%;
      }
      .plotdiv {
        position: absolute;
        left: 0;
        top: 0;
        right: 0;
        bottom: 0;
      }
      .plotdiv-1-1 { }
      .plotdiv-2-1 { right: 50%; }
      .plotdiv-2-2 { left: 50%; }
      .plotdiv-3-1 { right: 50%; }
      .plotdiv-3-2 { left: 50%; bottom: 50%; }
      .plotdiv-3-3 { left: 50%; top: 50%; }
      .plotdiv-4-1 { right: 50%; bottom: 50%; }
      .plotdiv-4-2 { left: 50%; bottom: 50%; }
      .plotdiv-4-3 { right: 50%; top: 50%; }
      .plotdiv-4-4 { left: 50%; top: 50%; }
    "),
    plotDivs
  ))

  server <- function(input, output, session) {
    v <- reactiveValues(activeBrush = NULL)

    observe({
      if (!is.null(input$brush)) {
        v$activeBrush <- input$brush
      }
    })

    fortified <- reactive({
      if (is.null(v$activeBrush)) {
        cbind(df, selected_ = FALSE)
      } else {
        brushedPoints(df, v$activeBrush, allRows = TRUE)
      }
    })

    observeEvent(input$done, {
      if (is.null(v$activeBrush)) {
        stopApp(df)
      } else {
        stopApp(brushedPoints(df, v$activeBrush))
      }
    })

    lapply(1:nplots, function(n) {
      output[[paste0("plot", n)]] <- renderPlot({
        df2 <- fortified()
        (plots[[n]]
          + scale_color_manual(values = c("black", "red"), guide = FALSE)
          + scale_fill_manual(values = c("black", "red"), guide = FALSE)) %+% df2
      })
    })
  }

  runApp(shinyApp(ui, server), launch.browser = getOption("viewer", TRUE))
}
