#' Linked display of multiple ggplot objects
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' gglinked(
#'   iris,
#'   ggplot(, aes(Sepal.Length, Sepal.Width)) + geom_point(aes(color = selected_)),
#'   ggplot(, aes(Petal.Length, Petal.Width)) + geom_point(aes(color = selected_)),
#'   ggplot(, aes(Species)) + geom_bar(stat = "count", aes(fill = selected_)),
#'   ggplot(, aes(Species)) + geom_bar(stat = "count", aes(fill = selected_))
#' )
#'
#' gglinked(
#'   mpg,
#'   ggplot(, aes(cyl, cty)) + geom_point(aes(color = selected_)),
#'   ggplot(, aes(displ, cty)) + geom_point(aes(color = selected_))
#' )
#' }
#'
#' @export
gglinked <- function(df, ...) {
  plots <- list(...)
  nplots <- length(plots)

  plotObj <- function(n) {
    dimensions <- paste(intersect(c("x", "y"), names(plots[[n]]$mapping)), collapse = "")
    plotOutput(paste0("plot", n), height = "100%",
      brush = brushOpts("brush", direction = dimensions, fill = "transparent")
    )
  }

  plotUI <- if (nplots == 0) {
    stop("No plots provided")
  } else if (nplots > 4) {
    stop("Too many plots provided; limit 4")
  } else if (nplots == 1) {
    plotObj(1)
  } else if (nplots == 2) {
    fillRow(
      plotObj(1),
      plotObj(2)
    )
  } else if (nplots == 3) {
    fillRow(
      plotObj(1),
      fillCol(
        plotObj(2),
        plotObj(3)
      )
    )
  } else if (nplots == 4) {
    fillCol(
      fillRow(
        plotObj(1), plotObj(2)
      ),
      fillRow(
        plotObj(3), plotObj(4)
      )
    )
  }

  ui <- miniPage(
    tags$head(tags$style(type="text/css", ".recalculating { transition: opacity 250ms ease 1500ms; }")),
    gadgetTitleBar("Linked brushing"),
    miniContentPanel(padding = 0,
      plotUI
    )
  )

  server <- function(input, output, session) {
    fortified <- reactive({
      if (is.null(input$brush)) {
        cbind(df, selected_ = FALSE)
      } else {
        brushedPoints(df, input$brush, allRows = TRUE)
      }
    })

    observeEvent(input$done, {
      if (is.null(input$brush)) {
        stopApp(df)
      } else {
        stopApp(brushedPoints(df, input$brush))
      }
    })

    lapply(1:nplots, function(n) {
      output[[paste0("plot", n)]] <- renderPlot({
        df2 <- fortified()
        (plots[[n]]
          + scale_color_manual(values = c("black", "#66D65C"), guide = FALSE)
          + scale_fill_manual(values = c("black", "#66D65C"), guide = FALSE)) %+% df2
      })
    })
  }

  runGadget(ui, server, viewer = dialogViewer("gglinked", 100000, 100000))
}
