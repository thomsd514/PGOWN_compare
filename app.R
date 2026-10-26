# Copyright [2026] David Thomson and Claude
# Licensed under the Apache License, Version 2.0

library(shiny)
library(readr)
library(ggplot2)
library(patchwork)
library(dplyr)
library(lubridate)

# B.C. design-system-inspired semantic colours used by the plots.
BC_BLUE <- "#013366"
BC_GOLD <- "#FCBA19"
BC_TEAL <- "#00718F"
BC_TEXT <- "#2D2D2D"
BC_GRID <- "#D8D8D8"

# Karly Harker function
get_gwl_data <- function(well, type = "data") {
  if (!grepl("^[0-9]{3}$", well)) {
    stop("Well numbers must contain exactly 3 digits, for example 071.")
  }

  read_csv(
    paste0(
      "https://www.env.gov.bc.ca/wsd/data_searches/obswell/map/data/OW",
      well, "-", type, ".csv"
    ),
    show_col_types = FALSE,
    progress = FALSE
  )
}

make_well_plot <- function(dat, well, colour, line_type,
                           start_date, end_date, ylim = NULL) {
  p <- ggplot(dat, aes(x = Time, y = Value)) +
    geom_line(colour = colour, linewidth = 0.8, linetype = line_type,
              na.rm = TRUE) +
    scale_y_reverse() +
    scale_x_datetime(
      limits = c(
        as.POSIXct(start_date, tz = "America/Vancouver"),
        as.POSIXct(end_date + 1, tz = "America/Vancouver")
      ),
      date_labels = "%b %Y",
      date_breaks = "3 months",
      expand = expansion(mult = c(0, 0.01))
    ) +
    labs(
      title = paste("Observation well", well),
      subtitle = paste(format(start_date, "%B %d, %Y"), "to",
                       format(end_date, "%B %d, %Y")),
      x = NULL,
      y = "Groundwater level (metres below ground)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      text = element_text(colour = BC_TEXT, family = "sans"),
      plot.title = element_text(size = 16, face = "bold", colour = BC_BLUE),
      plot.subtitle = element_text(size = 11, colour = BC_TEXT),
      axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
      axis.text.x = element_text(angle = 0, vjust = 0.5),
      panel.grid.major = element_line(colour = BC_GRID, linewidth = 0.35),
      panel.grid.minor = element_blank(),
      plot.margin = margin(12, 12, 18, 12)
    )

  if (!is.null(ylim)) {
    p <- p + coord_cartesian(ylim = ylim)
  }

  p
}

get_matched_ylims <- function(d1, d2) {
  r1 <- range(d1$Value, na.rm = TRUE)
  r2 <- range(d2$Value, na.rm = TRUE)
  span <- max(diff(r1), diff(r2))

  if (!is.finite(span)) {
    stop("A comparable depth range could not be calculated.")
  }

  # Give constant series a small visible range.
  if (span == 0) span <- 1

  centre_lim <- function(r) {
    midpoint <- mean(r)
    c(midpoint + span / 2, midpoint - span / 2)
  }

  list(lim1 = centre_lim(r1), lim2 = centre_lim(r2))
}

bc_header <- tags$header(
  class = "bc-header",
  tags$div(
    class = "bc-header__inner container-fluid",
    tags$a(
      class = "bc-wordmark",
      href = "https://www2.gov.bc.ca/",
      `aria-label` = "Government of British Columbia home",
      tags$span(class = "bc-wordmark__mark", `aria-hidden` = "true", "BC"),
      tags$span(class = "bc-wordmark__text", "Government of British Columbia")
    )
  )
)

bc_footer <- tags$footer(
  class = "bc-footer",
  tags$div(
    class = "container-fluid bc-footer__inner",
    tags$p("Province of British Columbia"),
    tags$nav(
      `aria-label` = "Footer",
      tags$a(href = "https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer"),
      tags$a(href = "https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy"),
      tags$a(href = "https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility")
    )
  )
)

ui <- tagList(
  tags$head(
    tags$title("Compare groundwater levels"),
    tags$meta(charset = "utf-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$link(rel = "stylesheet", type = "text/css", href = "bcgov.css")
  ),
  tags$a(class = "skip-link", href = "#main-content", "Skip to main content"),
  bc_header,
  tags$main(
    id = "main-content",
    class = "container-fluid bc-main",
    tabindex = "-1",
    tags$section(
      class = "bc-intro",
      tags$h1("Compare groundwater levels"),
      tags$p(
        class = "bc-lead",
        "Compare groundwater-level records from two Provincial Groundwater Observation Network wells."
      ),
      tags$p(
        "Enter two three-digit observation-well numbers and a date range, then select Update plots."
      )
    ),
    fluidRow(
      column(
        width = 4,
        tags$aside(
          class = "bc-panel bc-controls",
          `aria-labelledby` = "controls-heading",
          tags$h2(id = "controls-heading", "Plot settings"),
          tags$p(class = "required-note", tags$span(`aria-hidden` = "true", "*"),
                 " Required fields"),
          textInput("well1", tagList("First well number ", tags$span(class = "required", `aria-hidden` = "true", "*")), value = "409"),
          tags$p(id = "well1-help", class = "help-block", "Enter exactly three digits, for example 071."),
          textInput("well2", tagList("Second well number ", tags$span(class = "required", `aria-hidden` = "true", "*")), value = "384"),
          tags$p(id = "well2-help", class = "help-block", "Enter exactly three digits, for example 384."),
          dateInput("start_date", "Start date", value = Sys.Date() - 365,
                    format = "yyyy-mm-dd", weekstart = 0),
          dateInput("end_date", "End date", value = Sys.Date(),
                    format = "yyyy-mm-dd", weekstart = 0),
          checkboxInput(
            "same_scale",
            "Use the same total depth range for both plots",
            value = FALSE
          ),
          actionButton("goButton", "Update plots", class = "bc-button")
        )
      ),
      column(
        width = 8,
        tags$section(
          class = "bc-panel bc-results",
          `aria-labelledby` = "results-heading",
          tags$h2(id = "results-heading", "Groundwater-level plots"),
          tags$div(
            id = "status-region",
            class = "bc-alert bc-alert--error",
            role = "alert",
            `aria-live` = "assertive",
            `aria-atomic` = "true",
            conditionalPanel("output.errorMsg !== ''", textOutput("errorMsg", inline = TRUE))
          ),
          tags$div(
            class = "plot-description",
            tags$p(
              "Each chart shows groundwater level through time. Depth increases downward, so shallower readings appear higher on the chart. The two wells also use different line patterns so they remain distinguishable without colour."
            )
          ),
          tags$div(
            role = "img",
            `aria-label` = "Two time-series charts comparing groundwater levels for the selected observation wells and date range.",
            plotOutput("wellPlot", height = "680px")
          ),
          uiOutput("plotSummary")
        )
      )
    )
  ),
  bc_footer,
  tags$script(HTML("\n    $(function () {\n      $('#well1').attr({'inputmode':'numeric', 'pattern':'[0-9]{3}', 'maxlength':'3', 'aria-describedby':'well1-help'});\n      $('#well2').attr({'inputmode':'numeric', 'pattern':'[0-9]{3}', 'maxlength':'3', 'aria-describedby':'well2-help'});\n    });\n  "))
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    filtered1 = NULL,
    filtered2 = NULL,
    well1 = NULL,
    well2 = NULL,
    start_date = NULL,
    end_date = NULL,
    error = ""
  )

  output$errorMsg <- renderText(rv$error)

  observeEvent(input$goButton, {
    rv$error <- ""

    tryCatch({
      well1 <- trimws(input$well1)
      well2 <- trimws(input$well2)

      if (!grepl("^[0-9]{3}$", well1) || !grepl("^[0-9]{3}$", well2)) {
        stop("Well numbers must each contain exactly 3 digits.")
      }
      if (well1 == well2) {
        stop("Choose two different well numbers to make a comparison.")
      }
      if (is.null(input$start_date) || is.null(input$end_date)) {
        stop("Select both a start date and an end date.")
      }
      if (input$start_date > input$end_date) {
        stop("Start date must be on or before the end date.")
      }

      data1 <- get_gwl_data(well1)
      data2 <- get_gwl_data(well2)

      if (!all(c("Time", "Value") %in% names(data1)) ||
          !all(c("Time", "Value") %in% names(data2))) {
        stop("The downloaded file does not contain the expected Time and Value columns.")
      }

      data1 <- data1 %>% mutate(Time = ymd_hms(Time, quiet = TRUE))
      data2 <- data2 %>% mutate(Time = ymd_hms(Time, quiet = TRUE))

      start_time <- as.POSIXct(input$start_date, tz = "America/Vancouver")
      end_time <- as.POSIXct(input$end_date + 1, tz = "America/Vancouver")

      filtered1 <- data1 %>%
        filter(!is.na(Time), !is.na(Value), Time >= start_time, Time < end_time)
      filtered2 <- data2 %>%
        filter(!is.na(Time), !is.na(Value), Time >= start_time, Time < end_time)

      if (nrow(filtered1) == 0 || nrow(filtered2) == 0) {
        stop("No data were found for one or both wells in the selected date range.")
      }

      rv$filtered1 <- filtered1
      rv$filtered2 <- filtered2
      rv$well1 <- well1
      rv$well2 <- well2
      rv$start_date <- input$start_date
      rv$end_date <- input$end_date
    }, error = function(e) {
      rv$filtered1 <- NULL
      rv$filtered2 <- NULL
      rv$error <- paste("Unable to update the plots.", conditionMessage(e))
    })
  }, ignoreInit = TRUE)

  output$wellPlot <- renderPlot({
    validate(need(rv$filtered1, "Enter valid settings and select Update plots."))

    ylim1 <- NULL
    ylim2 <- NULL
    if (isTRUE(input$same_scale)) {
      limits <- get_matched_ylims(rv$filtered1, rv$filtered2)
      ylim1 <- limits$lim1
      ylim2 <- limits$lim2
    }

    plot1 <- make_well_plot(
      rv$filtered1, rv$well1, BC_BLUE, "solid",
      rv$start_date, rv$end_date, ylim1
    )
    plot2 <- make_well_plot(
      rv$filtered2, rv$well2, BC_TEAL, "22",
      rv$start_date, rv$end_date, ylim2
    )

    plot1 / plot2 + plot_annotation(
      caption = "Source: B.C. Provincial Groundwater Observation Network"
    ) & theme(plot.caption = element_text(hjust = 0, colour = BC_TEXT, size = 9))
  }, res = 96)

  output$plotSummary <- renderUI({
    req(rv$filtered1, rv$filtered2)
    tags$details(
      class = "bc-details",
      tags$summary("Text summary of the displayed data"),
      tags$ul(
        tags$li(sprintf(
          "Well %s: %s readings; groundwater depth ranges from %.2f to %.2f metres below ground.",
          rv$well1, nrow(rv$filtered1), min(rv$filtered1$Value, na.rm = TRUE),
          max(rv$filtered1$Value, na.rm = TRUE)
        )),
        tags$li(sprintf(
          "Well %s: %s readings; groundwater depth ranges from %.2f to %.2f metres below ground.",
          rv$well2, nrow(rv$filtered2), min(rv$filtered2$Value, na.rm = TRUE),
          max(rv$filtered2$Value, na.rm = TRUE)
        ))
      )
    )
  })
}

shinyApp(ui = ui, server = server)
