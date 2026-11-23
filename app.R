# app.R
#
# Running this script launches the trajectory explorer app!
# 
# Required:
#   R/helpers.R
#   R/buildsyntheticdata.R
#   www/logo.png

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(htmltools)
library(plotly)
library(lubridate)

shiny::devmode(FALSE)

source("scripts/buildsyntheticdata.R", local = TRUE)

# App parameters - These can be modified to change the default app settings ----

default_population_size <- 25
maximum_population_size <- 100
maximum_gap <- 730
elevated_threshold <- 3.5

trajectory_levels <- c(
  "low-low",
  "low-high",
  "high-low",
  "high-high"
)

trajectory_labels <- c(
  "low-low" = "Initially below reference value; finally below",
  "low-high" = "Initially below reference value; finally elevated",
  "high-low" = "Initially elevated; finally below reference value",
  "high-high" = "Initially elevated; finally elevated"
)

trajectory_colors <- c(
  "low-low" = "#540d6e",
  "low-high" = "#ee4266",
  "high-low" = "#0ead69",
  "high-high" = "#ffd23f"
)

test_result_levels <- c(
  "Below 3.5 \u00B5g/dL",
  "Elevated (\u22653.5 \u00B5g/dL)"
)

test_result_colors <- setNames(
  c("#540d6e", "#ee4266"),
  test_result_levels
)

trajectory_line_width <- 2
test_point_size <- 8

# User interface - This builds the visual components of the app ------

ui <- page_sidebar(
  title = tags$div(
    class = "um-app-title",

    tags$img(
      src = "logo.png",
      alt = "University of Michigan",
      class = "um-app-logo"
    ),

    tags$div(
      class = "um-app-title-text",

      tags$span(
        class = "um-app-title-main",
        "Childhood Blood Lead Trajectory Explorer"
      ),

      tags$span(
        class = "um-app-title-subtitle",
        "Synthetic data demonstration"
      )
    )
  ),

  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#00274C",
    secondary = "#475569",
    danger = "#9A3324",
    success = "#2F6544",
    warning = "#FFCB05",
    base_font = "Arial, Helvetica, sans-serif"
  ),

  fillable = TRUE,

  #This section is written in HTML/CSS, which allows for
  # more control over specific visual aspects
  tags$head(
    tags$style(
      HTML(
        "
        /* University of Michigan maize and blue theme */

        .navbar {
          background-color: #00274c !important;
          border-bottom: 5px solid #ffcb05;
          padding-top: 0.75rem;
          padding-bottom: 0.75rem;
        }

        .navbar-brand {
          color: #ffffff !important;
          white-space: normal;
          margin-right: 0;
        }

        .um-app-title {
          display: flex;
          align-items: center;
          gap: 1rem;
          min-width: 0;
        }

        .um-app-logo {
          display: block;
          flex: 0 0 auto;
          width: 64px;
          height: 64px;
          object-fit: contain;
          background-color: #ffffff;
          padding: 4px;
          border-radius: 4px;
        }

        .um-app-title-text {
          display: flex;
          flex-direction: column;
          gap: 0.2rem;
          min-width: 0;
        }

        .um-app-title-main {
          color: #ffffff;
          font-size: 1.3rem;
          font-weight: 700;
          line-height: 1.3;
        }

        .um-app-title-subtitle {
          color: #ffcb05;
          font-size: 0.85rem;
          font-weight: 400;
          line-height: 1.3;
        }

        .card-header {
          background-color: #00274c;
          color: #ffffff;
          border-bottom: 3px solid #ffcb05;
          font-weight: 600;
        }

        .bslib-sidebar-layout > .sidebar {
          background-color: #f5f7fa;
        }

        .sidebar h4 {
          color: #00274c;
          font-weight: 700;
        }

        .population-controls {
          display: flex;
          flex-wrap: wrap;
          align-items: flex-end;
          gap: 1rem;
        }

        .population-controls .form-group {
          margin-bottom: 0;
        }

        .population-controls .btn {
          margin-bottom: 0.25rem;
        }

        #generate_population {
          background-color: #ffcb05;
          border-color: #ffcb05;
          color: #00274c;
          font-weight: 700;
        }

        #generate_population:hover {
          background-color: #e5b600;
          border-color: #e5b600;
          color: #00274c;
        }

        #generate_population:focus-visible {
          outline: 3px solid #00274c;
          outline-offset: 3px;
        }

        .population-status {
          margin-top: 0.75rem;
          color: #475569;
        }

        .synthetic-notice {
          padding: 0.9rem 1rem;
          border-left: 5px solid #ffcb05;
          border-radius: 4px;
          background-color: #fff8df;
          color: #00274c;
        }

        .detailed-summary-controls {
          display: flex;
          flex-wrap: wrap;
          align-items: center;
          gap: 0.75rem;
          margin-top: 0.75rem;
          margin-bottom: 0.75rem;
        }

        .detailed-summary-table {
          max-height: 520px;
          overflow: auto;
        }

        .detailed-summary-table table {
          width: 100%;
        }

        .detailed-summary-footnote {
          margin-top: 0.6rem;
          color: #475569;
          font-size: 0.85rem;
        }

        /* Emphasize the counts without changing the descriptive labels. */
        #summary_table tbody td:nth-child(2) {
          font-weight: 700;
          font-size: 1.05rem;
          color: #00274c;
        }

        @media (max-width: 600px) {
          .um-app-title {
            gap: 0.65rem;
          }

          .um-app-logo {
            width: 48px;
            height: 48px;
          }

          .um-app-title-main {
            font-size: 1rem;
          }

          .um-app-title-subtitle {
            font-size: 0.75rem;
          }
        }
      "
      )
    )
  ),

  # This is the settings section, controlling sliders and buttons
  sidebar = sidebar(
    width = 340,

    h4("Analysis settings"),

    sliderInput(
      inputId = "minimum_gap",
      label = "Minimum gap between separate tests",
      min = 0,
      max = maximum_gap,
      value = 30,
      step = 1,
      post = " days"
    ),

    numericInput(
      inputId = "minimum_gap_numeric",
      label = "Enter minimum gap",
      value = 30,
      min = 0,
      max = maximum_gap,
      step = 1
    ),

    helpText(
      textOutput(
        "minimum_gap_description",
        inline = TRUE
      )
    ),

    hr(),

    selectInput(
      inputId = "time_baseline",
      label = "Time baseline",
      choices = c(
        "Time since first test" = "time_since_first",
        "Calendar date" = "calendar_date"
      ),
      selected = "time_since_first"
    ),

    helpText(
      paste(
        "Time since first test places each child's first retained test",
        "at day 0. Calendar date uses the simulated testing dates."
      )
    ),

    hr(),

    selectizeInput(
      inputId = "county_filter",
      label = "County",
      choices = NULL,
      multiple = TRUE,
      options = list(
        placeholder = "All counties",
        plugins = list("remove_button")
      )
    ),

    selectizeInput(
      inputId = "sex_filter",
      label = "Sex",
      choices = NULL,
      multiple = TRUE,
      options = list(
        placeholder = "All values",
        plugins = list("remove_button")
      )
    ),

    checkboxInput(
      inputId = "under_six_only",
      label = "Only include children under age 6 at first retained test",
      value = FALSE
    ),

    hr(),

    helpText(
      paste(
        "Counties demonstrate geographic filtering only.",
        "The simulated records do not describe actual county conditions."
      )
    )
  ),

  card(
    fill = FALSE,

    card_body(
      tags$div(
        class = "population-controls",

        numericInput(
          inputId = "population_size",
          label = "Population size",
          value = default_population_size,
          min = 1,
          max = maximum_population_size,
          step = 1,
          width = "220px"
        ),

        actionButton(
          inputId = "generate_population",
          label = "Generate Random Population",
          class = "btn-primary"
        )
      ),

      tags$div(
        class = "population-status",
        role = "status",
        textOutput("population_status")
      ),

      helpText(
        paste(
          "A random population is generated when your session starts.",
          "The button replaces your session's population and clears",
          "county, sex, and age filters. No records are saved to disk."
        )
      )
    )
  ),

  tags$div(
    class = "synthetic-notice",
    role = "note",

    tags$strong("Synthetic demonstration data. "),

    paste(
      "All children, dates, and blood lead measurements are simulated.",
      "This portfolio application is not a surveillance report",
      "or a clinical decision tool."
    )
  ),

  card(
    fill = FALSE,
    full_screen = FALSE,

    card_header("Summary"),

    card_body(
      tableOutput("summary_table"),

      tags$div(
        class = "detailed-summary-controls",

        actionButton(
          inputId = "toggle_detailed_summary",
          label = "Show detailed summary",
          class = "btn-primary"
        ),

        uiOutput(
          "detailed_summary_download_ui",
          inline = TRUE
        )
      ),

      uiOutput("detailed_summary_ui")
    )
  ),

  card(
    fill = FALSE,
    full_screen = TRUE,

    card_header("Blood lead trajectories"),

    card_body(
      plotlyOutput(
        "trajectory_plot",
        width = "100%",
        height = "950px"
      )
    )
  )
)

# Server - These parts connect with the ui to "make stuff happen" ----

server <- function(input, output, session) {
  # Generate a synthetic population
  current_tests <- reactiveVal(
    prepare_synthetic_tests(
      build_synthetic_data(default_population_size)
    )
  )

  # Regenerate population if you click the button
  observeEvent(input$generate_population, {
    n_children <- input$population_size

    if (
      length(n_children) != 1L ||
        !is.numeric(n_children) ||
        !is.finite(n_children) ||
        n_children < 1 ||
        n_children > maximum_population_size ||
        n_children != floor(n_children)
    ) {
      showNotification(
        paste0(
          "Enter a whole number of children between 1 and ",
          format(maximum_population_size, big.mark = ","),
          "."
        ),
        type = "error"
      )

      return()
    }

    new_tests <- tryCatch(
      {
        prepare_synthetic_tests(
          build_synthetic_data(n_children)
        )
      },
      error = function(error) {
        showNotification(
          paste(
            "Population generation failed:",
            conditionMessage(error)
          ),
          type = "error"
        )

        NULL
      }
    )

    if (is.null(new_tests)) {
      return()
    }

    current_tests(new_tests)

    updateCheckboxInput(
      session,
      "under_six_only",
      value = FALSE
    )

    showNotification(
      paste(
        "Generated",
        format(n_children, big.mark = ","),
        "synthetic records."
      ),
      type = "message",
      duration = 4
    )
  })

  output$population_status <- renderText({
    dat <- current_tests()

    paste0(
      "Current population: ",
      format(n_distinct(dat$Name), big.mark = ","),
      " children and ",
      format(nrow(dat), big.mark = ","),
      " source test records."
    )
  })

  # Keep the gap slider and numeric input synced

  observeEvent(input$minimum_gap, {
    req(
      !is.null(input$minimum_gap),
      is.finite(input$minimum_gap)
    )

    if (!isTRUE(input$minimum_gap_numeric == input$minimum_gap)) {
      updateNumericInput(
        session,
        "minimum_gap_numeric",
        value = input$minimum_gap
      )
    }
  })

  observeEvent(input$minimum_gap_numeric, {
    req(
      !is.null(input$minimum_gap_numeric),
      is.finite(input$minimum_gap_numeric)
    )

    new_gap <- max(
      0,
      min(maximum_gap, round(input$minimum_gap_numeric))
    )

    if (!isTRUE(input$minimum_gap_numeric == new_gap)) {
      updateNumericInput(
        session,
        "minimum_gap_numeric",
        value = new_gap
      )
    }

    if (!isTRUE(input$minimum_gap == new_gap)) {
      updateSliderInput(
        session,
        "minimum_gap",
        value = new_gap
      )
    }
  })

  gap_value <- debounce(
    reactive(input$minimum_gap),
    millis = 400
  )

  output$minimum_gap_description <- renderText({
    gap <- input$minimum_gap

    req(!is.null(gap), is.finite(gap))

    paste0(
      "The minimum gap for tests to be considered separate tests is ",
      gap,
      if (gap == 1) " day." else " days.",
      " Tests within this window are considered re-tests, and are deduplicated, keeping venous tests over capillary tests, and keeping the highest reported value from either."
    )
  })

  longitudinal_data <- reactive({
    req(!is.null(gap_value()))
    # From R/helpers.R
    find_multiple_tests(
      df = current_tests(),
      minimum_gap = gap_value()
    )
  })

  #Manipulate synthetic data into wide format
  case_metadata <- reactive({
    dat <- longitudinal_data()

    if (nrow(dat) == 0L) {
      return(
        tibble(
          .child_name = character(),
          Name = character(),
          trajectory = factor(
            character(),
            levels = trajectory_levels
          ),
          County = character(),
          Sex = character(),
          age_at_first_test = numeric(),
          under_six = logical(),
          first_test_date = as.Date(character()),
          final_test_date = as.Date(character()),
          first_test_result = numeric(),
          final_test_result = numeric(),
          test_count = integer(),
          age_at_first_test_display = character(),
          age_at_final_test_display = character()
        )
      )
    }

    dat %>%
      arrange(.child_name, SPEC_DT_R, .original_row) %>%
      group_by(.child_name) %>%
      summarize(
        Name = first(Name),
        trajectory = first(trajectory),

        County = first_nonmissing(COUNTY),
        Sex = first_nonmissing(SEX),
        date_of_birth = first_nonmissing(DOB_R),

        age_at_first_test = first(Age),

        first_test_date = first(SPEC_DT_R),
        final_test_date = last(SPEC_DT_R),

        first_test_result = first(PB_RESULT_new),
        final_test_result = last(PB_RESULT_new),

        test_count = n(),

        .groups = "drop"
      ) %>%
      mutate(
        trajectory = factor(
          trajectory,
          levels = trajectory_levels
        ),

        Name = replace_na(Name, "Unknown"),
        County = replace_na(County, "Unknown"),
        Sex = replace_na(Sex, "Unknown"),

        under_six = !is.na(age_at_first_test) &
          age_at_first_test < 6,

        age_at_first_test_display = vapply(
          seq_len(n()),
          function(i) {
            format_age_ymd(
              date_of_birth[i],
              first_test_date[i]
            )
          },
          character(1)
        ),

        age_at_final_test_display = vapply(
          seq_len(n()),
          function(i) {
            format_age_ymd(
              date_of_birth[i],
              final_test_date[i]
            )
          },
          character(1)
        ),

        age_at_first_test_display = replace_na(
          age_at_first_test_display,
          "Unknown"
        ),

        age_at_final_test_display = replace_na(
          age_at_final_test_display,
          "Unknown"
        )
      )
  })

  
observeEvent(
    current_tests(),
    {
      dat <- current_tests()

      updateSelectizeInput(
        session,
        "county_filter",
        choices = sort(unique(dat$COUNTY)),
        selected = character(),
        server = TRUE
      )

      updateSelectizeInput(
        session,
        "sex_filter",
        choices = sort(unique(dat$SEX)),
        selected = character(),
        server = TRUE
      )
    },
    ignoreInit = FALSE
  )

  # Apply settings filters as requested
  filtered_metadata <- reactive({
    metadata <- case_metadata()

    if (length(input$county_filter) > 0L) {
      metadata <- metadata %>%
        filter(County %in% input$county_filter)
    }

    if (length(input$sex_filter) > 0L) {
      metadata <- metadata %>%
        filter(Sex %in% input$sex_filter)
    }

    if (isTRUE(input$under_six_only)) {
      metadata <- metadata %>%
        filter(under_six)
    }

    metadata
  })

  filtered_longitudinal <- reactive({
    longitudinal_data() %>%
      filter(
        .child_name %in% filtered_metadata()$.child_name
      )
  })

  # Summary count calculation
  summary_counts <- reactive({
    metadata <- filtered_metadata()

    counts <- vapply(
      trajectory_levels,
      function(code) {
        as.integer(
          sum(
            as.character(metadata$trajectory) == code,
            na.rm = TRUE
          )
        )
      },
      integer(1)
    )

    tibble(
      Measure = c(
        "Children with multiple retained tests",
        unname(trajectory_labels[trajectory_levels])
      ),
      Count = c(nrow(metadata), unname(counts))
    )
  })

  output$summary_table <- renderTable(
    summary_counts(),
    striped = TRUE,
    hover = TRUE,
    spacing = "xs",
    width = "100%",
    align = "lr",
    rownames = FALSE
  )

  # Expandable detailed summary component
  detailed_summary_visible <- reactiveVal(FALSE)

  observeEvent(input$toggle_detailed_summary, {
    visible <- !detailed_summary_visible()

    detailed_summary_visible(visible)

    updateActionButton(
      session,
      "toggle_detailed_summary",
      label = if (visible) {
        "Hide detailed summary"
      } else {
        "Show detailed summary"
      }
    )
  })

  detailed_summary <- reactive({
    filtered_metadata() %>%
      transmute(
        `Summary category` = unname(
          trajectory_labels[as.character(trajectory)]
        ),

        # Extract the numeric identifier from "Synthetic Child 00001".
        `Synthetic ID` = as.integer(
          gsub("[^0-9]", "", Name)
        ),

        County,
        Sex,

        `Age at first test` = age_at_first_test_display,
        `First test date` = format(first_test_date, "%m/%d/%Y"),
        `First test result (µg/dL)` = first_test_result,

        `Age at final test` = age_at_final_test_display,
        `Final test date` = format(final_test_date, "%m/%d/%Y"),
        `Final test result (µg/dL)` = final_test_result,

        `Number of retained tests` = test_count
      ) %>%
      arrange(`Summary category`, `Synthetic ID`)
  })

  output$detailed_summary_download_ui <- renderUI({
    if (!detailed_summary_visible()) {
      return(NULL)
    }

    downloadButton(
      "download_detailed_summary",
      "Download synthetic summary as CSV",
      class = "btn-primary"
    )
  })

  output$detailed_summary_ui <- renderUI({
    if (!detailed_summary_visible()) {
      return(NULL)
    }

    tagList(
      hr(),

      h5("Synthetic data used to generate this summary"),

      helpText(
        "This table reflects the current gap, county, sex, and age filters."
      ),

      tags$div(
        class = "detailed-summary-table",
        tableOutput("detailed_summary_table")
      ),

      tags$p(
        class = "detailed-summary-footnote",

        tags$em(
          paste(
            "The first and final tests shown are retained tests only.",
            "Records removed by the testing-window selection procedure",
            "are not included. All values are synthetic."
          )
        )
      )
    )
  })

  output$detailed_summary_table <- renderTable(
    detailed_summary(),
    striped = TRUE,
    hover = TRUE,
    spacing = "xs",
    width = "100%",
    rownames = FALSE
  )

  
output$download_detailed_summary <- downloadHandler(
    filename = function() {
      paste0(
        "synthetic_lead_trajectory_summary_",
        format(Sys.Date(), "%Y-%m-%d"),
        ".csv"
      )
    },

    content = function(file) {
      write.csv(
        detailed_summary(),
        file = file,
        row.names = FALSE,
        na = "",
        fileEncoding = "UTF-8"
      )
    }
  )

  # Generate the interactive trajectory plot
  output$trajectory_plot <- renderPlotly({
    dat <- filtered_longitudinal() %>%
      filter(
        is.finite(PB_RESULT_new),
        PB_RESULT_new >= 0,
        !is.na(SPEC_DT_R),
        is.finite(time_since_first_test),
        time_since_first_test >= 0
      )

    validate(
      need(
        nrow(dat) > 0L,
        paste(
          "No children have multiple retained tests under the current",
          "settings. Reduce the minimum gap, clear filters,",
          "or generate another population."
        )
      )
    )

    use_calendar_dates <- identical(
      input$time_baseline,
      "calendar_date"
    )

    # axis config by settings
    if (use_calendar_dates) {
      date_min <- min(dat$SPEC_DT_R)
      date_max <- max(dat$SPEC_DT_R)

      date_span <- as.numeric(date_max - date_min)
      date_padding <- max(1, ceiling(date_span * 0.03))

      dat$plot_x <- format(dat$SPEC_DT_R, "%Y-%m-%d")

      x_axis_range <- format(
        c(
          date_min - date_padding,
          date_max + date_padding
        ),
        "%Y-%m-%d"
      )

      x_axis_title <- "Simulated testing date"
      x_axis_type <- "date"

      x_axis_tickformat <- if (date_span >= 180) {
        "%b %Y"
      } else {
        "%b %d, %Y"
      }

      plot_title <- "Synthetic blood lead levels by test date"
    } else {
      dat$plot_x <- dat$time_since_first_test

      time_max <- max(dat$plot_x)

      x_axis_range <- c(
        -max(1, time_max * 0.01),
        max(1, time_max + max(1, time_max * 0.03))
      )

      x_axis_title <- "Days since first retained test"
      x_axis_type <- "linear"
      x_axis_tickformat <- NULL

      plot_title <- "Synthetic blood lead levels since first retained test"
    }

    y_max <- max(dat$PB_RESULT_new, elevated_threshold)

    y_axis_range <- c(
      -max(0.1, y_max * 0.01),
      y_max + max(0.5, y_max * 0.08)
    )

    # hover over points to see details
    dat <- dat %>%
      mutate(
        trajectory_code = as.character(trajectory),

        trajectory_display = unname(
          trajectory_labels[trajectory_code]
        ),

        test_result = factor(
          if_else(
            elevated,
            test_result_levels[2],
            test_result_levels[1]
          ),
          levels = test_result_levels
        ),

        sample_type_display = case_when(
          SAM_TYP == "V" ~ "Venous",
          SAM_TYP == "C" ~ "Capillary",
          is.na(SAM_TYP) ~ "Unknown",
          TRUE ~ as.character(SAM_TYP)
        ),

        age_display = if_else(
          is.na(Age),
          "Unknown",
          paste0(round(Age, 2), " years")
        ),

        hover_baseline = if (use_calendar_dates) {
          ""
        } else {
          paste0(
            "<br><b>Days since first retained test:</b> ",
            time_since_first_test
          )
        },

        hover_text = paste0(
          "<b>Synthetic ID:</b> ",
          as.integer(gsub("[^0-9]", "", Name)),

          "<br><b>Trajectory:</b> ",
          htmlEscape(trajectory_display),

          "<br><b>Simulated test date:</b> ",
          format(SPEC_DT_R, "%B %d, %Y"),

          "<br><b>Blood lead level:</b> ",
          PB_RESULT_new,
          " \u00B5g/dL",

          "<br><b>Test result:</b> ",
          htmlEscape(as.character(test_result)),

          "<br><b>Sample type:</b> ",
          htmlEscape(sample_type_display),

          "<br><b>Age at test:</b> ",
          age_display,

          "<br><b>County:</b> ",
          htmlEscape(replace_na(COUNTY, "Unknown")),

          "<br><b>Sex:</b> ",
          htmlEscape(replace_na(SEX, "Unknown")),

          hover_baseline,

          "<br><b>Retained test:</b> ",
          retained_test_number,
          " of ",
          retained_test_count,

          "<br><i>Synthetic demonstration record</i>"
        )
      )

    # Valid coordinates matching the active x-axis type.
    # Legend-only traces are never displayed in the plotting area.
    legend_x <- dat$plot_x[1]
    legend_y <- dat$PB_RESULT_new[1]

    plot_object <- plot_ly()

    # fix the legend
    for (i in seq_along(trajectory_levels)) {
      code <- trajectory_levels[i]

      plot_object <- plot_object %>%
        add_trace(
          x = legend_x,
          y = legend_y,
          type = "scatter",
          mode = "lines",

          name = unname(trajectory_labels[code]),

          legendgroup = "trajectory",

          legendgrouptitle = if (i == 1L) {
            list(text = "Trajectory")
          } else {
            NULL
          },

          legendrank = i,
          visible = "legendonly",
          showlegend = TRUE,

          line = list(
            color = unname(trajectory_colors[code]),
            width = trajectory_line_width
          ),

          hoverinfo = "skip"
        )
    }

    # Generate lines for each child's trajectory
    child_groups <- split(dat, dat$.child_name)

    for (child_data in child_groups) {
      child_data <- child_data %>%
        arrange(SPEC_DT_R, .original_row)

      code <- child_data$trajectory_code[1]

      plot_object <- plot_object %>%
        add_trace(
          data = child_data,
          x = ~plot_x,
          y = ~PB_RESULT_new,

          type = "scatter",
          mode = "lines",

          legendgroup = "trajectory",
          showlegend = FALSE,

          line = list(
            color = unname(trajectory_colors[code]),
            width = trajectory_line_width
          ),

          text = ~hover_text,
          hovertemplate = "%{text}<extra></extra>"
        )
    }

    for (i in seq_along(test_result_levels)) {
      result_level <- test_result_levels[i]

      plot_object <- plot_object %>%
        add_trace(
          x = legend_x,
          y = legend_y,
          type = "scatter",
          mode = "markers",

          name = result_level,

          legendgroup = "test-result",

          legendgrouptitle = if (i == 1L) {
            list(text = "Test result")
          } else {
            NULL
          },

          legendrank = length(trajectory_levels) + i,
          visible = "legendonly",
          showlegend = TRUE,

          marker = list(
            size = test_point_size,
            color = unname(test_result_colors[result_level]),
            line = list(width = 0)
          ),

          hoverinfo = "skip"
        )
    }

    for (result_level in test_result_levels) {
      point_data <- dat %>%
        filter(
          as.character(test_result) == .env$result_level
        )

      if (nrow(point_data) == 0L) {
        next
      }

      plot_object <- plot_object %>%
        add_trace(
          data = point_data,
          x = ~plot_x,
          y = ~PB_RESULT_new,

          type = "scatter",
          mode = "markers",

          legendgroup = "test-result",
          showlegend = FALSE,
          cliponaxis = FALSE,

          marker = list(
            symbol = "circle",
            size = test_point_size,
            color = unname(test_result_colors[result_level]),
            line = list(width = 0)
          ),

          text = ~hover_text,
          hovertemplate = "%{text}<extra></extra>"
        )
    }

    # Determine plot layout

    plot_object %>%
      layout(
        title = list(
          text = plot_title,
          x = 0.02,
          xanchor = "left",
          y = 0.99,
          yanchor = "top",

          font = list(
            size = 18,
            color = "#00274C"
          )
        ),

        xaxis = list(
          title = list(text = x_axis_title),
          type = x_axis_type,
          range = x_axis_range,
          autorange = FALSE,
          tick0 = if (use_calendar_dates) NULL else 0,
          tickformat = x_axis_tickformat,
          showgrid = TRUE,
          zeroline = FALSE,
          layer = "below traces"
        ),

        yaxis = list(
          title = list(
            text = "Blood lead level (\u00B5g/dL)"
          ),
          range = y_axis_range,
          autorange = FALSE,
          tick0 = 0,
          showgrid = TRUE,
          zeroline = FALSE,
          layer = "below traces"
        ),

        shapes = list(
          list(
            type = "line",
            xref = "paper",
            x0 = 0,
            x1 = 1,
            yref = "y",
            y0 = elevated_threshold,
            y1 = elevated_threshold,

            line = list(
              color = "#00274C",
              width = 1.5,
              dash = "dot"
            )
          )
        ),

        annotations = list(
          list(
            text = paste0(
              "The dotted line marks the 3.5 \u00B5g/dL ",
              "blood lead reference value used in this demonstration.",
              "<br>All measurements are synthetic; ",
              "a result below the reference value does not imply ",
              "that lead exposure is safe."
            ),

            x = 0,
            y = -0.15,
            xref = "paper",
            yref = "paper",
            xanchor = "left",
            yanchor = "top",
            showarrow = FALSE,
            align = "left",

            font = list(
              size = 11,
              color = "#475569"
            )
          )
        ),

        hovermode = "closest",
        showlegend = TRUE,

        legend = list(
          orientation = "v",
          x = 0,
          xanchor = "left",
          y = 1.04,
          yanchor = "bottom",
          traceorder = "grouped",
          itemclick = FALSE,
          itemdoubleclick = FALSE,
          bgcolor = "#FFFFFF",
          bordercolor = "#00274C",
          borderwidth = 1,

          font = list(
            size = 11,
            color = "#1E293B"
          )
        ),

        margin = list(
          l = 85,
          r = 35,
          b = 155,
          t = 250
        ),

        paper_bgcolor = "#FFFFFF",
        plot_bgcolor = "#FFFFFF"
      ) %>%
      config(
        displaylogo = FALSE,
        responsive = TRUE,

        modeBarButtonsToRemove = c(
          "lasso2d",
          "select2d"
        )
      )
  })
}

# This function launches the app itself
shinyApp(ui = ui, server = server)
