# ---------------------------------------------------------------------------
# Shiny + plotly clinician dashboard for the Clinical Digital Twin.
#
# Screens:
#   1. Login
#   2. Cohort overview (sortable/filterable table by current risk tier)
#   3. Patient detail (vitals/activity time series, risk trend, drivers)
#   4. What-if panel (sliders -> live baseline vs. simulated twin risk)
#
# Run:  Rscript -e "shiny::runApp('app.R', port=3838, launch.browser=FALSE)"
#   or from an R session:  shiny::runApp("app.R")
#
# ALL DATA IS SYNTHETIC. Not for clinical use.
# ---------------------------------------------------------------------------

library(shiny)
library(plotly)
library(DT)
library(dplyr)

# Resolve project root and source package code.
if (!nzchar(Sys.getenv("CDT_PROJECT_ROOT"))) {
  Sys.setenv(CDT_PROJECT_ROOT = normalizePath(getwd()))
}
.root <- Sys.getenv("CDT_PROJECT_ROOT")
for (f in list.files(file.path(.root, "R"), pattern = "\\.R$", full.names = TRUE)) {
  source(f)
}

# Shared resources.
con <- cdt_db_connect()
model <- cdt_load_model()

# Risk-tier colors (clinically legible; no chart-junk).
TIER_COLORS <- c(Low = "#2e7d32", Moderate = "#f9a825", High = "#c62828")

# --- UI --------------------------------------------------------------------

login_ui <- function() {
  div(
    style = "max-width:360px;margin:80px auto;padding:24px;border:1px solid #ddd;border-radius:8px;",
    h3("Clinical Digital Twin"),
    p(style = "color:#888;font-size:13px;", "Synthetic-data prototype. Not for clinical use."),
    textInput("login_user", "Username", value = "clinician"),
    passwordInput("login_pass", "Password", value = ""),
    actionButton("login_btn", "Log in", class = "btn-primary"),
    br(), br(),
    div(style = "color:#c62828;", textOutput("login_msg"))
  )
}

dashboard_ui <- function() {
  fluidPage(
    tags$head(tags$style(HTML("
      .risk-badge{padding:2px 8px;border-radius:10px;color:#fff;font-weight:600;}
      .banner{background:#fff8e1;border:1px solid #ffe082;padding:6px 12px;
              border-radius:6px;font-size:12px;margin-bottom:10px;}
    "))),
    div(class = "banner",
      "\u26A0 All patient data is SYNTHETIC. Hackathon prototype - not for clinical use."),
    fluidRow(
      column(4, h3("Cohort")),
      column(8, div(style = "text-align:right;padding-top:18px;",
        span(textOutput("who", inline = TRUE)),
        actionLink("logout_btn", "Log out")
      ))
    ),
    tabsetPanel(
      id = "tabs",
      tabPanel(
        "Cohort overview",
        br(),
        fluidRow(
          column(3, selectInput("filter_tier", "Filter by 7d risk tier",
            choices = c("All", "High", "Moderate", "Low"), selected = "All")),
          column(3, selectInput("sort_by", "Sort by",
            choices = c("7d risk" = "p_7d", "24h risk" = "p_24h", "Age" = "age"),
            selected = "p_7d"))
        ),
        DTOutput("cohort_table")
      ),
      tabPanel(
        "Patient detail",
        br(),
        fluidRow(
          column(4, selectInput("sel_patient", "Patient", choices = NULL)),
          column(8, uiOutput("risk_cards"))
        ),
        fluidRow(
          column(6, plotlyOutput("plot_steps", height = "230px")),
          column(6, plotlyOutput("plot_hr", height = "230px"))
        ),
        fluidRow(
          column(6, plotlyOutput("plot_bp", height = "230px")),
          column(6, plotlyOutput("plot_sedentary", height = "230px"))
        ),
        br(),
        h4("Top contributing factors (7-day model)"),
        plotlyOutput("plot_importance", height = "260px")
      ),
      tabPanel(
        "What-if simulator",
        br(),
        p("Adjust inputs to simulate the patient's digital twin. The plot shows baseline vs. simulated fall risk."),
        fluidRow(
          column(4,
            sliderInput("wi_steps", "Change in daily steps (%)", -50, 100, 0, step = 5),
            sliderInput("wi_sbp", "Change in systolic BP (mmHg)", -30, 30, 0, step = 1),
            sliderInput("wi_sed", "Sedentary hours/day (override)", 6, 22, 15, step = 0.5),
            checkboxInput("wi_use_sed", "Apply sedentary override", FALSE),
            checkboxInput("wi_meds", "Simulate deprescribing (polypharmacy -> off)", FALSE)
          ),
          column(8,
            plotlyOutput("plot_whatif", height = "320px"),
            br(),
            uiOutput("whatif_summary")
          )
        )
      )
    )
  )
}

ui <- fluidPage(uiOutput("main_ui"))

# --- Server ----------------------------------------------------------------

server <- function(input, output, session) {
  auth <- reactiveVal(NULL)

  output$main_ui <- renderUI({
    if (is.null(auth())) login_ui() else dashboard_ui()
  })

  observeEvent(input$login_btn, {
    sess <- cdt_login(con, input$login_user, input$login_pass)
    if (is.null(sess)) {
      output$login_msg <- renderText("Invalid credentials.")
    } else {
      auth(sess)
    }
  })

  observeEvent(input$logout_btn, {
    if (!is.null(auth())) cdt_logout(con, auth()$token)
    auth(NULL)
  })

  output$who <- renderText({
    req(auth())
    paste0("Signed in \u00b7 role: ", auth()$role, "  ")
  })

  # Cohort snapshot (recomputed when logged in).
  cohort_snap <- reactive({
    req(auth())
    cdt_cohort_snapshot(con, model)
  })

  # Populate patient selector.
  observe({
    req(auth())
    snap <- cohort_snap()
    labels <- sprintf("%s (%s, age %d) - 7d %s",
      snap$patient_id, snap$sex, snap$age, snap$tier_7d)
    updateSelectInput(session, "sel_patient",
      choices = setNames(snap$patient_id, labels))
  })

  # --- Cohort table --------------------------------------------------------
  output$cohort_table <- renderDT({
    snap <- cohort_snap()
    if (input$filter_tier != "All") {
      snap <- snap[snap$tier_7d == input$filter_tier, ]
    }
    snap <- snap[order(-snap[[input$sort_by]]), ]
    disp <- snap %>%
      transmute(
        Patient = patient_id, Age = age, Sex = sex,
        `Prior falls` = prior_falls,
        `24h risk` = sprintf("%.1f%%", 100 * p_24h),
        `7d risk` = sprintf("%.1f%%", 100 * p_7d),
        Tier = tier_7d
      )
    datatable(disp, rownames = FALSE, selection = "single",
      options = list(pageLength = 15, order = list())) %>%
      formatStyle("Tier",
        backgroundColor = styleEqual(names(TIER_COLORS), unname(TIER_COLORS)),
        color = "white", fontWeight = "bold")
  })

  # Clicking a cohort row jumps to that patient's detail.
  observeEvent(input$cohort_table_rows_selected, {
    snap <- cohort_snap()
    if (input$filter_tier != "All") snap <- snap[snap$tier_7d == input$filter_tier, ]
    snap <- snap[order(-snap[[input$sort_by]]), ]
    idx <- input$cohort_table_rows_selected
    if (length(idx) == 1) {
      updateSelectInput(session, "sel_patient", selected = snap$patient_id[idx])
      updateTabsetPanel(session, "tabs", selected = "Patient detail")
    }
  })

  # --- Patient detail data -------------------------------------------------
  patient_readings <- reactive({
    req(input$sel_patient)
    r <- cdt_get_patient_timeline(con, input$sel_patient)
    r$ts <- as.Date(r$ts)
    r
  })

  patient_static <- reactive({
    req(input$sel_patient)
    cdt_get_patient(con, input$sel_patient)
  })

  baseline_features <- reactive({
    cdt_assemble_features(patient_static(), patient_readings())
  })

  output$risk_cards <- renderUI({
    req(input$sel_patient)
    r <- predict_fall_risk(model, baseline_features())
    card <- function(label, p, tier) {
      div(style = sprintf(
        "display:inline-block;margin:4px 8px;padding:10px 16px;border-radius:8px;background:%s;color:#fff;min-width:120px;text-align:center;",
        TIER_COLORS[[tier]]),
        div(style = "font-size:12px;", label),
        div(style = "font-size:22px;font-weight:700;", sprintf("%.1f%%", 100 * p)),
        div(style = "font-size:12px;", tier)
      )
    }
    tagList(
      card("24h fall risk", r$p_24h, r$tier_24h),
      card("7-day fall risk", r$p_7d, r$tier_7d)
    )
  })

  line_plot <- function(df, y, title, color, yaxis = "") {
    plot_ly(df, x = ~ts, y = as.formula(paste0("~", y)),
      type = "scatter", mode = "lines+markers",
      line = list(color = color), marker = list(size = 3, color = color)) %>%
      layout(title = list(text = title, font = list(size = 13)),
        xaxis = list(title = ""), yaxis = list(title = yaxis),
        margin = list(t = 30, b = 30))
  }

  output$plot_steps <- renderPlotly(
    line_plot(patient_readings(), "step_count", "Daily steps", "#1565c0", "steps"))
  output$plot_hr <- renderPlotly(
    line_plot(patient_readings(), "resting_hr", "Resting heart rate", "#c62828", "bpm"))
  output$plot_bp <- renderPlotly({
    df <- patient_readings()
    plot_ly(df, x = ~ts) %>%
      add_lines(y = ~sbp, name = "Systolic", line = list(color = "#6a1b9a")) %>%
      add_lines(y = ~dbp, name = "Diastolic", line = list(color = "#ab47bc")) %>%
      layout(title = list(text = "Blood pressure", font = list(size = 13)),
        xaxis = list(title = ""), yaxis = list(title = "mmHg"),
        margin = list(t = 30, b = 30))
  })
  output$plot_sedentary <- renderPlotly({
    df <- patient_readings()
    df$sedentary <- df$hours_sitting + df$hours_lying
    line_plot(df, "sedentary", "Sedentary time", "#455a64", "hours/day")
  })

  output$plot_importance <- renderPlotly({
    imp <- head(cdt_feature_importance(model, "7d"), 8)
    imp <- imp[order(imp$abs_coefficient), ]
    plot_ly(imp, x = ~abs_coefficient, y = ~factor(feature, levels = feature),
      type = "bar", orientation = "h",
      marker = list(color = ifelse(imp$coefficient > 0, "#c62828", "#2e7d32"))) %>%
      layout(xaxis = list(title = "|standardized coefficient|"),
        yaxis = list(title = ""), margin = list(l = 160, t = 10))
  })

  # --- What-if simulator ---------------------------------------------------
  whatif_overrides <- reactive({
    ov <- list()
    if (input$wi_steps != 0) ov$steps_pct <- input$wi_steps
    if (input$wi_sbp != 0) ov$sbp_delta <- input$wi_sbp
    if (isTRUE(input$wi_use_sed)) ov$sedentary_hours_mean_7d <- input$wi_sed
    if (isTRUE(input$wi_meds)) ov$polypharmacy <- 0
    if (length(ov) == 0) NULL else ov
  })

  whatif_result <- reactive({
    req(input$sel_patient)
    predict_fall_risk(model, baseline_features(),
      modified_inputs = whatif_overrides(), include_baseline = TRUE)
  })

  output$plot_whatif <- renderPlotly({
    r <- whatif_result()
    df <- data.frame(
      horizon = factor(rep(c("24h", "7-day"), 2), levels = c("24h", "7-day")),
      scenario = rep(c("Baseline", "Simulated twin"), each = 2),
      risk = c(r$baseline$p_24h, r$baseline$p_7d, r$p_24h, r$p_7d)
    )
    plot_ly(df, x = ~horizon, y = ~risk, color = ~scenario, type = "bar",
      colors = c("Baseline" = "#90a4ae", "Simulated twin" = "#1565c0")) %>%
      layout(barmode = "group", yaxis = list(title = "P(fall)", tickformat = ".0%"),
        xaxis = list(title = ""), title = list(text = "Baseline vs. simulated twin",
          font = list(size = 13)))
  })

  output$whatif_summary <- renderUI({
    r <- whatif_result()
    d7 <- 100 * r$delta$p_7d
    dir <- if (d7 < 0) "reduction" else "increase"
    col <- if (d7 <= 0) "#2e7d32" else "#c62828"
    div(style = sprintf("font-size:15px;color:%s;", col),
      sprintf("Simulated 7-day risk: %.1f%% (%s of %.1f points vs. baseline %.1f%%).",
        100 * r$p_7d, dir, abs(d7), 100 * r$baseline$p_7d))
  })

  session$onSessionEnded(function() {
    # Connection is process-shared; do not disconnect per session.
  })
}

shinyApp(ui, server)
