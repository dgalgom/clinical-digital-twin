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
library(bslib)
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

# Load secrets/overrides from a git-ignored .env (or .Renviron) if present.
# Non-destructive: existing shell/CI vars win; empty placeholders are skipped,
# so the Claude/Telegram clients keep their auto-mock fallback when no key is
# supplied. Values are never printed.
cdt_load_env()

# Shared resources.
con <- cdt_db_connect()
# Ensure newer tables (e.g. interventions) exist on databases built before they
# were added. Idempotent: CREATE TABLE IF NOT EXISTS leaves existing data intact.
cdt_db_init_schema(con)
model <- cdt_load_model()

# Risk-tier colors (clinically legible; no chart-junk).
TIER_COLORS <- c(Low = "#2e7d32", Moderate = "#f9a825", High = "#c62828")

# Dark palette (shared by CSS + plot templates).
CDT_BG <- "#0e1116"       # page background
CDT_PANEL <- "#161b22"    # cards / plot panels
CDT_FG <- "#e6edf3"       # primary text
CDT_GRID <- "#30363d"     # borders / gridlines
CDT_ACCENT <- "#3d8bfd"   # primary accent

# Professional dark theme (Bootstrap 5 via bslib). If the "darkly" bootswatch
# is unavailable on the installed bslib, fall back to a plain dark base_theme.
cdt_theme <- tryCatch(
  bslib::bs_theme(
    version = 5, bootswatch = "darkly",
    bg = CDT_BG, fg = CDT_FG, primary = CDT_ACCENT
  ),
  error = function(e) bslib::bs_theme(
    version = 5, bg = CDT_BG, fg = CDT_FG, primary = CDT_ACCENT
  )
)

# Apply the dark template to a plotly object (bg/font/grid only; traces/data
# and tier colors are left untouched to preserve clinical legibility).
dark_layout <- function(p) {
  ax <- list(gridcolor = CDT_GRID, zerolinecolor = CDT_GRID,
    linecolor = CDT_GRID, tickfont = list(color = CDT_FG),
    titlefont = list(color = CDT_FG))
  plotly::layout(p,
    paper_bgcolor = CDT_PANEL, plot_bgcolor = CDT_PANEL,
    font = list(color = CDT_FG),
    legend = list(font = list(color = CDT_FG)),
    xaxis = ax, yaxis = ax)
}

# --- UI --------------------------------------------------------------------

login_ui <- function() {
  div(
    style = sprintf(
      paste0("max-width:380px;margin:80px auto;padding:28px;",
        "background:%s;border:1px solid %s;border-radius:10px;",
        "box-shadow:0 2px 16px rgba(0,0,0,0.45);"),
      CDT_PANEL, CDT_GRID),
    h3("Clinical Digital Twin", style = sprintf("color:%s;", CDT_FG)),
    p(style = "color:#8b98a5;font-size:13px;",
      "Synthetic-data prototype. Not for clinical use."),
    textInput("login_user", "Username", value = "clinician"),
    passwordInput("login_pass", "Password", value = ""),
    actionButton("login_btn", "Log in", class = "btn-primary"),
    br(), br(),
    div(style = "color:#ff6b6b;", textOutput("login_msg"))
  )
}

dashboard_ui <- function() {
  fluidPage(
    tags$head(tags$style(HTML(sprintf("
      .risk-badge{padding:2px 8px;border-radius:10px;color:#fff;font-weight:600;}
      .banner{background:#3a2f00;border:1px solid #8a6d00;color:#ffd873;
              padding:6px 12px;border-radius:6px;font-size:12px;margin-bottom:10px;}
      .dataTables_wrapper{color:%s;}
      table.dataTable tbody tr{background-color:%s;}
      table.dataTable{border-color:%s;}
    ", CDT_FG, CDT_PANEL, CDT_GRID)))),
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
        "Shift triage",
        br(),
        p(paste("Who changed since the last snapshot? This surfaces movement",
          "(risk jumps and tier crossings), not the same frail patients every",
          "day - the question a nurse actually asks at handover.")),
        fluidRow(
          column(5, actionButton("refresh_alerts",
            "Re-snapshot & detect changes", class = "btn-primary")),
          column(7, div(style = "padding-top:6px;color:#8b98a5;",
            textOutput("alerts_status", inline = TRUE)))
        ),
        br(),
        uiOutput("alerts_list")
      ),
      tabPanel(
        "Post-fall huddles",
        br(),
        p(paste("Every fall should trigger a structured huddle. Falls below are",
          "awaiting one. Draft grounds on the 72h of pre-fall sensor data; the",
          "clinician reviews, edits, and saves - the AI never writes the record.")),
        fluidRow(
          column(5, actionButton("refresh_huddles",
            "Refresh open huddles", class = "btn-primary")),
          column(7, div(style = "padding-top:6px;color:#8b98a5;",
            textOutput("huddles_status", inline = TRUE)))
        ),
        br(),
        uiOutput("huddles_list")
      ),
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
        plotlyOutput("plot_importance", height = "260px"),
        br(),
        h4("Suggested interventions"),
        uiOutput("driver_interventions"),
        br(),
        h4("Log an intervention"),
        fluidRow(
          column(4, selectInput("iv_type", "Type", choices = c(
            "Physiotherapy / mobility referral",
            "Medication review / deprescribing",
            "BP medication review",
            "Toileting schedule / bed-exit precautions",
            "Increased observation / rounding",
            "Environmental de-hazarding",
            "Post-fall huddle follow-up",
            "Other"
          ))),
          column(6, textInput("iv_detail", "Detail (optional)",
            placeholder = "e.g. referred to PT; review benzodiazepine")),
          column(2, br(), actionButton("iv_log_btn", "Log", class = "btn-primary"))
        ),
        div(style = "color:#7bd88f;", textOutput("iv_log_msg")),
        br(),
        h4("Logged interventions"),
        uiOutput("intervention_log")
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
            uiOutput("whatif_summary"),
            br(),
            actionButton("wi_log_btn",
              "Log this scenario as a planned intervention",
              class = "btn-outline-primary"),
            div(style = "color:#7bd88f;", textOutput("wi_log_msg"))
          )
        )
      ),
      tabPanel(
        "Patient data",
        br(),
        p(style = "color:#8b98a5;",
          "Full synthetic cohort - not real patients. Search, sort, and export below."),
        DTOutput("patient_data_table")
      )
    )
  )
}

ui <- fluidPage(theme = cdt_theme, uiOutput("main_ui"))

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

  # --- Shift triage (P0-1) -------------------------------------------------
  # `alerts_refresh` is bumped after a re-snapshot or an acknowledgement so the
  # alert list re-renders. Colours mirror severity, not absolute risk.
  alerts_refresh <- reactiveVal(0)
  SEVERITY_COLORS <- c(info = "#3d8bfd", warning = "#f9a825", critical = "#c62828")

  open_alerts <- reactive({
    req(auth())
    alerts_refresh()
    cdt_get_alerts(con, only_open = TRUE)
  })

  # Clinician-initiated: snapshot current risk and detect movement vs. the last
  # stored snapshot. Never runs automatically (a button, like the intervention
  # log) so the demo stays deterministic and the action is a human decision.
  observeEvent(input$refresh_alerts, {
    req(auth())
    fired <- tryCatch(
      cdt_compute_alerts(con, model, as_of = as.character(Sys.time())),
      error = function(e) NULL)
    alerts_refresh(alerts_refresh() + 1)
    output$alerts_status <- renderText({
      if (is.null(fired)) "Could not compute alerts." else
        sprintf("Snapshot taken. %d new change alert(s) detected.", nrow(fired))
    })
  })

  observeEvent(input$ack_alert, {
    req(auth())
    cdt_ack_alert(con, as.integer(input$ack_alert),
      acknowledged_by = auth()$username %||% "clinician")
    alerts_refresh(alerts_refresh() + 1)
  })

  output$alerts_list <- renderUI({
    req(auth())
    al <- open_alerts()
    if (nrow(al) == 0) {
      return(p(style = "color:#8b98a5;",
        paste("No open change alerts. Click \u201cRe-snapshot & detect",
          "changes\u201d to compare current risk against the last snapshot.")))
    }
    # Acknowledge buttons post their alert_id back via a tiny JS shim so a single
    # observer handles all rows.
    ack_js <- "Shiny.setInputValue('ack_alert', %d, {priority:'event'});"
    cards <- lapply(seq_len(nrow(al)), function(i) {
      col <- SEVERITY_COLORS[[al$severity[i]]] %||% "#8b98a5"
      div(style = sprintf(
        "margin:6px 0;padding:10px 14px;border-left:4px solid %s;background:%s;border-radius:6px;",
        col, CDT_PANEL),
        fluidRow(
          column(9,
            div(style = "font-weight:600;",
              al$patient_id[i], "  ",
              span(style = sprintf(
                "font-size:11px;color:%s;text-transform:uppercase;", col),
                al$severity[i])),
            div(style = "font-size:13px;color:#c9d1d9;", al$reason_text[i])),
          column(3, div(style = "text-align:right;",
            tags$button(class = "btn btn-sm btn-outline-light",
              onclick = sprintf(ack_js, al$alert_id[i]), "Acknowledge")))
        )
      )
    })
    tagList(
      p(style = "color:#8b98a5;font-size:12px;",
        sprintf("%d open alert(s), most recent first.", nrow(al))),
      div(cards)
    )
  })

  # --- Post-fall huddles (P0-4) --------------------------------------------
  # Falls awaiting a structured huddle. The AI drafts (grounded on 72h pre-fall
  # sensor data); the clinician edits and saves. The draft NEVER writes the
  # record - only the explicit Save action does, via cdt_complete_huddle().
  huddles_refresh <- reactiveVal(0)

  open_huddles <- reactive({
    req(auth())
    huddles_refresh()
    cdt_get_open_huddles(con)
  })

  observeEvent(input$refresh_huddles, {
    req(auth())
    huddles_refresh(huddles_refresh() + 1)
    output$huddles_status <- renderText({
      sprintf("%d fall(s) awaiting a huddle.", nrow(open_huddles()))
    })
  })

  output$huddles_list <- renderUI({
    req(auth())
    oh <- open_huddles()
    if (nrow(oh) == 0) {
      return(p(style = "color:#8b98a5;",
        "No open post-fall huddles. Every recorded fall has been actioned."))
    }
    # Each row offers a "Start huddle" button that posts the event_id back.
    start_js <- "Shiny.setInputValue('start_huddle', %d, {priority:'event'});"
    cards <- lapply(seq_len(nrow(oh)), function(i) {
      div(style = sprintf(
        "margin:6px 0;padding:10px 14px;border-left:4px solid %s;background:%s;border-radius:6px;",
        "#f9a825", CDT_PANEL),
        fluidRow(
          column(9,
            div(style = "font-weight:600;",
              oh$patient_id[i], "  ",
              span(style = "font-size:11px;color:#f9a825;text-transform:uppercase;",
                paste0("fall ", substr(as.character(oh$ts[i]), 1, 10)))),
            div(style = "font-size:13px;color:#c9d1d9;",
              sprintf("Recorded severity: %s. Needs a post-fall huddle.",
                oh$severity[i] %||% "unspecified"))),
          column(3, div(style = "text-align:right;",
            tags$button(class = "btn btn-sm btn-outline-light",
              onclick = sprintf(start_js, oh$event_id[i]), "Start huddle")))
        )
      )
    })
    tagList(
      p(style = "color:#8b98a5;font-size:12px;",
        sprintf("%d open huddle(s), most recent fall first.", nrow(oh))),
      div(cards)
    )
  })

  # Start a huddle: draft (grounded, mock-safe) and open an editable modal.
  observeEvent(input$start_huddle, {
    req(auth())
    eid <- as.integer(input$start_huddle)
    fall <- cdt_get_fall_event(con, eid)
    if (nrow(fall) == 0) return(NULL)
    draft <- tryCatch(cdt_draft_huddle_summary(con, model, eid),
      error = function(e) NULL)
    if (is.null(draft)) draft <- ""
    showModal(modalDialog(
      title = sprintf("Post-fall huddle - %s (fall %s)",
        fall$patient_id[1], substr(as.character(fall$ts[1]), 1, 10)),
      size = "l", easyClose = FALSE,
      p(style = "color:#8b98a5;font-size:12px;",
        paste("AI draft grounded on the 72h of pre-fall sensor data. Review and",
          "edit every field before saving - this becomes the clinical record.")),
      fluidRow(
        column(6, textInput("hd_location", "Location", placeholder = "e.g. bathroom")),
        column(6, textInput("hd_activity", "Activity at fall",
          placeholder = "e.g. transferring from bed"))
      ),
      fluidRow(
        column(6, selectInput("hd_injury", "Injury level",
          choices = c("None", "Minor", "Moderate", "Severe"), selected = "None")),
        column(6, textInput("hd_factors", "Contributing factors",
          placeholder = "e.g. orthostatic BP, sedating meds"))
      ),
      textAreaInput("hd_summary", "Huddle summary (AI draft - editable)",
        value = draft, width = "100%", height = "180px"),
      textAreaInput("hd_plan", "Plan / precautions", width = "100%", height = "80px",
        placeholder = "e.g. hourly rounding, med review, bed-exit alarm"),
      tags$input(type = "hidden", id = "hd_event_id", value = eid),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_huddle", "Save huddle", class = "btn-primary")
      )
    ))
    # Keep the event id server-side too (hidden input is display-only).
    active_huddle_id(eid)
  })

  active_huddle_id <- reactiveVal(NULL)

  observeEvent(input$save_huddle, {
    req(auth())
    eid <- active_huddle_id()
    if (is.null(eid)) {
      removeModal()
      return(NULL)
    }
    cdt_complete_huddle(con, eid,
      fields = list(
        location = input$hd_location,
        activity_at_fall = input$hd_activity,
        injury_level = input$hd_injury,
        contributing_factors = input$hd_factors,
        plan = input$hd_plan,
        huddle_summary = input$hd_summary
      ),
      completed_by = auth()$username %||% auth()$role %||% "clinician")
    active_huddle_id(NULL)
    removeModal()
    huddles_refresh(huddles_refresh() + 1)
    output$huddles_status <- renderText({
      sprintf("Huddle saved. %d fall(s) still awaiting a huddle.",
        nrow(open_huddles()))
    })
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

  # --- Patient data tab (full synthetic cohort) ----------------------------
  yn <- function(x) ifelse(as.integer(x) == 1L, "Yes", "No")

  output$patient_data_table <- renderDT({
    snap <- cohort_snap()
    if (nrow(snap) == 0) {
      return(datatable(
        data.frame(Message = "No patients in the (synthetic) database."),
        rownames = FALSE, options = list(dom = "t")))
    }
    disp <- snap %>%
      transmute(
        Patient = patient_id, Name = name, Age = age, Sex = sex,
        Comorbidities = comorbidities,
        Parkinsons = yn(parkinsons), Osteoporosis = yn(osteoporosis),
        `Orthostatic hypotension` = yn(orthostatic_hypotension),
        Polypharmacy = yn(polypharmacy),
        `Prior falls` = prior_falls, `# Meds` = n_medications,
        Medications = medications,
        `24h risk` = sprintf("%.1f%%", 100 * p_24h),
        `7d risk` = sprintf("%.1f%%", 100 * p_7d),
        Tier = tier_7d
      )
    datatable(disp, rownames = FALSE, filter = "top",
      extensions = "Buttons",
      options = list(
        pageLength = 15, dom = "Bfrtip",
        buttons = c("copy", "csv", "excel"),
        scrollX = TRUE)) %>%
      formatStyle("Tier",
        backgroundColor = styleEqual(names(TIER_COLORS), unname(TIER_COLORS)),
        color = "white", fontWeight = "bold")
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

  # Logged interventions for the selected patient (P0-3). `iv_refresh` is bumped
  # after every successful log so dependent outputs (list + plot overlay) update.
  iv_refresh <- reactiveVal(0)
  patient_interventions <- reactive({
    req(input$sel_patient)
    iv_refresh()
    iv <- cdt_get_interventions(con, input$sel_patient)
    if (nrow(iv) > 0) iv$date <- as.Date(substr(iv$created_at, 1, 10))
    iv
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
      layout(title = list(text = title, font = list(size = 13, color = CDT_FG)),
        xaxis = list(title = ""), yaxis = list(title = yaxis),
        margin = list(t = 30, b = 30)) %>%
      dark_layout()
  }

  # Overlay logged-intervention markers as vertical lines on a time-series plot.
  add_intervention_markers <- function(p, iv, df, ycol) {
    if (nrow(iv) == 0) return(p)
    ymax <- suppressWarnings(max(df[[ycol]], na.rm = TRUE))
    if (!is.finite(ymax)) return(p)
    for (i in seq_len(nrow(iv))) {
      p <- p %>% add_segments(
        x = iv$date[i], xend = iv$date[i], y = 0, yend = ymax,
        line = list(color = "#3d8bfd", width = 1, dash = "dot"),
        hoverinfo = "text",
        text = paste0(iv$type[i],
          if (!is.na(iv$detail[i]) && nzchar(iv$detail[i])) paste0(": ", iv$detail[i]) else ""),
        showlegend = FALSE, inherit = FALSE)
    }
    p
  }

  output$plot_steps <- renderPlotly({
    df <- patient_readings()
    p <- line_plot(df, "step_count", "Daily steps (dotted = logged intervention)",
      "#1565c0", "steps")
    add_intervention_markers(p, patient_interventions(), df, "step_count")
  })
  output$plot_hr <- renderPlotly(
    line_plot(patient_readings(), "resting_hr", "Resting heart rate", "#c62828", "bpm"))
  output$plot_bp <- renderPlotly({
    df <- patient_readings()
    plot_ly(df, x = ~ts) %>%
      add_lines(y = ~sbp, name = "Systolic", line = list(color = "#b085f5")) %>%
      add_lines(y = ~dbp, name = "Diastolic", line = list(color = "#ce93d8")) %>%
      layout(title = list(text = "Blood pressure", font = list(size = 13, color = CDT_FG)),
        xaxis = list(title = ""), yaxis = list(title = "mmHg"),
        margin = list(t = 30, b = 30)) %>%
      dark_layout()
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
        yaxis = list(title = ""), margin = list(l = 160, t = 10)) %>%
      dark_layout()
  })

  # Suggested interventions mapped to the top model drivers (P0-2).
  output$driver_interventions <- renderUI({
    req(input$sel_patient)
    di <- cdt_driver_interventions(model, top_n = 3L)
    if (nrow(di) == 0) {
      return(p(style = "color:#8b98a5;", "No drivers available."))
    }
    urgency_col <- c(routine = "#2e7d32", prompt = "#f9a825", urgent = "#c62828")
    cards <- lapply(seq_len(nrow(di)), function(i) {
      col <- urgency_col[[di$urgency[i]]] %||% "#8b98a5"
      div(style = sprintf(
        "margin:6px 0;padding:10px 14px;border-left:4px solid %s;background:%s;border-radius:6px;",
        col, CDT_PANEL),
        div(style = "font-weight:600;",
          sprintf("%s  ", di$label[i]),
          span(style = sprintf("font-size:11px;color:%s;text-transform:uppercase;", col),
            di$urgency[i])),
        tags$ul(style = "margin:6px 0 4px 0;",
          lapply(di$interventions[[i]], function(x) tags$li(x))),
        div(style = "font-size:12px;color:#8b98a5;", di$evidence_note[i])
      )
    })
    tagList(
      div(cards),
      p(style = "font-size:11px;color:#8b98a5;margin-top:8px;",
        "Illustrative decision-support on synthetic data - not clinical guidance.")
    )
  })

  # --- Intervention logging (P0-3 closed loop) -----------------------------
  # Logging is clinician-initiated (button); unrestricted for the MVP demo.
  observeEvent(input$iv_log_btn, {
    req(input$sel_patient, auth())
    type <- input$iv_type
    detail <- trimws(input$iv_detail %||% "")
    cdt_log_intervention(con, input$sel_patient, type,
      detail = if (nzchar(detail)) detail else NULL,
      created_by = auth()$username %||% "clinician")
    updateTextInput(session, "iv_detail", value = "")
    iv_refresh(iv_refresh() + 1)
    output$iv_log_msg <- renderText(sprintf("Logged: %s", type))
  })

  # Log the current what-if scenario as a planned intervention.
  observeEvent(input$wi_log_btn, {
    req(input$sel_patient, auth())
    ov <- whatif_overrides()
    if (is.null(ov)) {
      output$wi_log_msg <- renderText("Adjust a slider first, then log the scenario.")
      return(invisible(NULL))
    }
    detail <- jsonlite::toJSON(ov, auto_unbox = TRUE)
    cdt_log_intervention(con, input$sel_patient,
      type = "Planned what-if scenario", detail = as.character(detail),
      created_by = auth()$username %||% "clinician")
    iv_refresh(iv_refresh() + 1)
    output$wi_log_msg <- renderText("Scenario logged as a planned intervention.")
  })

  output$intervention_log <- renderUI({
    req(input$sel_patient)
    iv <- patient_interventions()
    if (nrow(iv) == 0) {
      return(p(style = "color:#8b98a5;", "No interventions logged yet."))
    }
    iv <- iv[order(iv$created_at, decreasing = TRUE), ]
    rows <- lapply(seq_len(nrow(iv)), function(i) {
      det <- iv$detail[i]
      div(style = sprintf(
        "margin:4px 0;padding:8px 12px;background:%s;border-left:3px solid %s;border-radius:6px;",
        CDT_PANEL, CDT_ACCENT),
        div(style = "font-weight:600;", iv$type[i],
          span(style = "font-weight:400;color:#8b98a5;font-size:12px;",
            sprintf("  \u00b7 %s \u00b7 %s", iv$date[i], iv$created_by[i] %||% ""))),
        if (!is.na(det) && nzchar(det)) {
          div(style = "font-size:12px;color:#c9d1d9;", det)
        }
      )
    })
    div(rows)
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
      colors = c("Baseline" = "#90a4ae", "Simulated twin" = "#3d8bfd")) %>%
      layout(barmode = "group", yaxis = list(title = "P(fall)", tickformat = ".0%"),
        xaxis = list(title = ""), title = list(text = "Baseline vs. simulated twin",
          font = list(size = 13, color = CDT_FG))) %>%
      dark_layout()
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
