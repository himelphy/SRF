# =============================================================
# SRFSight: Balanced & Conflict-Free Evaluator Allocation Engine
# =============================================================

# --- 1. Dependencies & Safe Package Check ---
# Checks for missing packages and installs only if missing before loading
required_packages <- c("shiny", "readxl", "writexl", "tools")

missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(shiny)
  library(readxl)
  library(writexl)
  library(tools)
})

# --- 2. Algorithmic Helpers ---

#' Safe Sampling Wrapper
#' Prevents classic base::sample() index selection error when length(x) == 1
resample <- function(x, size, replace = FALSE) {
  if (length(x) == 0) return(character(0))
  if (length(x) == 1) {
    if (size > 1 && !replace) stop("Cannot sample more elements than exist in candidate set.")
    return(rep(x, size))
  }
  x[sample.int(length(x), size = size, replace = replace)]
}

#' Balanced Allocation Engine
assign_balanced <- function(proposals, evaluators, n_eval = 3, restriction_mode = "dept", seed = NULL) {
  if (!is.null(seed) && !is.na(seed)) {
    set.seed(as.integer(seed))
  }
  
  # Structural validation
  req_eval_cols <- c("evaluator_name", "department")
  if (!all(req_eval_cols %in% colnames(evaluators))) {
    stop("Evaluators file must contain: 'evaluator_name' and 'department'")
  }
  
  req_prop_cols <- c("proposal_id", "guide", "guide_dept", "co_guide", "co_guide_dept")
  if (!all(req_prop_cols %in% colnames(proposals))) {
    stop("Proposals file must contain: 'proposal_id', 'guide', 'guide_dept', 'co_guide', and 'co_guide_dept'")
  }
  
  # Clean text fields
  evaluators$evaluator_name <- trimws(as.character(evaluators$evaluator_name))
  evaluators$department     <- trimws(as.character(evaluators$department))
  
  if (any(duplicated(evaluators$evaluator_name))) {
    stop("Duplicate names detected in Evaluator roster. Each evaluator name must be unique.")
  }
  
  evaluator_pool <- evaluators$evaluator_name
  workload <- setNames(rep(0L, length(evaluator_pool)), evaluator_pool)
  eval_dept_map <- setNames(tolower(evaluators$department), evaluators$evaluator_name)
  
  # Shuffle proposals to eliminate sequential assignment bias
  shuffled_indices <- sample(seq_len(nrow(proposals)))
  long_list <- vector("list", nrow(proposals))
  
  for (idx in seq_along(shuffled_indices)) {
    i <- shuffled_indices[idx]
    prop_id <- trimws(as.character(proposals$proposal_id[i]))
    g       <- trimws(as.character(proposals$guide[i]))
    cg      <- trimws(as.character(proposals$co_guide[i]))
    g_d     <- tolower(trimws(as.character(proposals$guide_dept[i])))
    cg_d    <- tolower(trimws(as.character(proposals$co_guide_dept[i])))
    
    blocked_evaluators <- character(0)
    
    if (restriction_mode == "mentor") {
      mentors <- c(g, cg)
      blocked_evaluators <- mentors[!is.na(mentors) & mentors != ""]
    } else if (restriction_mode == "dept") {
      mentors <- c(g, cg)
      blocked_mentors <- mentors[!is.na(mentors) & mentors != ""]
      depts <- c(g_d, cg_d)
      blocked_depts <- depts[!is.na(depts) & depts != ""]
      dept_blocked <- names(eval_dept_map)[eval_dept_map %in% blocked_depts]
      blocked_evaluators <- unique(c(blocked_mentors, dept_blocked))
    }
    
    eligible <- setdiff(names(workload), blocked_evaluators)
    
    if (length(eligible) < n_eval) {
      stop(sprintf(
        "Proposal '%s' requires %d evaluators, but only %d are eligible under rule '%s'.",
        prop_id, n_eval, length(eligible), restriction_mode
      ))
    }
    
    # Stratified minimal-workload selection across tiers
    chosen <- character(0)
    needed <- n_eval
    curr_eligible <- eligible
    
    while (needed > 0) {
      wl_sub <- workload[curr_eligible]
      min_val <- min(wl_sub)
      min_candidates <- curr_eligible[wl_sub == min_val]
      
      n_take <- min(needed, length(min_candidates))
      picked <- resample(min_candidates, n_take)
      
      chosen <- c(chosen, picked)
      needed <- needed - length(picked)
      curr_eligible <- setdiff(curr_eligible, picked)
    }
    
    workload[chosen] <- workload[chosen] + 1L
    
    long_list[[idx]] <- data.frame(
      evaluator_name = chosen,
      proposal_id    = prop_id,
      stringsAsFactors = FALSE
    )
  }
  
  long <- do.call(rbind, long_list)
  
  # Aggregate assignments
  final <- aggregate(
    proposal_id ~ evaluator_name,
    data = long,
    FUN = function(x) paste(sort(x), collapse = ", ")
  )
  colnames(final)[2] <- "assigned_proposals"
  
  # Retain evaluators with zero assignments
  counts <- as.data.frame(table(factor(long$evaluator_name, levels = evaluator_pool)))
  colnames(counts) <- c("evaluator_name", "total_assigned")
  
  final <- merge(evaluators[, c("evaluator_name", "department")], final, by = "evaluator_name", all.x = TRUE)
  final <- merge(final, counts, by = "evaluator_name", all.x = TRUE)
  final$assigned_proposals[is.na(final$assigned_proposals)] <- "None"
  final$total_assigned[is.na(final$total_assigned)] <- 0
  
  final <- final[, c("evaluator_name", "department", "assigned_proposals", "total_assigned")]
  final[order(-final$total_assigned, final$evaluator_name), ]
}

# --- 3. User Interface ---
ui <- fluidPage(
  titlePanel("SRFSight"),
  
  tabsetPanel(
    tabPanel(
      "Randomization",
      sidebarLayout(
        sidebarPanel(
          h4("1. Download Templates"),
          downloadButton("eval_template", "Evaluator Template (.xlsx)"),
          br(), br(),
          downloadButton("prop_template", "Proposal Template (.xlsx)"),
          hr(),
          
          h4("2. Upload Data"),
          fileInput("eval_file", "Upload Evaluators (.xlsx or .csv)", accept = c(".xlsx", ".xls", ".csv")),
          fileInput("prop_file", "Upload Proposals (.xlsx or .csv)", accept = c(".xlsx", ".xls", ".csv")),
          hr(),
          
          h4("3. Configuration"),
          numericInput("n_eval", "Evaluators per Proposal:", value = 3, min = 1, step = 1),
          radioButtons(
            "restriction_mode",
            "Conflict of Interest (COI) Rule:",
            choices = list(
              "Randomize only (no restrictions)" = "none",
              "Exclude guide & co-guide" = "mentor",
              "Exclude guide, co-guide, & departmental faculty" = "dept"
            ),
            selected = "dept"
          ),
          numericInput("seed", "Random Seed (for auditing / reproducibility):", value = 101, min = 1, step = 1),
          hr(),
          
          actionButton("run", "Generate Allocation", class = "btn-primary", style = "width: 100%; font-weight: bold;"),
          br(), br(),
          downloadButton("download", "Download Allocation (.xlsx)", style = "width: 100%;")
        ),
        
        mainPanel(
          h4("Evaluator-wise Balanced Allocation"),
          tableOutput("result")
        )
      )
    ),
    
    tabPanel(
      "Department Summary",
      fluidRow(
        column(
          10,
          h3("Proposals Distribution by Department"),
          p("Upload the proposal file to view departmental representation."),
          hr(),
          tableOutput("dept_summary_table")
        )
      )
    ),
    
    tabPanel(
      "Instructions",
      fluidRow(
        column(
          9,
          h3("Operating Guidelines"),
          tags$ol(
            tags$li("Download the standard Excel templates from the sidebar."),
            tags$li("Populate the evaluator and proposal details and upload them."),
            tags$li("Review the ", strong("Department Summary"), " tab to verify representation."),
            tags$li("Select your institutional Conflict of Interest (COI) threshold."),
            tags$li("Specify the audit seed to ensure deterministic reproducibility across runs."),
            tags$li("Click ", strong("Generate Allocation"), " and export the workbook.")
          )
        )
      )
    ),
    
    tabPanel(
      "Credits",
      fluidRow(
        column(
          8,
          h3("Application Attribution"),
          tags$div(
            style = "padding: 15px; background-color: #f8f9fa; border-left: 4px solid #0d6efd; border-radius: 4px;",
            tags$p(tags$strong("Prepared by: "), "Dr. Himel Mondal"),
            tags$p(tags$strong("Affiliation: "), "Department of Physiology, AIIMS Deoghar, Jharkhand, India"),
            tags$p(tags$strong("Contact: "), tags$a(href = "mailto:himel.physiology@aiimsdeoghar.edu.in", "himel.physiology@aiimsdeoghar.edu.in")),
            tags$p("An open-source, reproducible algorithm for equitable and conflict-aware review allocation.")
          )
        )
      )
    )
  )
)

# --- 4. Server Engine ---
server <- function(input, output, session) {
  
  # Anonymized synthetic data for sample templates
  evaluators_sample <- data.frame(
    evaluator_name = paste("Faculty Member", LETTERS[1:10]),
    department = c(
      "Physiology", "Pediatrics", "Biochemistry", "Pathology",
      "Microbiology", "Nursing", "Pharmacology", "Obstetrics & Gynecology",
      "Community Medicine", "General Medicine"
    ),
    stringsAsFactors = FALSE
  )
  
  output$eval_template <- downloadHandler(
    filename = "evaluators_template.xlsx",
    content = function(file) {
      writexl::write_xlsx(evaluators_sample, path = file)
    }
  )
  
  output$prop_template <- downloadHandler(
    filename = "proposals_template.xlsx",
    content = function(file) {
      set.seed(42)
      n_sample <- 15
      prop_df <- data.frame(
        proposal_id   = sprintf("PROP_%03d", seq_len(n_sample)),
        guide         = sample(evaluators_sample$evaluator_name, n_sample, replace = TRUE),
        guide_dept    = sample(evaluators_sample$department, n_sample, replace = TRUE),
        co_guide      = sample(evaluators_sample$evaluator_name, n_sample, replace = TRUE),
        co_guide_dept = sample(evaluators_sample$department, n_sample, replace = TRUE),
        stringsAsFactors = FALSE
      )
      writexl::write_xlsx(prop_df, path = file)
    }
  )
  
  load_file <- function(file_info) {
    ext <- tools::file_ext(file_info$name)
    if (ext %in% c("xlsx", "xls")) {
      as.data.frame(readxl::read_excel(file_info$datapath))
    } else {
      read.csv(file_info$datapath, stringsAsFactors = FALSE)
    }
  }
  
  evaluators <- reactive({
    req(input$eval_file)
    load_file(input$eval_file)
  })
  
  proposals <- reactive({
    req(input$prop_file)
    load_file(input$prop_file)
  })
  
  output$dept_summary_table <- renderTable({
    req(proposals())
    df <- proposals()
    
    validate(
      need(all(c("guide_dept", "co_guide_dept") %in% colnames(df)),
           "Proposals file missing 'guide_dept' or 'co_guide_dept' columns.")
    )
    
    g_depts  <- trimws(as.character(df$guide_dept))
    cg_depts <- trimws(as.character(df$co_guide_dept))
    all_depts <- sort(unique(c(g_depts[!is.na(g_depts) & g_depts != ""], 
                               cg_depts[!is.na(cg_depts) & cg_depts != ""])))
    
    if (length(all_depts) == 0) return(data.frame(Message = "No department data found."))
    
    t_g  <- table(factor(g_depts, levels = all_depts))
    t_cg <- table(factor(cg_depts, levels = all_depts))
    
    unique_counts <- sapply(all_depts, function(d) {
      sum(df$guide_dept == d | df$co_guide_dept == d, na.rm = TRUE)
    })
    
    summary_df <- data.frame(
      department             = all_depts,
      proposals_as_guide     = as.integer(t_g),
      proposals_as_co_guide  = as.integer(t_cg),
      total_unique_proposals = as.integer(unique_counts),
      stringsAsFactors       = FALSE
    )
    summary_df[order(-summary_df$total_unique_proposals, summary_df$department), ]
  })
  
  allocation <- eventReactive(input$run, {
    validate(
      need(input$eval_file, "Please upload the Evaluators roster."),
      need(input$prop_file, "Please upload the Proposals data.")
    )
    
    tryCatch({
      assign_balanced(
        proposals        = proposals(),
        evaluators       = evaluators(),
        n_eval           = as.integer(input$n_eval),
        restriction_mode = input$restriction_mode,
        seed             = input$seed
      )
    }, error = function(e) {
      validate(need(FALSE, paste("Allocation Error:", e$message)))
    })
  })
  
  output$result <- renderTable({
    allocation()
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste0("srf_allocation_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(allocation(), path = file)
    }
  )
}

# --- 5. Application Launch ---
shinyApp(ui = ui, server = server) 