# --- 1. Automatic Package Check & Installation ---
required_pkgs <- c("shiny", "qrcode", "magick", "base64enc")
installed <- required_pkgs %in% installed.packages()[, "Package"]
if (any(!installed)) {
  install.packages(required_pkgs[!installed], repos = "https://cloud.r-project.org")
}

library(shiny)
library(qrcode)
library(magick)
library(base64enc)

# --- 2. User Interface ---
ui <- fluidPage(
  titlePanel("SRFQRSight"),
  sidebarLayout(
    sidebarPanel(
      # Form Inputs
      textInput("srf_num", "SRF Number:", value = "SRF-2024-001"),
      textInput("student_name", "Student Name:", value = "Jane Doe"),
      textInput("guide", "Guide:", value = "Dr. Alan Turing"),
      textInput("co_guide", "Co-guide:", value = "Dr. Ada Lovelace"),
      dateInput("comp_date", "Completion Date:", value = Sys.Date(), format = "yyyy-mm-dd"),
      
      hr(),
      
      # Customization Options
      radioButtons(
        "bg_color", 
        "Background Color:", 
        choices = c("White" = "white", "Transparent" = "transparent"), 
        selected = "white"
      ),
      fileInput("logo_file", "Upload Center Logo (Optional):", accept = c("image/png", "image/jpeg")),
      
      hr(),
      
      # Action Button
      actionButton("generate_btn", "Generate QR Code", class = "btn-success", style = "width: 100%; font-weight: bold; margin-bottom: 10px;"),
      
      # Download Button
      downloadButton("download_qr", "Download QR Code (PNG)", class = "btn-primary", style = "width: 100%;")
    ),
    
    mainPanel(
      h4("Generated QR Code"),
      uiOutput("qr_display_container"),
      br(),
      uiOutput("metadata_display"),
      
      # App Credit Section
      tags$div(
        style = "max-width: 450px; margin-top: 20px; padding: 12px 15px; background-color: #f1f3f5; border-left: 4px solid #0d6efd; border-radius: 4px; font-size: 0.88em; color: #495057;",
        tags$p(
          style = "margin: 0; line-height: 1.5;",
          tags$strong("Prepared by: "), "Dr. Himel Mondal", tags$br(),
          "Department of Physiology, AIIMS Deoghar, Jharkhand, India", tags$br(),
          tags$strong("Email: "), tags$a(href = "mailto:himel.physiology@aiimsdeoghar.edu.in", "himel.physiology@aiimsdeoghar.edu.in")
        )
      )
    )
  )
)

# --- 3. Server Logic ---
server <- function(input, output, session) {
  
  # Trigger only on initial load and when "Generate QR Code" is clicked
  submitted_data <- eventReactive(input$generate_btn, {
    list(
      srf_num = trimws(input$srf_num),
      student_name = trimws(input$student_name),
      guide = trimws(input$guide),
      co_guide = trimws(input$co_guide),
      comp_date = format(input$comp_date, "%Y-%m-%d"),
      bg_color = input$bg_color,
      logo_path = if (!is.null(input$logo_file)) input$logo_file$datapath else NULL
    )
  }, ignoreNULL = FALSE)
  
  # Formatted text string encoded in the QR code
  qr_payload <- reactive({
    data <- submitted_data()
    paste(
      paste0("SRF Number: ", data$srf_num),
      paste0("Student Name: ", data$student_name),
      paste0("Guide: ", data$guide),
      paste0("Co-guide: ", data$co_guide),
      paste0("Completion Date: ", data$comp_date),
      sep = "\n"
    )
  })
  
  # Build and return the final magick image object
  generate_qr_magick <- reactive({
    data <- submitted_data()
    req(qr_payload())
    
    # Generate high error-correction matrix
    qr_matrix <- qrcode::qr_code(qr_payload(), ecl = "H")
    
    # Safe temporary file write to avoid device errors
    tmp_png <- tempfile(fileext = ".png")
    on.exit(if (file.exists(tmp_png)) unlink(tmp_png), add = TRUE)
    
    bg_col <- if (data$bg_color == "transparent") "transparent" else "white"
    
    # Open standard PNG device with a valid string filename
    png(filename = tmp_png, width = 600, height = 600, bg = bg_col)
    par(mar = c(0, 0, 0, 0))
    image(
      t(qr_matrix[nrow(qr_matrix):1, ]), 
      col = c(bg_col, "black"), 
      axes = FALSE
    )
    dev.off()
    
    # Read the generated PNG into magick
    base_img <- magick::image_read(tmp_png)
    
    # Overlay logo if uploaded
    if (!is.null(data$logo_path) && file.exists(data$logo_path)) {
      tryCatch({
        logo <- magick::image_read(data$logo_path)
        logo_scaled <- magick::image_resize(logo, "130x130^")
        logo_scaled <- magick::image_extent(logo_scaled, "130x130", gravity = "Center")
        base_img <- magick::image_composite(base_img, logo_scaled, operator = "Over", gravity = "Center")
      }, error = function(e) {
        warning("Logo overlay bypassed: ", e$message)
      })
    }
    
    base_img
  })
  
  # Preview Container
  output$qr_display_container <- renderUI({
    data <- submitted_data()
    
    bg_style <- if (data$bg_color == "transparent") {
      "background: repeating-conic-gradient(#e0e0e0 0% 25%, #ffffff 0% 50%) 50% / 20px 20px; display: inline-block; padding: 15px; border: 1px solid #ccc; border-radius: 8px;"
    } else {
      "background: #ffffff; display: inline-block; padding: 15px; border: 1px solid #ccc; border-radius: 8px;"
    }
    
    img <- generate_qr_magick()
    raw_bytes <- magick::image_write(img, format = "png")
    b64_string <- base64enc::base64encode(raw_bytes)
    src_data <- paste0("data:image/png;base64,", b64_string)
    
    tags$div(
      style = bg_style,
      tags$img(src = src_data, width = "280px", height = "280px", alt = "SRFQRSight QR Code")
    )
  })
  
  # Metadata Display
  output$metadata_display <- renderUI({
    data <- submitted_data()
    
    tags$div(
      style = "max-width: 450px; background-color: #f8f9fa; border: 1px solid #e9ecef; border-radius: 8px; padding: 15px; margin-top: 10px;",
      tags$h5(style = "margin-top: 0; font-weight: bold; border-bottom: 2px solid #dee2e6; padding-bottom: 5px;", "Entered Details"),
      tags$table(
        class = "table table-sm table-borderless",
        style = "margin-bottom: 0;",
        tags$tbody(
          tags$tr(tags$td(tags$strong("SRF Number:")), tags$td(data$srf_num)),
          tags$tr(tags$td(tags$strong("Student Name:")), tags$td(data$student_name)),
          tags$tr(tags$td(tags$strong("Guide:")), tags$td(data$guide)),
          tags$tr(tags$td(tags$strong("Co-guide:")), tags$td(data$co_guide)),
          tags$tr(tags$td(tags$strong("Completion Date:")), tags$td(data$comp_date))
        )
      )
    )
  })
  
  # Download Handler
  output$download_qr <- downloadHandler(
    filename = function() {
      clean_srf <- gsub("[^A-Za-z0-9_-]", "_", submitted_data()$srf_num)
      if (clean_srf == "") clean_srf <- "QR"
      paste0("SRFQRSight_", clean_srf, ".png")
    },
    content = function(file) {
      img <- generate_qr_magick()
      magick::image_write(img, path = file, format = "png")
    },
    contentType = "image/png"
  )
}

# --- 4. Launch App ---
shinyApp(ui = ui, server = server)