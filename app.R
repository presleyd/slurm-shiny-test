library(shiny)
library(processx)
library(stringr)

ui <- fluidPage(
  titlePanel("Slurm Job Launcher"),
  sidebarLayout(
    sidebarPanel(
      actionButton("submit", "Submit Slurm Job")
    ),
    mainPanel(
      verbatimTextOutput("job_id"),
      verbatimTextOutput("job_status"),
      verbatimTextOutput("job_node")
    )
  )
)

server <- function(input, output, session) {
  job_info <- reactiveValues(id = NULL, status = NULL, node = NULL)
  
  observeEvent(input$submit, {
    # Write SLURM script to a temp file
    slurm_script <- tempfile(fileext = ".sh")
    writeLines(c(
      "#!/bin/bash",
      "#SBATCH --job-name=shinyjob",
      "#SBATCH --output=/tmp/shinyjob-%j.out",
      "#SBATCH --time=00:01:00",
      "#SBATCH --partition=short",
      "hostname",
      "sleep 30"
    ), con = slurm_script)
    
    # Submit the job
    res <- run("sbatch", slurm_script, echo = TRUE, error_on_status = FALSE)
    output_line <- res$stdout
    match <- str_match(output_line, "Submitted batch job ([0-9]+)")
    if (!is.na(match[2])) {
      job_info$id <- match[2]
      job_info$status <- "PENDING"
      job_info$node <- "N/A"
    }
  })
  
  observe({
    req(job_info$id)
    invalidateLater(3000, session)
    
    res <- run("squeue", c("-j", job_info$id, "-o", "%T|%N", "--noheader"), error_on_status = FALSE)
    line <- str_trim(res$stdout)
    
    if (line == "") {
      job_info$status <- "COMPLETED or not found"
      job_info$node <- "N/A"
    } else {
      parts <- str_split_fixed(line, "\\|", 2)
      job_info$status <- parts[1]
      job_info$node <- parts[2]
    }
  })
  
  output$job_id <- renderText({
    req(job_info$id)
    paste("Job ID:", job_info$id)
  })
  
  output$job_status <- renderText({
    req(job_info$status)
    paste("Status:", job_info$status)
  })
  
  output$job_node <- renderText({
    req(job_info$node)
    paste("Node:", job_info$node)
  })
}

shinyApp(ui = ui, server = server)