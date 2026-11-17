# Copyright duration checker (excluding Crown copyright)
# Based on The National Archives' flowchart:
# https://cdn.nationalarchives.gov.uk/documents/information-management/non-crown-copyright-flowchart.pdf
#
# Required packages: shiny, bslib
#   install.packages(c("shiny", "bslib"))

library(shiny)
library(bslib)

# ---------------------------------------------------------------------------
# Decision tree
# Each node is either a question (with yes/no destinations) or an outcome.
# To change the logic, edit only this section.
# ---------------------------------------------------------------------------
q <- function(id, text, yes, no) list(id = id, type = "question", text = text, yes = yes, no = no)
o <- function(id, text)          list(id = id, type = "outcome",  text = text)

nodes <- list(
  # --- Start ---
  q("start", "Is the author known?", yes = "k1", no = "u1"),
  
  # --- Author UNKNOWN (red branch in the flowchart) ---
  q("u1", "Was the work created before 1 January 1969?",
    yes = "u2", no = "o_creation"),
  q("u2", "Is the work a photograph created before 1 June 1957?",
    yes = "o_creation", no = "u3"),
  q("u3", "Was the work published before 1 August 1989?",
    yes = "o_pub70", no = "u4"),
  q("u4", "Has the work ever been made available to the public?",
    yes = "u5", no = "o_2039"),
  q("u5", "Was the work first made available to the public before 1 January 1969?",
    yes = "o_2039", no = "o_avail70"),
  
  # --- Author KNOWN (green branch in the flowchart) ---
  q("k1", "Is the work a literary, dramatic or musical work, a photograph or an engraving, created before 1 August 1989?",
    yes = "k2", no = "o_life70"),
  q("k2", "Is the work a photograph taken before 1 June 1957?",
    yes = "o_life70", no = "k3"),
  q("k3", "Was the work published before 1 August 1989?",
    yes = "k4", no = "k5"),
  q("k4", "Did the author die more than 20 years before publication?",
    yes = "o_pub50", no = "o_life70"),
  q("k5", "Did the author die before 1 January 1969?",
    yes = "o_2039", no = "o_life70"),
  
  # --- Outcomes ---
  o("o_life70",   "Copyright expires 70 years after the author dies."),
  o("o_pub50",    "Copyright expires 50 years after first publication."),
  o("o_pub70",    "Copyright expires 70 years after first publication."),
  o("o_2039",     "Copyright expires on 31 December 2039."),
  o("o_avail70",  "Copyright expires 70 years after the work was made available to the public."),
  o("o_creation", paste0("Copyright expires 70 years after creation, or 70 years after the work ",
                         "was made available to the public if that happened within 70 years after creation."))
)
nodes <- setNames(nodes, vapply(nodes, function(n) n$id, character(1)))

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    primary = "#8d9c27",              # cedar green
    "link-color" = "#6b7819",         # darker green so links are readable on white
    "link-hover-color" = "#4f5a12"
  ),
  title = "Copyright duration checker",
  tags$head(tags$style(HTML("
    .app-wrap { max-width: 720px; margin: 2rem auto; }
    .question { font-size: 1.35rem; font-weight: 600; }
    .outcome  { font-size: 1.35rem; font-weight: 600; }
    .trail li { margin-bottom: .25rem; }
  "))),
  
  div(class = "app-wrap",
      h1("Copyright duration checker"),
      p(class = "text-muted",
        "Work out how long copyright lasts for a UK work (excluding Crown copyright) ",
        "by answering a few questions."),
      
      div(`aria-live` = "polite", uiOutput("card")),
      uiOutput("trail"),
      
      hr(),
      p(class = "small text-muted",
        "This tool follows ",
        tags$a(href = "https://cdn.nationalarchives.gov.uk/documents/information-management/non-crown-copyright-flowchart.pdf",
               "The National Archives' duration of copyright flowchart (PDF)", target = "_blank", rel = "noopener"),
        ", which is licensed under the ",
        tags$a(href = "https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/",
               "Open Government Licence v3.0", target = "_blank", rel = "noopener"),
        ". It covers the duration of copyright in the UK only, and does not cover Crown copyright, ",
        "works from other countries, or other rights such as database rights or performers' rights. ",
        "It is general guidance, not legal advice. Check the original flowchart and seek professional ",
        "advice if you are unsure."),
      h2(class = "h6 mt-4", "Further information"),
      tags$ul(class = "small",
              tags$li(tags$a(href = "https://www.legislation.gov.uk/ukpga/1988/48/contents",
                             "Copyright, Designs and Patents Act 1988",
                             target = "_blank", rel = "noopener")),
              tags$li(tags$a(href = "https://www.lboro.ac.uk/services/library/copyright/",
                             "Loughborough University Library: Copyright Guidance",
                             target = "_blank", rel = "noopener")),
              tags$li(tags$a(href = "https://blog.lboro.ac.uk/rdm/",
                             "Loughborough Research Data Management blog",
                             target = "_blank", rel = "noopener"))
      ),
      tags$div(
        class = "footer",
        tags$div(class = "app-wrap text-end", style = "margin: 0 auto;",
                 tags$a(href = "https://doi.org/10.17028/rd.lboro.28525481",
                        "Accessibility Statement")
        )
      )
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # history of nodes visited and the answers given to leave each question
  rv <- reactiveValues(ids = "start", answers = character(0))
  
  current <- reactive(nodes[[tail(rv$ids, 1)]])
  
  go <- function(answer) {
    node <- current()
    if (node$type != "question") return()
    nxt <- if (answer == "Yes") node$yes else node$no
    rv$answers <- c(rv$answers, answer)
    rv$ids     <- c(rv$ids, nxt)
  }
  
  observeEvent(input$yes, go("Yes"))
  observeEvent(input$no,  go("No"))
  
  observeEvent(input$back, {
    if (length(rv$ids) > 1) {
      rv$ids     <- head(rv$ids, -1)
      rv$answers <- head(rv$answers, -1)
    }
  })
  
  observeEvent(input$restart, {
    rv$ids     <- "start"
    rv$answers <- character(0)
  })
  
  output$card <- renderUI({
    node <- current()
    back_btn <- if (length(rv$ids) > 1)
      actionButton("back", "Back", class = "btn-outline-secondary")
    
    if (node$type == "question") {
      card(
        card_body(
          p(class = "question", node$text),
          div(class = "d-flex gap-2",
              actionButton("yes", "Yes", class = "btn-primary"),
              actionButton("no",  "No",  class = "btn-primary"),
              back_btn)
        )
      )
    } else {
      card(
        class = "border-success",
        card_body(
          p(class = "text-muted mb-1", "Result"),
          p(class = "outcome", node$text),
          div(class = "d-flex gap-2",
              back_btn,
              actionButton("restart", "Start again", class = "btn-success"))
        )
      )
    }
  })
  
  output$trail <- renderUI({
    n <- length(rv$answers)
    if (n == 0) return(NULL)
    items <- lapply(seq_len(n), function(i) {
      tags$li(nodes[[rv$ids[i]]]$text, " ", tags$strong(rv$answers[i]))
    })
    div(class = "mt-4",
        h2(class = "h6", "Your answers so far"),
        tags$ol(class = "trail small", items))
  })
}

shinyApp(ui, server)