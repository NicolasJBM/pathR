#' @name design_path_ui
#' @title Design learning paths
#' @author Nicolas Mangin
#' @description Module facilitating the design and visualization or a complete learning experience.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return Save the new or modified document in the original documents folder.
#' @importFrom editR selection_ui
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom shiny NS
#' @importFrom shiny actionButton
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny icon
#' @importFrom shiny numericInput
#' @importFrom shiny plotOutput
#' @importFrom shiny selectInput
#' @importFrom shiny span
#' @importFrom shiny tabPanel
#' @importFrom shiny uiOutput
#' @importFrom shinydashboard tabBox
#' @importFrom shinydashboardPlus box
#' @export


design_path_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    shiny::fluidRow(
      shiny::column(6, shiny::uiOutput(ns("slctlanguage"))),
      shiny::column(2, shiny::actionButton(
        ns("savepaths"), "Save",
        icon = shiny::icon("floppy-disk"),
        style = "width:100%;color:#FFFFFF;background-color:#006633;"
      )),
      shiny::column(2, shiny::actionButton(
        ns("openpaths"), "Open",
        icon = shiny::icon("file-excel"),
        style = "width:100%;color:#FFFFFF;background-color:#003399;"
      )),
      shiny::column(2, shiny::actionButton(
        ns("exportpaths"), "Export",
        icon = shiny::icon("file-export"),
        style = "width:100%;color:#FFFFFF;background-color:#660099;"
      ))
    ),
    shinydashboard::tabBox(
      side = "left", width = "100%",
      shiny::tabPanel(
        title = shiny::span(
          shiny::icon("bullseye"), "Learning Outcomes",
          title = "Map the general and specific learning outcomes for a course."
        ),
        shiny::fluidRow(
          shinydashboardPlus::box(
            title = "Create and edit outcomes",
            status = "navy",
            solidHeader = TRUE,
            width = 10,
            collapsible = TRUE,
            collapsed = TRUE,
            closable = FALSE,
            icon = shiny::icon("list"),
            gradient = FALSE,
            shiny::actionButton(ns("updateoutcomes"), "Update", icon = shiny::icon("rotate"), style = "width:100%;color:#FFFFFF;background-color:#003366;"),
            rhandsontable::rHandsontableOutput(ns("editoutcomes"))
          ),
          shinydashboardPlus::box(
            title = "Create and edit connections",
            status = "primary",
            solidHeader = TRUE,
            width = 2,
            collapsible = TRUE,
            collapsed = TRUE,
            closable = FALSE,
            icon = shiny::icon("circle-nodes"),
            gradient = FALSE,
            shiny::actionButton(ns("updateconnections"), "Update", icon = shiny::icon("rotate"), style = "width:100%;color:#FFFFFF;background-color:#003366;"),
            rhandsontable::rHandsontableOutput(ns("editconnections"))
          )
        ),
        shiny::fluidRow(
          shiny::column(4, shiny::selectInput(
            ns("slctlayoutoutcomes"), "Layout",
            choices = c("dh","drl","fr","graphopt","kk"),
            selected = "fr",
            width = "100%"
          )),
          shiny::column(4, shiny::numericInput(
            ns("defseedoutcomes"), "Seed",
            value = 1, min = 1, max = 1000, step = 1,
            width = "100%"
          )),
          shiny::column(4, shiny::numericInput(
            ns("defscalingoutcomes"), "Scale",
            value = 1, min = 0.01, max = 100, step = 0.01,
            width = "100%"
          ))
        ),
        shiny::uiOutput(ns("outcomemap"))
      ),
      shiny::tabPanel(
        title = shiny::span(
          shiny::icon("code-fork"), "Learning Paths",
          title = "Define sequences of activities to show the different paths through the course."
        ),
        shiny::fluidRow(
          shinydashboardPlus::box(
            title = "Create activities",
            status = "maroon",
            solidHeader = TRUE,
            width = 6,
            collapsible = TRUE,
            collapsed = TRUE,
            closable = FALSE,
            icon = shiny::icon("list"),
            gradient = FALSE,
            shiny::actionButton(ns("updateactlist"), "Update", icon = shiny::icon("rotate"), style = "width:100%;color:#FFFFFF;background-color:#003366;"),
            rhandsontable::rHandsontableOutput(ns("createactivities"))
          ),
          shinydashboardPlus::box(
            title = "Edit activities' attributes",
            status = "danger",
            solidHeader = TRUE,
            width = 3,
            collapsible = TRUE,
            collapsed = TRUE,
            closable = FALSE,
            icon = shiny::icon("id-card"),
            gradient = FALSE,
            shiny::actionButton(ns("updateactattr"), "Update", icon = shiny::icon("rotate"), style = "width:100%;color:#FFFFFF;background-color:#003366;"),
            editR::selection_ui(ns("slctact"), "Activity:"),
            shiny::uiOutput(ns("editactivities"))
          ),
          shinydashboardPlus::box(
            title = "Create and edit paths",
            status = "warning",
            solidHeader = TRUE,
            width = 3,
            collapsible = TRUE,
            collapsed = TRUE,
            closable = FALSE,
            icon = shiny::icon("circle-nodes"),
            gradient = FALSE,
            shiny::actionButton(ns("updatepaths"), "Update", icon = shiny::icon("rotate"), style = "width:100%;color:#FFFFFF;background-color:#003366;"),
            rhandsontable::rHandsontableOutput(ns("editpaths"))
          )
        ),
        shiny::fluidRow(
          shiny::column(4, shiny::selectInput(
            ns("slctlayoutactivities"), "Layout",
            choices = c("dh","drl","fr","graphopt","kk"),
            selected = "fr",
            width = "100%"
          )),
          shiny::column(4, shiny::numericInput(
            ns("defseedactivities"), "Seed",
            value = 1, min = 1, max = 1000, step = 1,
            width = "100%"
          )),
          shiny::column(4, shiny::numericInput(
            ns("defscalingactivities"), "Scale",
            value = 1, min = 0.1, max = 100, step = 0.1,
            width = "100%"
          ))
        ),
        shiny::uiOutput(ns("activitymap"))
      ),
      shiny::tabPanel(
        title = shiny::span(
          shiny::icon("scale-balanced"), "Design",
          title = "Check whether the learning experience design covers properly the learning outcomes and is well balanced."
        ),
        shiny::plotOutput(ns("outcomes_heatmap")) 
      ),
      shiny::tabPanel(
        title = shiny::span(
          shiny::icon("user-graduate"), "Students",
          title = "Student list."
        )
      ),
      shiny::tabPanel(
        title = shiny::span(
          shiny::icon("keyboard"), "Interactions",
          title = "Logs grom the learning management system."
        )
      ),
      shiny::tabPanel(
        title = shiny::span(
          shiny::icon("heart-pulse"), "Experiences",
          title = "Visualize students' interactions with the learning materials."
        ),
        shiny::plotOutput(ns("interaction_count"))
      )
    )
  )
}
