#' @name design_path_ui
#' @title Design learning paths
#' @author Nicolas Mangin
#' @description Module facilitating the design and visualization or a complete learning experience.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return Save the new or modified document in the original documents folder.
#' @importFrom DT dataTableOutput
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom shiny HTML
#' @importFrom shiny NS
#' @importFrom shiny actionButton
#' @importFrom shiny column
#' @importFrom shiny downloadButton
#' @importFrom shiny fileInput
#' @importFrom shiny fluidPage
#' @importFrom shiny fluidRow
#' @importFrom shiny icon
#' @importFrom shiny numericInput
#' @importFrom shiny plotOutput
#' @importFrom shiny selectInput
#' @importFrom shiny span
#' @importFrom shiny tabPanel
#' @importFrom shiny uiOutput
#' @importFrom shinyWidgets animateOptions
#' @importFrom shinyWidgets checkboxGroupButtons
#' @importFrom shinyWidgets dropdown
#' @importFrom shinydashboard tabBox
#' @export


design_path_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    shiny::tags$head(
      shiny::tags$style(
        shiny::HTML(".shiny-notification {
              height: 100px;
              width: 800px;
              position:fixed;
              top: calc(50% - 50px);;
              left: calc(50% - 400px);;
            }
           "
        )
      )
    ),
    
    # Top menu buttons (common to all tabs)
    shiny::uiOutput(ns("topmenu")),
    
    # Outcome tab
    shinydashboard::tabBox(
      side = "left", width = "100%",
      shiny::tabPanel(
        title = shiny::span(
          shiny::icon("bullseye"), "Learning Outcomes",
          title = "Map the general and specific learning outcomes for a course."
        ),
        shiny::fluidRow(
          # Edition
          shiny::column(
            2,
            shiny::fluidRow(
              shiny::column(
                4,
                shinyWidgets::dropdown(
                  shiny::actionButton(ns("updateoutcomes"), "Update", icon = shiny::icon("rotate"), style = "width:100%;color:#FFFFFF;background-color:#003366;"),
                  rhandsontable::rHandsontableOutput(ns("editoutcomes")),
                  size = "lg", style = "unite", icon = icon("arrows-to-dot"),
                  status = "danger", width = "1600px",
                  animate = shinyWidgets::animateOptions(
                    enter = shinyWidgets::animations$fading_entrances$fadeInLeftBig,
                    exit = shinyWidgets::animations$fading_exits$fadeOutRightBig
                  )
                )
              ),
              shiny::column(
                4,
                shinyWidgets::dropdown(
                  shiny::actionButton(ns("updateconnections"), "Update", icon = shiny::icon("rotate"), style = "width:100%;color:#FFFFFF;background-color:#003366;"),
                  rhandsontable::rHandsontableOutput(ns("editconnections")),
                  size = "lg", style = "unite", icon = icon("circle-nodes"),
                  status = "danger", width = "900px",
                  animate = shinyWidgets::animateOptions(
                    enter = shinyWidgets::animations$fading_entrances$fadeInLeftBig,
                    exit = shinyWidgets::animations$fading_exits$fadeOutRightBig
                  )
                )
              ),
              shiny::column(
                4,
                shinyWidgets::dropdown(
                  DT::dataTableOutput(ns("outcometable")),
                  size = "lg", style = "unite", icon = icon("table"),
                  status = "danger", width = "1600px",
                  animate = shinyWidgets::animateOptions(
                    enter = shinyWidgets::animations$fading_entrances$fadeInLeftBig,
                    exit = shinyWidgets::animations$fading_exits$fadeOutRightBig
                  )
                )
              )
            ),
            
            shiny::uiOutput(ns("egooutcomeselection")),
            
            shiny::fluidRow(
              shiny::column(
                6,
                shiny::actionButton(ns("newoutcome"), "Outcome", icon = shiny::icon("circle-dot"), style = "width:100%;margin-top:25px;color:#FFFFFF;background-color:#009933;"),
                shiny::actionButton(ns("splitoutcome"), "Split", icon = shiny::icon("scissors"), style = "width:100%;margin-top:25px;color:#FFFFFF;background-color:#003399;"),
                shiny::actionButton(ns("changeoutid"), "Change", icon = shiny::icon("left-right"), style = "width:100%;margin-top:25px;color:#FFFFFF;background-color:#330099;")
              ),
              shiny::column(
                6,
                shiny::actionButton(ns("newconnection"), "Link", icon = shiny::icon("arrow-right"), style = "width:100%;margin-top:25px;color:#FFFFFF;background-color:#009933;"),
                shiny::actionButton(ns("mergeoutcomes"), "Merge", icon = shiny::icon("down-left-and-up-right-to-center"), style = "width:100%;margin-top:25px;color:#FFFFFF;background-color:#003399;"),
                shiny::actionButton(ns("deleteoutcome"), "Delete", icon = shiny::icon("trash"), style = "width:100%;margin-top:25px;color:#FFFFFF;background-color:#330099;")
              )
            ),
            
            shiny::selectInput(
              ns("slctlayoutoutcomes"), "Layout",
              choices = c("dh","drl","fr","graphopt","kk"),
              selected = "dh",
              width = "100%"
            ),
            shiny::numericInput(
              ns("defseedoutcomes"), "Seed",
              value = 1, min = 1, max = 1000, step = 1,
              width = "100%"
            ),
            shiny::numericInput(
              ns("defscalingoutcomes"), "Scale",
              value = 3, min = 0.01, max = 100, step = 0.01,
              width = "100%"
            ),
            shinyWidgets::checkboxGroupButtons(
              inputId = ns("outcomeaxes"),
              label = "Change axes:", 
              choices = c(
                `<i class='fa fa-turn-up'></i>` = "switchxy",
                `<i class='fa fa-left-right'></i>` = "invertx",
                `<i class='fa fa-up-down'></i>` = "inverty"
              ),
              justified = TRUE, width = "100%"
            ),
            
            shiny::actionButton(ns("refreshoutmap"), "Map", icon = shiny::icon("map"), style = "width:100%;margin-top:25px;color:#FFFFFF;background-color:#990066;")
          ),
          # Visualization
          shiny::column(10, shiny::uiOutput(ns("outcomemap")))
        )
      ),
      
      
      
      # Activities tab
      shiny::tabPanel(
        title = shiny::span(
          shiny::icon("code-fork"), "Learning Paths",
          title = "Define sequences of activities to show the different paths through the course."
        ),
        shiny::fluidRow(
          # Edition
          shiny::column(
            2,
            shiny::fluidRow(
              shiny::column(
                3,
                shinyWidgets::dropdown(
                  shiny::actionButton(ns("doc_to_act"), "Organize", icon = shiny::icon("wand-magic-sparkles"), style = "width:100%;color:#FFFFFF;background-color:#003366;"),
                  shiny::uiOutput(ns("att_to_tags")),
                  shiny::dateInput(ns("startingdate"), "First day:"),
                  shiny::actionButton(
                    ns("createactivities"),
                    "Create activities", icon = shiny::icon("gears"),
                    style = "width:100%;margin-top:25px;color:#FFFFFF;background-color:#660099;"
                  ),
                  size = "lg", style = "unite", icon = icon("layer-group"),
                  status = "danger", width = "1600px",
                  animate = shinyWidgets::animateOptions(
                    enter = shinyWidgets::animations$fading_entrances$fadeInLeftBig,
                    exit = shinyWidgets::animations$fading_exits$fadeOutRightBig
                  )
                )
              ),
              shiny::column(
                3,
                shinyWidgets::dropdown(
                  shiny::actionButton(ns("updateactivities"), "Update", icon = shiny::icon("rotate"), style = "width:100%;color:#FFFFFF;background-color:#003366;"),
                  rhandsontable::rHandsontableOutput(ns("editactivities")),
                  size = "lg", style = "unite", icon = icon("arrows-to-dot"),
                  status = "danger", width = "1600px",
                  animate = shinyWidgets::animateOptions(
                    enter = shinyWidgets::animations$fading_entrances$fadeInLeftBig,
                    exit = shinyWidgets::animations$fading_exits$fadeOutRightBig
                  )
                )
              ),
              shiny::column(
                3,
                shinyWidgets::dropdown(
                  pathR::select_activity_ui(ns("slctact"), "Activity"),
                  shiny::uiOutput(ns("editattributes")),
                  shiny::actionButton(ns("updateactattr"), "Update", icon = shiny::icon("rotate"), style = "width:100%;color:#FFFFFF;background-color:#003366;"),
                  shiny::tableOutput(ns("relfiles")),
                  size = "lg", style = "unite", icon = icon("id-card"),
                  status = "danger", width = "1600px",
                  animate = shinyWidgets::animateOptions(
                    enter = shinyWidgets::animations$fading_entrances$fadeInLeftBig,
                    exit = shinyWidgets::animations$fading_exits$fadeOutRightBig
                  )
                )
              ),
              shiny::column(
                3,
                shinyWidgets::dropdown(
                  shiny::fluidRow(
                    shiny::column(
                      3,
                      shinyWidgets::materialSwitch(
                        inputId = ns("filternonassigned"),
                        label = "Only non-assigned", 
                        status = "primary",
                        right = FALSE
                      )
                    ),
                    shiny::column(
                      3,
                      shiny::uiOutput(ns("selectdoctoassign"))
                    ),
                    shiny::column(
                      3,
                      shiny::uiOutput(ns("selectacttoassign"))
                    ),
                    shiny::column(
                      3,
                      shiny::actionButton(
                        ns("updatedocassign"), "Update",
                        icon = shiny::icon("rotate"), style = "width:50%;color:#FFFFFF;background-color:#003366;margin-top:25px;"
                      ),
                    )
                  ),
                  size = "lg", style = "unite", icon = icon("file-arrow-down"),
                  status = "danger", width = "1600px",
                  animate = shinyWidgets::animateOptions(
                    enter = shinyWidgets::animations$fading_entrances$fadeInLeftBig,
                    exit = shinyWidgets::animations$fading_exits$fadeOutRightBig
                  )
                )
              )
            ),
            
            shiny::uiOutput(ns("egoactivityselection")),
            
            shiny::actionButton(ns("newactivity"), "Activity", icon = shiny::icon("circle-dot"), style = "width:100%;margin-top:25px;color:#FFFFFF;background-color:#009933;"),
            shiny::actionButton(ns("changeactid"), "Change", icon = shiny::icon("left-right"), style = "width:100%;margin-top:25px;color:#FFFFFF;background-color:#003399;"),
            shiny::actionButton(ns("deleteactivity"), "Delete", icon = shiny::icon("trash"), style = "width:100%;margin-top:25px;color:#FFFFFF;background-color:#330099;"),
            
            shiny::selectInput(
              ns("slctlayoutactivities"), "Layout",
              choices = c("dh","drl","fr","graphopt","kk"),
              selected = "dh",
              width = "100%"
            ),
            shiny::numericInput(
              ns("defseedactivities"), "Seed",
              value = 1, min = 1, max = 1000, step = 1,
              width = "100%"
            ),
            shiny::numericInput(
              ns("defscalingactivities"), "Scale",
              value = 3, min = 0.01, max = 100, step = 0.01,
              width = "100%"
            ),
            shinyWidgets::checkboxGroupButtons(
              inputId = ns("activityaxes"),
              label = "Change axes:", 
              choices = c(
                `<i class='fa fa-turn-up'></i>` = "switchxy",
                `<i class='fa fa-left-right'></i>` = "invertx",
                `<i class='fa fa-up-down'></i>` = "inverty"
              ),
              justified = TRUE, width = "100%"
            ),
            shiny::actionButton(ns("refreshactmap"), "Map", icon = shiny::icon("map"), style = "width:100%;margin-top:25px;color:#FFFFFF;background-color:#990066;"),
            shiny::actionButton(ns("maketest"), "Create test", icon = shiny::icon("list-check"), style = "width:100%;margin-top:25px;color:#FFFFFF;background-color:#990000;")
          ),
          # Visualization
          shiny::column(10, shiny::uiOutput(ns("activitymap")))
        )
      ),
      
      
      # Diagnostics tab
      shiny::tabPanel(
        title = shiny::span(
          shiny::icon("scale-balanced"), "Design",
          title = "Check whether the learning experience design covers properly the learning outcomes and is well balanced."
        ),
        shiny::uiOutput(ns("design_selections")),
        shiny::uiOutput(ns("design_valueboxes")),
        shiny::plotOutput(ns("workload_density")),
        shiny::plotOutput(ns("outcomes_heatmap")) 
      )
    )
  )
}

