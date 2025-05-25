#' @name design_path_server
#' @title Design learning paths
#' @author Nicolas Mangin
#' @description Module facilitating the design and visualization or a complete learning experience.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param intake Reactive. Function containing all information about the selected intake.
#' @param course_data List. description
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @return Save the new or modified page in the folder "2_documents/main_language/".
#' @importFrom DT renderDataTable
#' @importFrom DiagrammeR add_edge
#' @importFrom DiagrammeR add_node
#' @importFrom DiagrammeR create_graph
#' @importFrom DiagrammeR render_graph
#' @importFrom DiagrammeR to_igraph
#' @importFrom dplyr all_of
#' @importFrom dplyr anti_join
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr count
#' @importFrom dplyr everything
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_all
#' @importFrom dplyr mutate_if
#' @importFrom dplyr n
#' @importFrom dplyr percent_rank
#' @importFrom dplyr rename
#' @importFrom dplyr sample_n
#' @importFrom dplyr select
#' @importFrom dplyr slice_head
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom forcats fct_reorder
#' @importFrom forcats fct_rev
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 fill_alpha
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 geom_polygon
#' @importFrom ggplot2 geom_tile
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 scale_fill_gradient
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_linewidth
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_light
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 ylim
#' @importFrom igraph authority_score
#' @importFrom igraph betweenness
#' @importFrom igraph closeness
#' @importFrom igraph constraint
#' @importFrom igraph degree
#' @importFrom igraph eigen_centrality
#' @importFrom igraph harmonic_centrality
#' @importFrom igraph hub_score
#' @importFrom igraph layout_with_dh
#' @importFrom igraph layout_with_drl
#' @importFrom igraph layout_with_fr
#' @importFrom igraph layout_with_graphopt
#' @importFrom igraph layout_with_kk
#' @importFrom igraph power_centrality
#' @importFrom igraph strength
#' @importFrom igraph vertex_attr
#' @importFrom lubridate as_date
#' @importFrom lubridate hour
#' @importFrom lubridate week
#' @importFrom purrr map
#' @importFrom purrr map2_chr
#' @importFrom purrr map_chr
#' @importFrom purrr safely
#' @importFrom readxl read_excel
#' @importFrom rhandsontable hot_col
#' @importFrom rhandsontable hot_cols
#' @importFrom rhandsontable hot_context_menu
#' @importFrom rhandsontable hot_to_r
#' @importFrom rhandsontable renderRHandsontable
#' @importFrom rhandsontable rhandsontable
#' @importFrom shiny NS
#' @importFrom shiny actionButton
#' @importFrom shiny column
#' @importFrom shiny downloadHandler
#' @importFrom shiny eventReactive
#' @importFrom shiny fluidRow
#' @importFrom shiny h2
#' @importFrom shiny icon
#' @importFrom shiny incProgress
#' @importFrom shiny modalButton
#' @importFrom shiny modalDialog
#' @importFrom shiny moduleServer
#' @importFrom shiny numericInput
#' @importFrom shiny observe
#' @importFrom shiny observeEvent
#' @importFrom shiny reactive
#' @importFrom shiny reactiveValues
#' @importFrom shiny removeModal
#' @importFrom shiny renderPlot
#' @importFrom shiny renderUI
#' @importFrom shiny req
#' @importFrom shiny selectInput
#' @importFrom shiny selectizeInput
#' @importFrom shiny showModal
#' @importFrom shiny sliderInput
#' @importFrom shiny tagList
#' @importFrom shiny textAreaInput
#' @importFrom shiny textInput
#' @importFrom shiny withProgress
#' @importFrom shinyWidgets airDatepickerInput
#' @importFrom shinyWidgets colorPickr
#' @importFrom shinyWidgets pickerInput
#' @importFrom shinyWidgets radioGroupButtons
#' @importFrom shinyWidgets sliderTextInput
#' @importFrom shinyWidgets virtualSelectInput
#' @importFrom shinyalert shinyalert
#' @importFrom shinydashboard valueBox
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_split
#' @importFrom tibble tibble
#' @importFrom tibble tribble
#' @importFrom tidyr complete
#' @importFrom tidyr nest
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr replace_na
#' @importFrom tidyr unnest
#' @importFrom writexl write_xlsx
#' @export



design_path_server <- function(id, intake = NULL, course_data = NULL, course_paths = NULL){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    
    # ASSIGN ###################################################################
    
    color <- NULL
    flag <- NULL
    label <- NULL
    langiso <- NULL
    language <- NULL
    modrval <- NULL
    outcome <- NULL
    type <- NULL
    destination <- NULL
    destlab <- NULL
    origin <- NULL
    origlab <- NULL
    shape <- NULL
    URL <- NULL
    description <- NULL
    lmsid <- NULL
    n <- NULL
    tooltip <- NULL
    activity <- NULL
    code <- NULL
    condition <- NULL
    title <- NULL
    attribute <- NULL
    value <- NULL
    duration <- NULL
    end <- NULL
    peripheries <- NULL
    reqlab <- NULL
    soclab <- NULL
    style <- NULL
    tslab <- NULL
    weigth <- NULL
    data <- NULL
    labdest <- NULL
    laborig <- NULL
    penwidth <- NULL
    requirement <- NULL
    alpha <- NULL
    bordercolor <- NULL
    fillcolor <- NULL
    category <- NULL
    fullname <- NULL
    object <- NULL
    userid <- NULL
    week <- NULL
    outcomelab <- NULL
    day <- NULL
    newid <- NULL
    outcome_labels <- NULL
    simplelab <- NULL
    fontcolor <- NULL
    endday <- NULL
    endweek <- NULL
    objectnbr <- NULL
    social <- NULL
    start <- NULL
    startday <- NULL
    startweek <- NULL
    level <- NULL
    time_space <- NULL
    outcomenbr <- NULL
    endorder <- NULL
    period <- NULL
    startorder <- NULL
    height <- NULL
    subject <- NULL
    width <- NULL
    fontsize <- NULL
    avg <- NULL
    high <- NULL
    low <- NULL
    sdev <- NULL
    student <- NULL
    lty <- NULL
    lwd <- NULL
    levlab <- NULL
    from <- NULL
    to <- NULL
    authority <- NULL
    harmonic <- NULL
    hub <- NULL
    indegree <- NULL
    negpower <- NULL
    outdegree <- NULL
    power <- NULL
    shapesize <- NULL
    subgroup <- NULL
    findbidir <- NULL
    preslctsubgroup <- NULL
    
    
    
    
    
    # INPUT METHOD #############################################################
    
    sourcetype <- shiny::reactive({
      shiny::req(!base::is.null(intake))
      if (base::is.null(course_data) | base::is.null(course_paths)){
        "external"
      } else {
        "internal"
      }
    })
    
    output$topmenu <- shiny::renderUI({
      shiny::req(!base::is.null(sourcetype()))
      
      if (sourcetype() == "internal"){
        
        shiny::fluidRow(
          shiny::column(2, shiny::uiOutput(ns("slctlanguage"))),
          shiny::column(2, shiny::actionButton(
            ns("loadpath"), "Reload",
            icon = shiny::icon("upload"),
            style = "width:100%;color:#FFFFFF;background-color:#003366;"
          )),
          shiny::column(2, shiny::actionButton(
            ns("savepaths"), "Save",
            icon = shiny::icon("floppy-disk"),
            style = "width:100%;color:#FFFFFF;background-color:#006633;"
          )),
          shiny::column(2, shiny::actionButton(
            ns("openpaths"), "Open",
            icon = shiny::icon("file-excel"),
            style = "width:100%;color:#FFFFFF;background-color:#000099;"
          )),
          shiny::column(2, shiny::actionButton(
            ns("openfolder"), "Folder",
            icon = shiny::icon("folder-open"),
            style = "width:100%;color:#FFFFFF;background-color:#660099;"
          ))
        )
        
      } else {
        
        shiny::fluidRow(
          shiny::column(2, shiny::uiOutput(ns("slctlanguage"))),
          shiny::column(2, shiny::fileInput(ns("uploadpath"), label = NULL, accept = ".xlsx", width = "100%")),
          shiny::column(2, shiny::actionButton(
            ns("loadpath"), "Reload",
            icon = shiny::icon("upload"),
            style = "width:100%;color:#FFFFFF;background-color:#003366;"
          )),
          shiny::column(2, shiny::actionButton(
            ns("savepaths"), "Save",
            icon = shiny::icon("floppy-disk"),
            style = "width:100%;color:#FFFFFF;background-color:#006633;"
          )),
          shiny::column(2, shiny::downloadButton(ns("downloadpath"), "Download"))
        )
        
      }
    })
    
     
     # DATA #####################################################################
     
    reactval <- shiny::reactiveValues()
    
    pathfile <- shiny::reactive({
      input$loadpath
      shiny::req(!base::is.null(sourcetype()))
      if (sourcetype() == "internal"){
        base::paste0(
          course_paths()$subfolders$paths, "/",
          intake()$intake$path[1], ".xlsx"
        )
      } else {
        if (!base::is.null(input$uploadpath)) {
          input$uploadpath$datapath
        } else {
          NA
        }
      }
    })
    
    shiny::observe({
      
      shiny::req(!base::is.null(sourcetype()))
      shiny::req(!base::is.na(pathfile()))
      
      if (sourcetype() == "internal" & !base::file.exists(pathfile())){
        writexl::write_xlsx(pathR::create_database(), path = pathfile())
      }
      
      shiny::req(base::file.exists(pathfile()))
      
      reactval$outcomes <- readxl::read_excel(pathfile(), sheet = "outcomes", col_types = "text")
      reactval$connections <- readxl::read_excel(pathfile(), sheet = "connections", col_types = "text")
      reactval$outlabels <- readxl::read_excel(pathfile(), sheet = "outlabels", col_types = "text")
      reactval$activities <- readxl::read_excel(pathfile(), sheet = "activities", col_types = "text")
      reactval$actlabels <- readxl::read_excel(pathfile(), sheet = "actlabels", col_types = "text")
      reactval$attributes <- readxl::read_excel(pathfile(), sheet = "attributes", col_types = "text")
      
      if (sourcetype() == "internal"){
        reactval$languages <- course_data()$languages
      } else {
        reactval$languages <- readxl::read_excel(pathfile(), sheet = "languages", col_types = "text")
      }
      
    })
    
    
    
    output$slctlanguage <- shiny::renderUI({
      input$reloadpath
      shiny::req(!base::is.null(reactval$languages))
      languages <- reactval$languages |>
        dplyr::select(langiso, language, flag)
      shinyWidgets::radioGroupButtons(
        inputId = ns("slctlang"), label = NULL,
        choiceNames = base::lapply(
          base::seq_along(languages$langiso), 
          function(i) shiny::tagList(
            shiny::tags$img(src = languages$flag[i], width = 20, height = 15),
            languages$language[i]
          )
        ),
        choiceValues = languages$langiso,
        status = "primary", justified = FALSE, size = "sm",
        direction = "horizontal",
        checkIcon = base::list(yes = shiny::icon("check"))
      )
    })
    
    
    
    
    
    # OUTCOME EDITION ##########################################################
    
    
    
    # Combine outcome information for a selected language
    outcomelist <- shiny::reactive({
      shiny::req(!base::is.null(input$slctlang))
      shiny::req(!base::is.null(reactval$outcomes))
      shiny::req(!base::is.null(reactval$outlabels))
      labels <- reactval$outlabels |>
        dplyr::filter(language == input$slctlang) |>
        dplyr::select(-language)
      reactval$outcomes |>
        dplyr::left_join(labels, by = "outcome") |>
        dplyr::select(outcome, label, description, lmsid, URL, order, type, color) |>
        dplyr::mutate(
          type = base::factor(type, levels = c("GEN","SPE","PRE")),
          order = base::as.numeric(order)
        ) |>
        dplyr::arrange(order)
    })
    
    
    
    # Edit outcomes in a table
    output$editoutcomes <- rhandsontable::renderRHandsontable({
      shiny::req(outcomelist())
      outcomelist() |>
        rhandsontable::rhandsontable(
          height = 500, width = "100%", rowHeaders = NULL, stretchH = "all"
        ) |>
        rhandsontable::hot_cols(
          colWidths = c("10%","20%","40%","5%","10%","5%","5%","5%")
        ) |>
        rhandsontable::hot_col(1, readOnly = TRUE) |>
        rhandsontable::hot_context_menu(
          allowRowEdit = FALSE, allowColEdit = FALSE
        )
    })
    
    
    
    shiny::observeEvent(input$updateoutcomes, {
      shiny::req(!base::is.null(input$editoutcomes))
      updated_outcomes <- rhandsontable::hot_to_r(input$editoutcomes) |>
        dplyr::mutate_all(base::as.character) |>
        dplyr::filter(!base::is.na(outcome), outcome != "")
      
      checkids <- updated_outcomes |>
        dplyr::group_by(outcome) |>
        dplyr::count() |>
        dplyr::filter(n > 1)
      
      misslabels <- updated_outcomes |>
        dplyr::filter(base::is.na(label) | label == "")
      
      if (base::nrow(checkids) == 0 & base::nrow(misslabels) == 0){
        
        updated_outlabels <- updated_outcomes |>
          dplyr::mutate(language = input$slctlang) |>
          dplyr::select(outcome, language, label, description, lmsid, URL) |>
          base::unique()
        
        updated_outcomes <- updated_outcomes |>
          dplyr::select(-language, -label, -description, -lmsid, -URL) |>
          base::unique()
        
        all_languages <- reactval$languages$langiso
        
        outlabels <- updated_outcomes |>
          dplyr::select(outcome) |>
          dplyr::mutate(language = base::list(all_languages)) |>
          tidyr::unnest(language) |>
          dplyr::left_join(reactval$outlabels, by = c("outcome", "language")) |>
          dplyr::group_by(outcome) |>
          tidyr::nest() |>
          dplyr::mutate(data = purrr::map(data, function(x,y){
            replacements <- dplyr::filter(x, language == y)
            tidyr::replace_na(x, base::list(
              label = replacements$label[1],
              description = replacements$description[1],
              lmsid = replacements$lmsid[1],
              URL = replacements$URL[1]
            ))
          }, y = input$slctlang)) |>
          tidyr::unnest("data") |>
          dplyr::ungroup() |>
          dplyr::filter(language != input$slctlang) |>
          dplyr::bind_rows(updated_outlabels) |>
          base::unique()
        
        connections <- reactval$connections |>
          dplyr::filter(
            origin %in% updated_outcomes$outcome,
            destination %in% updated_outcomes$outcome
          )
        
        activities <- reactval$activities
        actoucomes <- activities$outcomes |>
          stringr::str_split(" ", simplify = TRUE) |>
          base::as.character() |>
          base::unique()
        actoucomes <- actoucomes[actoucomes != ""]
        newoutcomes <- updated_outcomes$outcome
        missing <- base::setdiff(actoucomes, newoutcomes)
        if (base::length(missing > 0)){
          missing <- missing |>
            base::trimws() |>
            base::paste(collapse = "|")
          activities <- activities |>
            dplyr::mutate(outcomes = stringr::str_remove_all(outcomes, missing))
        }
        
        reactval$outlabels <- outlabels
        reactval$outcomes <- updated_outcomes
        reactval$connections <- connections
        reactval$activities <- activities
        
        shinyalert::shinyalert(
          "Outcomes updtated",
          "Refresh the map to see the changes.",
          type = "success"
        )
        
      } else if (base::nrow(checkids) > 0) {
        shinyalert::shinyalert(
          "Duplicated IDs",
          base::paste0(
            "The ID(s) ",
            base::paste(checkids$outcome, collapse = ", "),
            " is/are duplicated. They should be unique."
          ),
          type = "error"
        )
      } else if (base::nrow(misslabels) > 0) {
        shinyalert::shinyalert(
          "Missing labels",
          base::paste0(
            "The ID(s) ",
            base::paste(misslabels$outcome, collapse = ", "),
            " have no label. Please make a unique one."
          ),
          type = "error"
        )
      }
    })
    
    
    
    # Edit connections between outcomes in a table
    output$editconnections <- rhandsontable::renderRHandsontable({
      reactval$connections |>
        stats::na.omit() |>
        dplyr::mutate(
          origin = base::factor(origin, reactval$outcomes$outcome),
          destination = base::factor(destination, reactval$outcomes$outcome)
        ) |>
        dplyr::arrange(origin, destination) |>
        rhandsontable::rhandsontable(
          height = 500, width = "100%", rowHeaders = NULL, stretchH = "all"
        ) |>
        rhandsontable::hot_cols(
          colWidths = c("50%","50%")
        ) |>
        rhandsontable::hot_context_menu(
          allowRowEdit = TRUE, allowColEdit = FALSE
        )
    })
    
    shiny::observeEvent(input$updateconnections, {
      updated_connections <- rhandsontable::hot_to_r(input$editconnections) |>
        dplyr::mutate_all(base::as.character) |>
        stats::na.omit() |>
        base::unique() |>
        dplyr::filter(origin != destination)
      reactval$connections <- updated_connections
      shinyalert::shinyalert(
        "Connections updtated",
        "Refresh the map to see the changes.",
        type = "success"
      )
    })
    
    
    
    # Button to add outcomes to the database
    shiny::observeEvent(input$newoutcome, {
      shiny::req(!base::is.null(outcomelist()))
      
      types <- c("GEN","SPE","PRE")
      base::names(types) <- c("Generic Learning Outcome","Specific Learning Outcome","Prerequisite")
      
      outcomes <- outcomelist()$outcome
      base::names(outcomes) <- stringr::str_replace_all(outcomelist()$label, "_", " ")
      
      shiny::showModal(shiny::modalDialog(
        title = "Add a new outcome",
        shiny::fluidRow(
          shiny::column(
            2,
            shiny::textInput(ns("newoutid"), "ID:", value = "", width = "100%")
          ),
          shiny::column(
            6,
            shiny::textInput(ns("newoutlab"), "Label:", value = "Write a label here", width = "100%")
          ),
          shiny::column(
            2,
            shiny::selectInput(
              ns("newouttype"), "Type:",
              choices = types, selected = types[1],
              multiple = FALSE, width = "100%"
            )
          ),
          shiny::column(
            1,
            shiny::textInput(ns("newoutorder"), "Order:", value = NA, width = "100%")
          ),
          shiny::column(
            1,
            shinyWidgets::colorPickr(ns("newoutcolor"), "Color:")
          )
        ),
        
        shiny::textAreaInput(ns("newoutdesc"), "Description:", value = "", width = "100%"),
        
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::textAreaInput(ns("newoutlmsid"), "ID LMD:", value = "", width = "100%"),
          ),
          shiny::column(
            6,
            shiny::textAreaInput(ns("newouturl"), "URL address:", value = "", width = "100%"),
          )
        ),
        
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::selectInput(
              ns("newoutorig"), "Origins:",
              choices = outcomes, selected = NULL,
              multiple = TRUE, width = "100%"
            )
          ),
          shiny::column(
            6,
            shiny::selectInput(
              ns("newoutdest"), "Destinations:",
              choices = outcomes, selected = NULL,
              multiple = TRUE, width = "100%"
            )
          )
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("addoutcome"), "OK")
        )
      ))
    })
    
    
    
    shiny::observeEvent(input$addoutcome, {
      if (input$newoutid == ""){
        shinyalert::shinyalert(
          title = "Missing ID!",
          text = "You must give the outcome an ID.",
          type = "error"
        )
      } else if (input$newoutid %in% reactval$outcomes$outcome) {
        shinyalert::shinyalert(
          title = "Non unique ID!",
          text = "The ID you gave to the outcome is already assigned to another outcome.",
          type = "error"
        )
      } else {
        shiny::removeModal()
        
        newoutcomes <- tibble::tibble(
          outcome = input$newoutid,
          order = input$newoutorder,
          type = input$newouttype,
          color = input$newoutcolor
        ) |>
          dplyr::mutate_all(base::as.character)
        
        newoutlabels <- tibble::tibble(
          outcome = input$newoutid,
          language = base::unique(reactval$languages$langiso),
          label = input$newoutlab,
          description = input$newoutdesc,
          lmsid = input$newoutlmsid,
          URL = input$newouturl
        ) |>
          dplyr::mutate_all(base::as.character)
        
        reactval$outcomes <- reactval$outcomes |>
          dplyr::bind_rows(newoutcomes)
        reactval$outlabels <- reactval$outlabels |>
          dplyr::bind_rows(newoutlabels)
        
        newconnections <- dplyr::bind_rows(
          tibble::tibble(
            origin = input$newoutorig,
            destination = input$newoutid
          ),
          tibble::tibble(
            origin = input$newoutid,
            destination = input$newoutdest
          )
        ) |>
          stats::na.omit() |>
          dplyr::mutate_all(base::as.character) |>
          dplyr::filter(
            origin %in% reactval$outcomes$outcome,
            destination %in% reactval$outcomes$outcome
          )
        
        reactval$connections <- reactval$connections |>
          dplyr::bind_rows(newconnections)
        
        shinyalert::shinyalert(
          title = "New outcome added",
          text = "The new outcome has been successfully added to the database.",
          type = "success"
        )
      }
    })
    
    
    
    # Button to create a new connection between outcomes
    shiny::observeEvent(input$newconnection, {
      shiny::req(!base::is.null(outcomelist()))
      
      outcomes <- c(outcomelist()$outcome)
      base::names(outcomes) <- c(stringr::str_replace_all(outcomelist()$label, "_", " "))
      
      shiny::showModal(shiny::modalDialog(
        title = "Add or edit a connection",
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::selectInput(
              ns("newconnectionorig"), "Origin:",
              choices = outcomes,
              selected = NULL,
              multiple = FALSE, width = "100%"
            )
          ),
          shiny::column(
            6,
            shiny::selectInput(
              ns("newconnectiondest"), "Destination:",
              choices = outcomes,
              selected = NULL,
              multiple = FALSE, width = "100%"
            )
          )
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("addconnection"), "OK")
        )
      ))
    })
    
    
    
    shiny::observeEvent(input$addconnection, {
      shiny::removeModal()
      
      newconnection <- tibble::tibble(
        origin = input$newconnectionorig,
        destination = input$newconnectiondest
      )
      
      connections <- reactval$connections |>
        dplyr::anti_join(newconnection, by = c("origin","destination")) |>
        dplyr::anti_join(dplyr::rename(
          newconnection, origin = destination,
          destination = origin),
          by = c("origin","destination")
        ) |>
        dplyr::bind_rows(newconnection)
      
      reactval$connections <- connections
      
      shinyalert::shinyalert(
        title = "Connection added or edited",
        text = "The connection has been added or changed in the database.",
        type = "success"
      )
    })
    
    
    
    # Button to split an outcome in two (one origin connected to antecedents and one destination connected to dependents)
    shiny::observeEvent(input$splitoutcome, {
      shiny::req(!base::is.null(outcomelist()))
      
      outcomes <- outcomelist()$outcome
      base::names(outcomes) <- outcomelist()$label
      
      shiny::showModal(shiny::modalDialog(
        title = "Split an outcome",
        shiny::selectInput(
          ns("frominitout"), "Outcome to split:",
          choices = outcomes,
          selected = NULL,
          multiple = FALSE, width = "100%"
        ),
        shiny::fluidRow(
          shiny::column(
            5,
            shiny::textInput(ns("tooriginoutid"), "Into origin:", value = "", width = "100%"),
            shiny::textInput(ns("tooriginoutlab"), "With the label:", value = "", width = "100%")
          ),
          shiny::column(
            5,
            shiny::textInput(ns("todestoutid"), "Into destination:", value = "", width = "100%"),
            shiny::textInput(ns("todestoutlab"), "With the label:", value = "", width = "100%")
          )
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("splitout"), "OK")
        )
      ))
    })
    
    
    
    shiny::observeEvent(input$splitout, {
      
      if (input$tooriginoutid == "" | input$tooriginoutlab == "" |
          input$todestoutid == "" | input$todestoutlab == ""){
        shinyalert::shinyalert(
          title = "Missing ID or labels!",
          text = "You must IDS and labels to all outcomes.",
          type = "error"
        )
      } else if (input$tooriginoutid %in% reactval$outcomes$outcome |
                 input$todestoutid %in% reactval$outcomes$outcome) {
        shinyalert::shinyalert(
          title = "Non unique ID!",
          text = "At least one of the ID you gave to the outcomes is already assigned to another outcome.",
          type = "error"
        )
      } else {
        shiny::removeModal()
        
        shiny::req(!base::is.null(outcomelist()))
        selectedout <- outcomelist() |>
          dplyr::filter(outcome == input$frominitout) |>
          dplyr::select(outcome, order, type, color)
        
        newoutcomes <- tibble::tibble(
          outcome = c(input$tooriginoutid, input$todestoutid),
          order = selectedout$order[1],
          type = selectedout$type[1],
          color = selectedout$color[1]
        ) |>
          dplyr::mutate_all(base::as.character)
        
        neworiglabels <- tibble::tibble(
          outcome = input$tooriginoutid,
          language = base::unique(reactval$languages$langiso),
          label = input$tooriginoutlab,
          description = "",
          lmsid = NA,
          URL = NA
        ) |>
          dplyr::mutate_all(base::as.character)
        
        newdestlabels <- tibble::tibble(
          outcome = input$todestoutid,
          language = base::unique(reactval$languages$langiso),
          label = input$todestoutlab,
          description = "",
          lmsid = NA,
          URL = NA
        ) |>
          dplyr::mutate_all(base::as.character)
        
        outcomes <- reactval$outcomes |>
          dplyr::filter(outcome != input$frominitout) |>
          dplyr::bind_rows(newoutcomes)
        
        outlabels <- reactval$outlabels |>
          dplyr::filter(outcome != input$frominitout) |>
          dplyr::bind_rows(neworiglabels) |>
          dplyr::bind_rows(newdestlabels)
        
        allbefore <- reactval$connections |>
          dplyr::filter(destination == input$frominitout) |>
          dplyr::mutate(destination = input$tooriginoutid)
        
        allafter <- reactval$connections |>
          dplyr::filter(origin == input$frominitout) |>
          dplyr::mutate(origin = input$todestoutid)
        
        newconnections <- tibble::tibble(
          origin = input$tooriginoutid,
          destination = input$todestoutid
        ) |>
          dplyr::bind_rows(allbefore) |>
          dplyr::bind_rows(allafter) |>
          stats::na.omit() |>
          dplyr::mutate_all(base::as.character)
        
        connections <- reactval$connections |>
          dplyr::filter(
            origin != input$frominitout,
            destination != input$frominitout
          ) |>
          dplyr::bind_rows(newconnections) |>
          dplyr::mutate_all(base::as.character)
        
        activities <- reactval$activities |>
          dplyr::mutate(outcomes = stringr::str_replace_all(outcomes, input$frominitout, input$tooriginoutid))
        
        reactval$outcomes <- outcomes
        reactval$outlabels <- outlabels
        reactval$connections <- connections
        reactval$activities <- activities
        
        shinyalert::shinyalert(
          title = "Outcome splitted",
          text = "The outcome has been successfully splitted in the database.",
          type = "success"
        )
      }
    })
    
    
    
    # Button to merge two outcomes in one
    shiny::observeEvent(input$mergeoutcomes, {
      shiny::req(!base::is.null(outcomelist()))
      
      outcomes <- outcomelist()$outcome
      base::names(outcomes) <- outcomelist()$label
      
      shiny::showModal(shiny::modalDialog(
        title = "Merge outcomes",
        
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::selectInput(
              ns("mergenode1"), "First outcome to merge:",
              choices = outcomes,
              selected = NULL,
              multiple = FALSE, width = "100%"
            )
          ),
          shiny::column(
            6,
            shiny::selectInput(
              ns("mergenode2"), "Second outcome to merge:",
              choices = outcomes,
              selected = NULL,
              multiple = FALSE, width = "100%"
            )
          )
        ),
        shiny::textInput(ns("mergedid"), "Into outcome ID:", value = "NEWID", width = "100%"),
        shiny::textInput(ns("mergedlab"), "Into outcome ID label:", value = "New label", width = "100%"),
        
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("mergeout"), "OK")
        )
      ))
    })
    
    
    
    shiny::observeEvent(input$mergeout, {
      if (input$mergedid %in% reactval$outcomes$outcome){
        shinyalert::shinyalert(
          title = "Non unique ID!",
          text = "The ID you gave to the merged outcome is already assigned to another outcome.",
          type = "error"
        )
      } else {
        
        shiny::removeModal()
        shiny::req(!base::is.null(outcomelist()))
        
        selectedout <- outcomelist() |>
          dplyr::filter(outcome %in% c(input$mergenode1, input$mergenode2)) |>
          dplyr::select(outcome, order, type, color)
        
        newoutcome <- tibble::tibble(
          outcome = input$mergedid,
          order = selectedout$order[1],
          type = selectedout$type[1],
          color = selectedout$color[1]
        ) |>
          dplyr::mutate_all(base::as.character)
        
        newlabel <- tibble::tibble(
          outcome = input$mergedid,
          language = base::unique(reactval$languages$langiso),
          label = input$mergedlab,
          description = "",
          lmsid = NA,
          URL = NA
        ) |>
          dplyr::mutate_all(base::as.character)
        
        outcomes <- reactval$outcomes |>
          dplyr::filter(!(outcome %in% c(input$mergenode1, input$mergenode2))) |>
          dplyr::bind_rows(newoutcome)
        
        outlabels <- reactval$outlabels |>
          dplyr::filter(!(outcome %in% c(input$mergenode1, input$mergenode2))) |>
          dplyr::bind_rows(newlabel)
        
        allbefore <- reactval$connections |>
          dplyr::filter(destination %in% c(input$mergenode1, input$mergenode2)) |>
          dplyr::mutate(destination = input$mergedid)
        
        allafter <- reactval$connections |>
          dplyr::filter(origin %in% c(input$mergenode1, input$mergenode2)) |>
          dplyr::mutate(origin = input$mergedid)
        
        newconnections <- dplyr::bind_rows(allbefore, allafter) |>
          base::unique() |>
          dplyr::mutate_all(base::as.character) |>
          stats::na.omit() |>
          dplyr::filter(origin != "", destination != "")
        
        connections <- reactval$connections |>
          dplyr::filter(
            !(origin %in% c(input$mergenode1, input$mergenode2)),
            !(destination %in% c(input$mergenode1, input$mergenode2))
          ) |>
          dplyr::bind_rows(newconnections) |>
          base::unique() |>
          stats::na.omit() |>
          dplyr::mutate_all(base::as.character)
        
        activities <- reactval$activities |>
          dplyr::mutate(outcomes = stringr::str_replace_all(outcomes, input$mergenode1, input$mergedid)) |>
          dplyr::mutate(outcomes = stringr::str_replace_all(outcomes, input$mergenode2, input$mergedid))
        
        reactval$outcomes <- outcomes
        reactval$outlabels <- outlabels
        reactval$connections <- connections
        reactval$activities <- activities
        
        shinyalert::shinyalert(
          title = "Outcomes merged",
          text = "The outcomes have been successfully merged in the database.",
          type = "success"
        )
      }
    })
    
    
    
    # Button to change the ID of an outcome in all databases
    shiny::observeEvent(input$changeoutid, {
      shiny::req(!base::is.null(reactval$outcomes))
      outcomeids <- reactval$outcomes$outcome
      shiny::showModal(shiny::modalDialog(
        title = "Change the outcome id",
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::selectInput(ns("fromoutid"), "From this ID:", choices = outcomeids, width = "100%")
          ),
          shiny::column(
            6,
            shiny::textInput(ns("tooutid"), "To thios ID:", value = "NEWID", width = "100%")
          )
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("replaceoutid"), "OK")
        )
      ))
    })
    
    
    
    shiny::observeEvent(input$replaceoutid, {
      
      shiny::req(!base::is.null(input$fromoutid))
      shiny::req(!base::is.null(input$tooutid))
      
      fromid <- input$fromoutid
      toid <- input$tooutid
      
      outcomes <- reactval$outcomes
      connections <- reactval$connections
      activities <- reactval$activities
      outlabels <- reactval$outlabels
      
      if (!(toid %in% reactval$outcomes$outcome)){
        
        shiny::removeModal()
        
        outcomes$outcome <- stringr::str_replace_all(
          outcomes$outcome, fromid, toid
        )
        connections$origin <- stringr::str_replace_all(
          connections$origin, fromid, toid
        )
        connections$destination <- stringr::str_replace_all(
          connections$destination, fromid, toid
        )
        outlabels$outcome <- stringr::str_replace_all(
          outlabels$outcome, fromid, toid
        )
        activities$outcomes <- stringr::str_replace_all(
          activities$outcomes, fromid, toid
        )
        
        reactval$outcomes <- outcomes
        reactval$connections <- connections
        reactval$activities <- activities
        reactval$outlabels <- outlabels
        
        shinyalert::shinyalert(
          title = "Outcome ID replaced",
          text = "The outcome ID has been replaced in all databases. Please save the map to definitely record your changes.",
          type = "success"
        )
      } else {
        shinyalert::shinyalert(
          title = "Outcome ID already existing",
          text = "You cannot replace the ID of an outcome by an ID which is already existing in the database.",
          type = "error"
        )
      }
    })
    
    
    
    # Button to delete an outcome
    shiny::observeEvent(input$deleteoutcome, {
      shiny::req(!base::is.null(reactval$outcomes))
      outcomeids <- reactval$outcomes$outcome
      shiny::showModal(shiny::modalDialog(
        title = "Delete an outcome",
        shiny::selectInput(ns("outcometodelete"), "Outcome to delete:", choices = outcomeids, width = "100%"),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("removeoutcome"), "OK")
        )
      ))
    })
    
    
    
    shiny::observeEvent(input$removeoutcome, {
      
      shiny::removeModal()
      
      outcomes <- reactval$outcomes |>
        dplyr::filter(outcome != input$outcometodelete)
      
      connections <- reactval$connections |>
        dplyr::filter(
          origin != input$outcometodelete,
          destination != input$outcometodelete
        )
      
      activities <- reactval$activities
      activities$outcomes <- stringr::str_remove_all(
        activities$outcomes, input$outcometodelete
      )
      
      outlabels <- reactval$outlabels |>
        dplyr::filter(outcome != input$outcometodelete)
      
      reactval$outcomes <- outcomes
      reactval$connections <- connections
      reactval$activities <- activities
      reactval$outlabels <- outlabels
      
      shinyalert::shinyalert(
        title = "Outcome deleted",
        text = "The outcome has been deleted from the database.",
        type = "success"
      )
    })
    
    
    
    
    
    # OUTCOME VISUALIZATION ####################################################
    
    
    
    # Visualize the outcome map.
    output$egooutcomeselection <- shiny::renderUI({
      shiny::req(!base::is.null(outcomelist()))
      outcomes <- outcomelist() |>
        dplyr::select(outcome, label, color) |>
        dplyr::mutate(label = stringr::str_replace_all(label, "_", " "))
      outcomechoices <- outcomes$outcome
      base::names(outcomechoices) <- outcomes$label
      outcomecolors <- base::paste0(
        "color: #FFFFFF; background: ", outcomes$color,";"
      )
      shinyWidgets::pickerInput(
        inputId = ns("slctegooutcome"),
        label = "Focus on outcomes:", 
        choices = outcomechoices,
        selected = NULL,
        choicesOpt = base::list(style = outcomecolors),
        multiple = TRUE,
        width = "100%"
      )
    })
    
    
    
    outcome_graph <- shiny::eventReactive(input$refreshoutmap, {
      shiny::req(base::nrow(reactval$outlabels) > 1)
      shiny::req(base::nrow(reactval$outcomes) > 1)
      shiny::req(base::nrow(reactval$connections) > 1)
      shiny::req(!base::is.null(input$slctlang))
      shiny::req(!base::is.null(outcomelist()))
      
      shiny::withProgress(message = "Create the network of outcomes", {
        
        shiny::incProgress(1/10)
        
        outcome_graph <- DiagrammeR::create_graph()
        
        shiny::incProgress(1/10)
        
        nodes <- outcomelist() |>
          tidyr::replace_na(base::list(description = " ")) |>
          dplyr::mutate(
            shape = dplyr::case_when(
              type == "PRE" ~ "rectangle",
              type == "GEN" ~ "star",
              TRUE ~ "ellipse"
            ),
            simplelab = stringr::str_replace_all(label, "_", " "),
            tooltip = base::paste(simplelab, description, sep = "\n\n"),
            tooltip = base::paste(tooltip, outcome, sep = "\n\n")
          ) |>
          dplyr::select(outcome, label, shape, color, tooltip, URL) |>
          dplyr::mutate(label = stringr::str_replace_all(label, "_", "\n"))
        
        shiny::incProgress(1/10)
        
        for (i in base::seq_len(base::nrow(nodes))) {
          outcome_graph <- DiagrammeR::add_node(
            outcome_graph,
            label = nodes[[i,'label']]
          )
        }
        
        shiny::incProgress(1/10)
        
        edges <- reactval$connections |>
          dplyr::left_join(dplyr::select(outcomelist(), origin = outcome, origlab = label), by = "origin") |>
          dplyr::left_join(dplyr::select(outcomelist(), destination = outcome, destlab = label), by = "destination") |>
          dplyr::select(origin = origlab, destination = destlab) |>
          dplyr::mutate(
            origin = stringr::str_replace_all(origin, "_", "\n"),
            destination = stringr::str_replace_all(destination, "_", "\n")
          )
        
        shiny::incProgress(1/10)
        
        for (i in base::seq_len(base::nrow(edges))) {
          outcome_graph <- DiagrammeR::add_edge(
            outcome_graph,
            from = edges$origin[[i]],
            to = edges$destination[[i]]
          )
        }
        
        shiny::incProgress(1/10)
        
        outcome_graph$nodes_df <- outcome_graph$nodes_df |>
          dplyr::mutate(
            shape = nodes$shape,
            width = 0.5,
            height = 0.5,
            fontsize = 8,
            color = base::paste0(nodes$color, "66"),
            fillcolor = base::paste0(nodes$color, "33"),
            fontcolor = "black",
            penwidth = 3,
            tooltip = nodes$tooltip,
            URL = nodes$URL,
            outcome = nodes$outcome
          )
        
        shiny::incProgress(1/10)
        
        base::set.seed(input$defseedoutcomes)
        
        if (!base::is.null(input$slctegooutcome)){
          x <- input$slctegooutcome
          y <- outcome_graph$nodes_df |> dplyr::filter(outcome %in% x)
          y <- y$id
          tmp <- outcome_graph$edges_df |>
            dplyr::filter(from %in% y | to %in% y)
          z <- base::unique(c(tmp$from, tmp$to))
          outcome_graph$nodes_df <- dplyr::filter(outcome_graph$nodes_df, id %in% z)
          outcome_graph$edges_df <- outcome_graph$edges_df |>
            dplyr::filter(from %in% z, to %in% z)
        } else {
          outcome_graph <- outcome_graph
        }
        
        shiny::incProgress(1/10)
        
        layout_basis <- DiagrammeR::to_igraph(outcome_graph)
        
        layout <- base::switch(
          input$slctlayoutoutcomes,
          dh = igraph::layout_with_dh(layout_basis),
          drl = igraph::layout_with_drl(layout_basis),
          fr = igraph::layout_with_fr(layout_basis),
          graphopt = igraph::layout_with_graphopt(layout_basis),
          kk = igraph::layout_with_kk(layout_basis)
        )
        
        shiny::incProgress(1/10)
        
        outcome_graph$nodes_df$x <- layout[,1] / input$defscalingoutcomes
        outcome_graph$nodes_df$y <- layout[,2] / input$defscalingoutcomes
        
        outcome_centrality <- tibble::tibble(
          outcome = igraph::vertex_attr(layout_basis, "outcome"),
          degree = igraph::degree(layout_basis, mode = "all"),
          indegree = igraph::degree(layout_basis, mode = "in"),
          outdegree = igraph::degree(layout_basis, mode = "out"),
          closeness = base::round(igraph::closeness(layout_basis, mode = "all")*100,2),
          incloseness = base::round(igraph::closeness(layout_basis, mode = "in")*100,2),
          outcloseness = base::round(igraph::closeness(layout_basis, mode = "out")*100,2),
          betweenness = base::round(igraph::betweenness(layout_basis),2),
          authority = base::round(igraph::authority_score(layout_basis)$vector*100,0),
          hub = base::round(igraph::hub_score(layout_basis)$vector*100,0),
          strength = igraph::strength(layout_basis),
          constraint = base::round(igraph::constraint(layout_basis)*100,0),
          harmonic = base::round(igraph::harmonic_centrality(layout_basis)*10,0),
          eigen = base::round(igraph::eigen_centrality(layout_basis)$vector*100,0),
          power = base::round(igraph::power_centrality(layout_basis)*100,0),
          negpower = base::round(igraph::power_centrality(layout_basis, exponent = -1)*100,0),
        )
        outcome_graph$nodes_df <- outcome_graph$nodes_df |>
          dplyr::left_join(outcome_centrality, by = "outcome")
        
        shiny::incProgress(1/10)
      })
      
      outcome_graph
    })
    
    
    
    output$outcomemap <- shiny::renderUI({
      shiny::req(!base::is.null(outcome_graph()))
      
      shiny::withProgress(message = "Creating the outcome map", {
        shiny::incProgress(1/3)
        outcome_graph <- outcome_graph()
        if ("switchxy" %in% input$outcomeaxes){
          xaxis <- outcome_graph$nodes_df$y
          yaxis <- outcome_graph$nodes_df$x
        } else {
          xaxis <- outcome_graph$nodes_df$x
          yaxis <- outcome_graph$nodes_df$y
        }
        if ("invertx" %in% input$outcomeaxes){
          xaxis <- -xaxis
        }
        if ("inverty" %in% input$outcomeaxes){
          yaxis <- -yaxis
        }
        shiny::incProgress(1/3)
        outcome_graph$nodes_df$x <- xaxis
        outcome_graph$nodes_df$y <- yaxis
        graph <- DiagrammeR::render_graph(
          outcome_graph,
          width = "1250px",
          height = "650px",
          as_svg = TRUE
        )
        shiny::incProgress(1/3)
      })
      
      graph
    })
    
    
    
    # Display statistics
    output$outcometable <- DT::renderDataTable({
      shiny::req(!base::is.null(outcome_graph()))
      outcome_graph()$nodes_df |>
        dplyr::select(
          label, indegree, outdegree, closeness, betweenness, authority,
          hub, strength, constraint, harmonic, eigen, power, negpower
        )
    })
    
    
    
    
    
    # ACTIVITIES ###############################################################
    
    
    # ACTIVITY EDITION #########################################################
    
    
    
    # Create activities and edit attributes specific to a language in a table.
    activitylist <- shiny::reactive({
      shiny::req(!base::is.na(pathfile()))
      shiny::req(!base::is.null(input$slctlang))
      shiny::req(!base::is.null(reactval$activities))
      labels <- reactval$actlabels |>
        dplyr::filter(language == input$slctlang) |>
        dplyr::select(-language)
      reactval$activities |>
        dplyr::left_join(labels, by = "activity") |>
        dplyr::select(activity, order, label, description, lmsid, URL) |>
        dplyr::mutate(order = base::as.numeric(order)) |>
        dplyr::arrange(order)
    })
    
    
    
    output$editactivities <- rhandsontable::renderRHandsontable({
      shiny::req(!base::is.null(activitylist()))
      activitylist() |>
        rhandsontable::rhandsontable(
          height = 500, width = "100%", rowHeaders = NULL, stretchH = "all"
        ) |>
        rhandsontable::hot_cols(
          colWidths = c("10%","5%","25%","40%","10%","10%")
        ) |>
        rhandsontable::hot_col(1, readOnly = TRUE) |>
        rhandsontable::hot_context_menu(
          allowRowEdit = FALSE, allowColEdit = FALSE
        )
    })
    
    
    
    # Update the language specific attributes of activities
    shiny::observeEvent(input$updateactivities, {
      shiny::req(!base::is.null(input$editactivities))
      
      updated_labels <- rhandsontable::hot_to_r(input$editactivities) |>
        dplyr::mutate_all(base::as.character) |>
        dplyr::filter(!base::is.na(activity), activity != "")
      
      updated_activities <- updated_labels |>
        dplyr::select(activity, order)
      
      updated_labels <- updated_labels |>
        dplyr::select(-order)
      
      actlabels <- reactval$actlabels
      activities <- reactval$activities
      
      checkids <- updated_labels |>
        dplyr::group_by(activity) |>
        dplyr::count() |>
        dplyr::filter(n > 1)
      
      misslabel <- updated_labels |>
        dplyr::filter(base::is.na(label) | label == "")
      
      if (base::nrow(checkids) == 0 & base::nrow(misslabel) == 0){
        
        updated_actlabels <- updated_labels |>
          dplyr::mutate(language = input$slctlang) |>
          dplyr::select(activity, language, label, description, lmsid, URL) |>
          base::unique()
        
        all_languages <- reactval$languages$langiso
        
        actlabels <- updated_labels |>
          dplyr::select(activity) |>
          base::unique() |>
          dplyr::mutate(language = base::list(all_languages)) |>
          tidyr::unnest(language) |>
          dplyr::left_join(actlabels, by = c("activity", "language")) |>
          dplyr::group_by(activity) |>
          tidyr::nest() |>
          dplyr::mutate(data = purrr::map(data, function(x,y){
            replacements <- dplyr::filter(x, language == y)
            tidyr::replace_na(x, base::list(
              label = replacements$label[1],
              description = replacements$description[1],
              lmsid = replacements$lmsid[1],
              URL = replacements$URL[1]
            ))
          }, y = input$slctlang)) |>
          tidyr::unnest("data") |>
          dplyr::ungroup() |>
          dplyr::filter(language != input$slctlang) |>
          dplyr::bind_rows(updated_actlabels) |>
          base::unique()
        
        reactval$actlabels <- actlabels
        
        activities <- activities |>
          dplyr::select(-order) |>
          dplyr::left_join(updated_activities, by = "activity") |>
          dplyr::select(activity, order, dplyr::everything())
        
        reactval$activities <- activities
        
        shinyalert::shinyalert(
          "Activity labels updated",
          "Activity orders and labels have been updated. Refresh the map to see the change and save the file to keep the changes.",
          type = "success"
        )
        
      } else if (base::nrow(checkids) > 0) {
        shinyalert::shinyalert(
          "Duplicated IDs",
          base::paste0(
            "The ID(s) ",
            base::paste(checkids$activity, collapse = ", "),
            " is/are duplicated. They should be unique."
          ),
          type = "error"
        )
      } else if (base::nrow(misslabel) > 0){
        shinyalert::shinyalert(
          "Missing label",
          base::paste0(
            "The ID(s) ",
            base::paste(misslabel$activity, collapse = ", "),
            " has (have) no label; please define a unique label."
          ),
          type = "error"
        )
      }
    })
    
    
    
    # Edit the attributes common to all languages.
    list_of_activities <- shiny::reactive({
      shiny::req(!base::is.null(activitylist()))
      actchoices <- activitylist()$activity
      base::names(actchoices) <- activitylist()$label
      actchoices
    })
    
    selected_activity <- pathR::select_activity_server("slctact", list_of_activities)
    
    output$editattributes <- shiny::renderUI({
      shiny::req(!base::is.null(activitylist()))
      shiny::req(!base::is.null(outcomelist()))
      shiny::req(!base::is.null(selected_activity()))
      shiny::req(!base::is.null(input$slctlang))
      
      actlabels <- activitylist() |>
        dplyr::filter(activity == selected_activity()) |>
        dplyr::mutate(label = stringr::str_replace_all(label, "_", " "))
      
      activity <- reactval$activities |>
        dplyr::filter(activity == selected_activity())
      
      types <- c("Slide","Video","Textbook","Note","Tutorial","Game","Case","Test")
      acttype <- activity$type[[1]]
      
      outcomes <- c(NA, outcomelist()$outcome)
      base::names(outcomes) <- c("Not assigned yet", stringr::str_replace_all(outcomelist()$label, "_", " "))
      
      preslctoutcomes <- activity$outcomes[1] |>
        stringr::str_split(" ", simplify = TRUE) |>
        base::as.character()
      
      subgroups <- reactval$activities$subgroup |>
        base::unlist() |>
        base::as.character() |>
        stats::na.omit() |>
        base::unique()
      
      preslctsubgroup <- activity$subgroup[1]
      
      attrchoices <- reactval$attributes |>
        dplyr::filter(language == input$slctlang) |>
        dplyr::mutate(label = base::paste0("<i class='fa fa-",icon,"'> | ",value,"</i>")) |>
        dplyr::select(attribute, label, value)
      
      tmp <- attrchoices |>
        dplyr::filter(attribute == "requirement")
      reqchoices <- base::unlist(tmp$value)
      base::names(reqchoices) <- base::unlist(tmp$label)
      
      tmp <- attrchoices |>
        dplyr::filter(attribute == "level")
      levchoices <- base::unlist(tmp$value)
      base::names(levchoices) <- base::unlist(tmp$label)
      
      tmp <- attrchoices |>
        dplyr::filter(attribute == "time_space")
      timspachoices <- base::unlist(tmp$value)
      base::names(timspachoices) <- base::unlist(tmp$label)
      
      tmp <- attrchoices |>
        dplyr::filter(attribute == "social")
      socchoices <- base::unlist(tmp$value)
      base::names(socchoices) <- base::unlist(tmp$label)
      
      base::list(
        shiny::h2(actlabels$label[[1]]),
        shiny::tags$hr(),
        shiny::fluidRow(
          shiny::column(
            2,
            shiny::numericInput(
              ns("deforder"),
              "Order:",
              value = activity$order[1],
              width = "100%"
            )
          ),
          shiny::column(
            2,
            shiny::selectInput(
              ns("deftype"),
              "Type:",
              choices = types,
              selected = acttype,
              multiple = FALSE,
              width = "100%"
            )
          ),
          shiny::column(
            6,
            shiny::selectInput(
              ns("defoutcomes"),
              "Outcomes:",
              choices = outcomes,
              selected = preslctoutcomes,
              multiple = TRUE,
              width = "100%"
            )
          ),
          shiny::column(
            2,
            shiny::selectizeInput(
              ns("defsubgroup"),
              "Subgroup:",
              choices = subgroups,
              selected = preslctsubgroup,
              multiple = FALSE,
              options = base::list(create = TRUE),
              width = "100%"
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(
            3,
            shinyWidgets::radioGroupButtons(
              inputId = ns("defreq"),
              label = "Requirement:", 
              choices = reqchoices,
              selected = activity$requirement[1],
              justified = TRUE,
              status = "danger",
              size = "normal",
              checkIcon = base::list(yes = shiny::icon("check")),
              direction = "vertical"
            )
          ),
          shiny::column(
            3,
            shinyWidgets::radioGroupButtons(
              inputId = ns("deflevel"),
              label = "Level:", 
              choices = levchoices,
              selected = activity$level[1],
              justified = TRUE,
              status = "warning",
              size = "normal",
              checkIcon = base::list(yes = shiny::icon("check")),
              direction = "vertical"
            )
          ),
          shiny::column(
            3,
            shinyWidgets::radioGroupButtons(
              inputId = ns("deftimespace"),
              label = "Time and Space:", 
              choices = timspachoices,
              selected = activity$time_space[1],
              justified = TRUE,
              status = "info",
              size = "normal",
              checkIcon = base::list(yes = shiny::icon("check")),
              direction = "vertical"
            )
          ),
          shiny::column(
            3,
            shinyWidgets::radioGroupButtons(
              inputId = ns("defsocial"),
              label = "Social:", 
              choices = socchoices,
              selected = activity$social[1],
              justified = TRUE,
              status = "success",
              size = "normal",
              checkIcon = base::list(yes = shiny::icon("check")),
              direction = "vertical"
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(
            9,
            shiny::sliderInput(
              ns("defweight"), "Weigth:",
              min = 0., max = 1, step = 0.01,
              value = base::as.numeric(activity$weigth[1]),
              width = "100%"
            )
          ),
          shiny::column(
            3,
            shiny::numericInput(
              ns("defduration"), "Duration:",
              min = 5, max = 480, step = 5,
              value = base::as.numeric(activity$duration[1]),
              width = "100%"
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(
            6,
            shinyWidgets::airDatepickerInput(
              inputId = ns("defstart"),
              label = "Start:", 
              value = base::as.Date(activity$start[1]),
              width = "100%"
            )
          ),
          shiny::column(
            6,
            shinyWidgets::airDatepickerInput(
              inputId = ns("defend"),
              label = "End:", 
              value = base::as.Date(activity$end[1]),
              width = "100%"
            )
          )
        )
      )
    })
    
    shiny::observeEvent(input$updateactattr, {
      shiny::req(!base::is.null(reactval$activities))
      shiny::req(!base::is.null(selected_activity()))
      shiny::req(!base::is.null(input$deforder))
      shiny::req(!base::is.null(input$deftype))
      shiny::req(!base::is.null(input$defoutcomes))
      shiny::req(!base::is.null(input$defsubgroup))
      shiny::req(!base::is.null(input$defreq))
      shiny::req(!base::is.null(input$deftimespace))
      shiny::req(!base::is.null(input$defsocial))
      shiny::req(!base::is.null(input$defweight))
      
      update_activity <- tibble::tibble(
        activity = selected_activity(),
        order = input$deforder,
        type = input$deftype,
        outcomes = base::paste(input$defoutcomes, collapse = " "),
        subgroup = input$defsubgroup,
        requirement = input$defreq,
        weigth = input$defweight,
        level = input$deflevel,
        time_space = input$deftimespace,
        social = input$defsocial,
        duration = input$defduration,
        start = input$defstart,
        end = input$defend
      ) |>
        dplyr::mutate_all(base::as.character)
      
      activities <- reactval$activities |>
        dplyr::filter(activity != selected_activity()) |>
        dplyr::bind_rows(update_activity)
      
      reactval$activities <- activities
      
      shinyalert::shinyalert(
        "Activity attributes updated",
        "The attributes of this activity have been updated. Refresh the map to see the change and save the file to keep the changes.",
        type = "success"
      )
    })
    
    
    
    # Button to create a new activity
    shiny::observeEvent(input$newactivity, {
      
      shiny::req(!base::is.null(outcomelist()))
      shiny::req(!base::is.null(reactval$activities))
      
      types <- c("Slide","Video","Textbook","Note","Tutorial","Game","Case","Test")
      outcomes <- c(NA, outcomelist()$outcome)
      base::names(outcomes) <- c("Not assigned yet", stringr::str_replace_all(outcomelist()$label, "_", " "))
      subgroups <- c(NA, base::unique(reactval$activities$subgroup))
      attrchoices <- reactval$attributes |>
        dplyr::filter(language == input$slctlang) |>
        dplyr::mutate(label = base::paste0("<i class='fa fa-",icon,"'> | ",value,"</i>")) |>
        dplyr::select(attribute, label, value)
      tmp <- attrchoices |>
        dplyr::filter(attribute == "requirement")
      reqchoices <- base::unlist(tmp$value)
      base::names(reqchoices) <- base::unlist(tmp$label)
      tmp <- attrchoices |>
        dplyr::filter(attribute == "level")
      levchoices <- base::unlist(tmp$value)
      base::names(levchoices) <- base::unlist(tmp$label)
      tmp <- attrchoices |>
        dplyr::filter(attribute == "time_space")
      timspachoices <- base::unlist(tmp$value)
      base::names(timspachoices) <- base::unlist(tmp$label)
      tmp <- attrchoices |>
        dplyr::filter(attribute == "social")
      socchoices <- base::unlist(tmp$value)
      base::names(socchoices) <- base::unlist(tmp$label)
      
      shiny::showModal(shiny::modalDialog(
        title = "Add a new activity",
        shiny::fluidRow(
          shiny::column(
            2,
            shiny::textInput(ns("newactid"), "ID:", value = "", width = "100%")
          ),
          shiny::column(
            7,
            shiny::textInput(ns("newactlab"), "Label:", value = "Write a label here", width = "100%")
          ),
          shiny::column(
            1,
            shiny::numericInput(
              ns("newactorder"), "Order:",
              value = 1+base::nrow(reactval$activities),
              width = "100%"
            )
          ),
          shiny::column(
            2,
            shiny::selectInput(
              ns("newacttype"), "Type:",
              choices = types, selected = types[1],
              multiple = FALSE, width = "100%"
            )
          )
        ),
        
        shiny::fluidRow(
          shiny::column(
            7,
            shiny::textAreaInput(ns("newactdesc"), "Description:", value = "", width = "100%")
          ),
          shiny::column(
            2,
            shiny::textInput(ns("newactlmsid"), "LMSID:", value = "", width = "100%")
          ),
          shiny::column(
            3,
            shiny::textInput(ns("newacturl"), "URL:", value = "", width = "100%")
          )
        ),
        
        shiny::fluidRow(
          shiny::column(
            8,
            shiny::selectInput(
              ns("newactoutcome"), "Outcomes:",
              choices = outcomes,
              multiple = TRUE, width = "100%"
            )
          ),
          shiny::column(
            4,
            shiny::selectizeInput(
              ns("newactsubgroup"), "Subgroup:",
              choices = subgroups,
              multiple = FALSE,
              options = base::list(create = TRUE),
              width = "100%"
            )
          )
        ),
        
        shiny::fluidRow(
          shiny::column(
            3,
            shinyWidgets::radioGroupButtons(
              inputId = ns("newactreq"),
              label = "Requirement:", 
              choices = reqchoices,
              selected = reqchoices[1],
              justified = TRUE,
              status = "danger",
              size = "normal",
              checkIcon = base::list(yes = shiny::icon("check")),
              direction = "vertical"
            )
          ),
          shiny::column(
            3,
            shinyWidgets::radioGroupButtons(
              inputId = ns("newactlevel"),
              label = "Level:", 
              choices = levchoices,
              selected = levchoices[1],
              justified = TRUE,
              status = "warning",
              size = "normal",
              checkIcon = base::list(yes = shiny::icon("check")),
              direction = "vertical"
            )
          ),
          shiny::column(
            3,
            shinyWidgets::radioGroupButtons(
              inputId = ns("newacttimespace"),
              label = "Time and Space:", 
              choices = timspachoices,
              selected = timspachoices[1],
              justified = TRUE,
              status = "info",
              size = "normal",
              checkIcon = base::list(yes = shiny::icon("check")),
              direction = "vertical"
            )
          ),
          shiny::column(
            3,
            shinyWidgets::radioGroupButtons(
              inputId = ns("newactsocial"),
              label = "Social:", 
              choices = socchoices,
              selected = socchoices[1],
              justified = TRUE,
              status = "success",
              size = "normal",
              checkIcon = base::list(yes = shiny::icon("check")),
              direction = "vertical"
            )
          )
        ),
        
        shiny::fluidRow(
          shiny::column(
            9,
            shiny::sliderInput(
              ns("newactweight"), "Weigth:",
              min = 0., max = 1, step = 0.01,
              value = 0, width = "100%"
            )
          ),
          shiny::column(
            3,
            shiny::numericInput(
              ns("newactduration"), "Duration:",
              min = 5, max = 480, step = 5,
              value = 0, width = "100%"
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(
            6,
            shinyWidgets::airDatepickerInput(
              inputId = ns("newactstart"),
              label = "Start:", width = "100%"
            )
          ),
          shiny::column(
            6,
            shinyWidgets::airDatepickerInput(
              inputId = ns("newactend"),
              label = "End:", width = "100%"
            )
          )
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("addactivity"), "OK")
        )
      ))
    })
    
    
    
    shiny::observeEvent(input$addactivity, {
      if (input$newactid == ""){
        shinyalert::shinyalert(
          title = "Missing ID!",
          text = "You must give the activity an ID.",
          type = "error"
        )
      } else if (input$newactid %in% reactval$activities$activity) {
        shinyalert::shinyalert(
          title = "Non unique ID!",
          text = "The ID you gave to the activity is already assigned to another activity.",
          type = "error"
        )
      } else {
        shiny::removeModal()
        
        newactivities <- tibble::tibble(
          activity = input$newactid,
          order = input$newactorder,
          type = input$newacttype,
          outcomes = base::paste(input$newactoutcome, sep = " "),
          subgroup = input$newactsubgroup,
          requirement = input$newactreq,
          weigth = input$newactweight,
          level = input$newactlevel,
          time_space = input$newacttimespace,
          social = input$newactsocial,
          duration = input$newactduration,
          start = input$newactstart,
          end = input$newactend
        ) |>
          dplyr::mutate_all(base::as.character)
        
        newactlabels <- tibble::tibble(
          activity = input$newactid,
          order = input$newactorder,
          language = reactval$languages$langiso,
          label = input$newactlab,
          description = input$newactdesc,
          lmsid = input$newactlmsid,
          URL = input$newacturl
        ) |>
          dplyr::mutate_all(base::as.character)
        
        activities <- reactval$activities |>
          dplyr::bind_rows(newactivities) |>
          dplyr::arrange(order)
        actlabels <- reactval$actlabels |>
          dplyr::bind_rows(newactlabels) |>
          dplyr::arrange(order) |>
          dplyr::select(-order)
        
        reactval$activities <- activities
        reactval$actlabels <- actlabels
        
        shinyalert::shinyalert(
          title = "New activity added",
          text = "The new activity has been successfully added to the database.",
          type = "success"
        )
      }
    })
    
    
    
    # Button to change the ID of an activity in all databases
    shiny::observeEvent(input$changeactid, {
      shiny::req(!base::is.null(reactval$activities))
      activityids <- reactval$activities$activity
      shiny::showModal(shiny::modalDialog(
        title = "Change the activity id",
        shiny::fluidRow(
          shiny::column(
            2,
            shiny::selectInput(ns("fromactid"), "From this ID:", choices = activityids, width = "100%")
          ),
          shiny::column(
            6,
            shiny::textInput(ns("toactid"), "To thios ID:", value = "NEWID", width = "100%")
          )
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("replaceactid"), "OK")
        )
      ))
    })
    
    
    
    shiny::observeEvent(input$replaceactid, {
      
      shiny::req(!base::is.null(input$fromactid))
      shiny::req(!base::is.null(input$toactid))
      
      fromid <- input$fromactid
      toid <- input$toactid
      
      activities <- reactval$activities
      actlabels <- reactval$actlabels
      
      if (!(toid %in% reactval$activities$activity)){
        
        shiny::removeModal()
        
        activities$activity <- stringr::str_replace_all(
          activities$activity, fromid, toid
        )
        actlabels$activity <- stringr::str_replace_all(
          actlabels$activity, fromid, toid
        )
        
        reactval$activities <- activities
        reactval$actlabels <- actlabels
        
        shinyalert::shinyalert(
          title = "Activity ID replaced",
          text = "The activity ID has been replaced in all databases. Please save the map to definitely record your changes.",
          type = "success"
        )
      } else {
        shinyalert::shinyalert(
          title = "Activity ID already existing",
          text = "You cannot replace the ID of an activity by an ID which is already existing in the database.",
          type = "error"
        )
      }
    })
    
    
    
    # Button to delete an outcome
    shiny::observeEvent(input$deleteactivity, {
      shiny::req(!base::is.null(reactval$outcomes))
      activityids <- reactval$activities$activity
      shiny::showModal(shiny::modalDialog(
        title = "Delete an activity",
        shiny::selectInput(ns("activitytodelete"), "Activity to delete:", choices = activityids, width = "100%"),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("removeactivity"), "OK")
        )
      ))
    })
    
    
    
    shiny::observeEvent(input$removeactivity, {
      
      shiny::removeModal()
      
      activities <- reactval$activities |>
        dplyr::filter(activity != input$activitytodelete)
      
      actlabels <- reactval$actlabels |>
        dplyr::filter(activity != input$activitytodelete)
      
      reactval$activities <- activities
      reactval$actlabels <- actlabels
      
      shinyalert::shinyalert(
        title = "Activity deleted",
        text = "The activity has been deleted from the database.",
        type = "success"
      )
    })
    
    
    
    
    
    # PATH GENERATION ##########################################################
    
    
    
    # ACTIVITY VISUALIZATION ###################################################
    
    
    activity_labels <- shiny::reactive({
      input$refreshmapact
      shiny::req(!base::is.null(reactval$outcomes))
      outcomes <- dplyr::select(reactval$outcomes, outcome, color)
      req <- reactval$attributes |>
        dplyr::filter(attribute == "requirement", language == input$slctlang) |>
        dplyr::select(requirement = value, reqlab = label)
      lev <- reactval$attributes |>
        dplyr::filter(attribute == "level", language == input$slctlang) |>
        dplyr::select(level = value, levlab = label)
      ts <- reactval$attributes |>
        dplyr::filter(attribute == "time_space", language == input$slctlang) |>
        dplyr::select(time_space = value, tslab = label)
      soc <- reactval$attributes |>
        dplyr::filter(attribute == "social", language == input$slctlang) |>
        dplyr::select(social = value, soclab = label)
      
      reactval$actlabels |>
        dplyr::filter(language == input$slctlang) |>
        dplyr::left_join(reactval$activities, by = "activity") |>
        dplyr::mutate(
          outcome = purrr::map_chr(outcomes, function(x){
            y <- stringr::str_split(x, " ", simplify = TRUE) |>
              base::as.character()
            y[1]
          }),
          order = base::as.numeric(order)
        ) |>
        dplyr::left_join(outcomes, by = "outcome") |>
        tidyr::replace_na(base::list(color = "#FFFFFF")) |>
        dplyr::left_join(req, by = "requirement") |>
        dplyr::left_join(lev, by = "level") |>
        dplyr::left_join(ts, by = "time_space") |>
        dplyr::left_join(soc, by = "social") |>
        dplyr::select(-language) |>
        tidyr::replace_na(base::list(end = "-")) |>
        dplyr::arrange(order)
    })
    
    output$egoactivityselection <- shiny::renderUI({
      shiny::req(!base::is.null(activity_labels()))
      
      activities <- activity_labels() |>
        dplyr::select(activity, label, color, outcome, order) |>
        dplyr::group_by(activity) |>
        dplyr::slice_head(n = 1) |>
        dplyr::ungroup() |>
        dplyr::mutate(label = stringr::str_replace_all(label, "_", " ")) |>
        dplyr::arrange(order)
      
      activitychoices <- activities$activity
      base::names(activitychoices) <- activities$label
      activitycolors <- base::paste0(
        "color: #FFFFFF; background: ", activities$color,";"
      )
      
      if (!base::is.null(input$slctegooutcome)){
        slctactivities <- activities |>
          dplyr::filter(outcome %in% input$slctegooutcome) |>
          dplyr::arrange(order) |>
          dplyr::select(activity) |>
          base::unlist()
      } else slctactivities <- NULL
      
      shinyWidgets::pickerInput(
        inputId = ns("slctegoactivity"),
        label = "Focus on activities:", 
        choices = activitychoices,
        selected = slctactivities,
        choicesOpt = base::list(style = activitycolors),
        multiple = TRUE,
        width = "100%"
      )
    })
    
    
    
    paths <- shiny::reactive({
      shiny::req(!base::is.null(reactval$connections))
      shiny::req(!base::is.null(reactval$activities))
      pathR::create_path(reactval$activities, reactval$connections)
    })
    
    
    
    activity_graph <- shiny::eventReactive(input$refreshactmap, {
      shiny::req(!base::is.null(activity_labels()))
      shiny::req(!base::is.null(paths()))
      
      shiny::withProgress(message = "Building the network of activities...", {
        shiny::incProgress(1/10)
        
        activity_graph <- DiagrammeR::create_graph()
        
        nodes <- activity_labels() |>
          dplyr::filter(activity %in% c(paths()$origin, paths()$destination)) |>
          dplyr::group_by(activity) |>
          dplyr::slice_head(n = 1) |>
          tidyr::replace_na(base::list(description = " ")) |>
          dplyr::mutate(weigth = base::as.numeric(weigth)) |>
          dplyr::mutate(
            shape = dplyr::case_when(
              type == "Slide" ~ "square",
              type == "Video" ~ "rectangle",
              type == "Textbook" ~ "ellipse",
              type == "Note" ~ "circle",
              type == "Tutorial" ~ "hexagon",
              type == "Game" ~ "septagon",
              type == "Case" ~ "octagon",
              type == "Test" ~ "star",
              TRUE ~ "point"
            ),
            shapesize = dplyr::case_when(
              requirement == "NEC" ~ 1.0,
              requirement == "REC" ~ 0.9,
              TRUE ~ 0.8
            ),
            alpha = dplyr::case_when(
              requirement == "OPT" ~ "11",
              requirement == "REC" ~ "33",
              TRUE ~ "77"
            ),
            peripheries = dplyr::case_when(
              time_space == "AOL" ~ 1,
              time_space == "SOL" ~ 2,
              TRUE ~ 3
            ),
            penwidth = 0.5 + weigth * 10,
            tooltip = base::paste0(
              stringr::str_replace_all(label, "_", " "), "\n\n",
              description, "\n\n",
              type, " | ", levlab, "\n",
              reqlab, " | ", base::round(weigth * 100,0), "%\n",
              soclab, " | ", tslab, "\n",
              duration, " minutes | ", end, "\n\n",
              activity
            )
          ) |>
          dplyr::mutate(
            label = stringr::str_replace_all(label, "_", "\n"),
            bordercolor = base::paste(color, "CC", sep = ""),
            fillcolor = base::paste(color, alpha, sep = "")
          ) |>
          dplyr::select(
            activity, requirement, label, shape, shapesize, peripheries, penwidth, bordercolor, fillcolor, fontcolor, tooltip, URL
          )
        
        shiny::incProgress(1/10)
        
        for (i in base::seq_len(base::nrow(nodes))) {
          activity_graph <- DiagrammeR::add_node(
            activity_graph,
            label = nodes[[i,'label']]
          )
        }
        
        shiny::incProgress(1/10)
        
        edges <- paths() |>
          dplyr::left_join(dplyr::select(nodes, origin = activity, laborig = label, reqorig = requirement), by = "origin") |>
          dplyr::left_join(dplyr::select(nodes, destination = activity, labdest = label, reqdest = requirement), by = "destination") |>
          dplyr::mutate(
            color = dplyr::case_when(
              condition == "Done" ~ "#0000FF",
              condition == "Not OK" ~ "#990000",
              condition == "OK" ~ "#006600",
              TRUE ~ "#777777"
            ),
            style = dplyr::case_when(
              reqorig == "NEC" & reqdest == "NEC" ~ "solid",
              TRUE ~ "dashed"
            ),
            penwidth = dplyr::case_when(
              style == "solid" & color != "#333333" ~ 5,
              TRUE ~ 2
            )
          ) |>
          dplyr::select(origin = laborig, destination = labdest, color, style, penwidth) |>
          dplyr::mutate(
            origin = stringr::str_replace_all(origin, "_", "\n"),
            destination = stringr::str_replace_all(destination, "_", "\n")
          )
        
        shiny::incProgress(1/10)
        
        shiny::req(base::all(edges$origin %in% nodes$label))
        shiny::req(base::all(edges$destination %in% nodes$label))
        
        for (i in base::seq_len(base::nrow(edges))) {
          activity_graph <- DiagrammeR::add_edge(
            activity_graph,
            from = edges$origin[[i]],
            to = edges$destination[[i]]
          )
        }
        
        shiny::incProgress(1/10)
        
        activity_graph$nodes_df <- activity_graph$nodes_df |>
          dplyr::mutate(
            shape = nodes$shape,
            width = nodes$shapesize,
            height = nodes$shapesize,
            fontsize = 10,
            style = "filled",
            color = nodes$bordercolor,
            fillcolor = nodes$fillcolor,
            fontcolor = "black",
            penwidth = nodes$penwidth,
            tooltip = nodes$tooltip,
            URL = base::paste0(nodes$URL,'/"target="_blank'),
            peripheries = nodes$peripheries,
            activity = nodes$activity
          )
        
        shiny::incProgress(1/10)
        
        activity_graph$edges_df <- activity_graph$edges_df |>
          dplyr::mutate(
            style = edges$style,
            penwidth = edges$penwidth,
            color = edges$color
          )
        
        shiny::incProgress(1/10)
        
        base::set.seed(input$defseedactivities)
        
        if (!base::is.null(input$slctegoactivity)){
          x <- input$slctegoactivity
          y <- activity_graph$nodes_df |> dplyr::filter(activity %in% x)
          y <- y$id
          tmp <- activity_graph$edges_df |>
            dplyr::filter(from %in% y | to %in% y)
          z <- base::unique(c(tmp$from, tmp$to))
          activity_graph$nodes_df <- dplyr::filter(activity_graph$nodes_df, id %in% z)
          activity_graph$edges_df <- activity_graph$edges_df |>
            dplyr::filter(from %in% z, to %in% z)
          
        } else {
          activity_graph <- activity_graph
        }
        
        shiny::incProgress(1/10)
        
        layout_basis <- DiagrammeR::to_igraph(activity_graph)
        
        shiny::incProgress(1/10)
        
        layout <- base::switch(
          input$slctlayoutactivities,
          dh = igraph::layout_with_dh(layout_basis),
          drl = igraph::layout_with_drl(layout_basis),
          fr = igraph::layout_with_fr(layout_basis),
          graphopt = igraph::layout_with_graphopt(layout_basis),
          kk = igraph::layout_with_kk(layout_basis)
        )
        
        shiny::incProgress(1/10)
        
        activity_graph$nodes_df$x <- layout[,1] / input$defscalingactivities
        activity_graph$nodes_df$y <- layout[,2] / input$defscalingactivities
        
        activity_centrality <- tibble::tibble(
          activity = igraph::vertex_attr(layout_basis, "activity"),
          degree = igraph::degree(layout_basis, mode = "all"),
          indegree = igraph::degree(layout_basis, mode = "in"),
          outdegree = igraph::degree(layout_basis, mode = "out"),
          closeness = base::round(igraph::closeness(layout_basis, mode = "all")*100,2),
          incloseness = base::round(igraph::closeness(layout_basis, mode = "in")*100,2),
          outcloseness = base::round(igraph::closeness(layout_basis, mode = "out")*100,2),
          betweenness = base::round(igraph::betweenness(layout_basis),2),
          authority = base::round(igraph::authority_score(layout_basis)$vector*100,0),
          hub = base::round(igraph::hub_score(layout_basis)$vector*100,0),
          strength = igraph::strength(layout_basis),
          constraint = base::round(igraph::constraint(layout_basis)*100,0),
          harmonic = base::round(igraph::harmonic_centrality(layout_basis)*10,0),
          eigen = base::round(igraph::eigen_centrality(layout_basis)$vector*100,0),
          power = base::round(igraph::power_centrality(layout_basis)*100,0),
          negpower = base::round(igraph::power_centrality(layout_basis, exponent = -1)*100,0),
        )
        activity_graph$nodes_df <- activity_graph$nodes_df |>
          dplyr::left_join(activity_centrality, by = "activity")
        
        shiny::incProgress(1/10)
      })
      
      activity_graph
    })
    
    output$activitymap <- shiny::renderUI({
      shiny::req(!base::is.null(activity_graph()))
      shiny::withProgress(message = "Creating a visualization of the activity map", {
        
        shiny::incProgress(1/5)
        
        activity_graph <- activity_graph()
        if ("switchxy" %in% input$activityaxes){
          xaxis <- activity_graph$nodes_df$y
          yaxis <- activity_graph$nodes_df$x
        } else {
          xaxis <- activity_graph$nodes_df$x
          yaxis <- activity_graph$nodes_df$y
        }
        
        shiny::incProgress(1/5)
        
        if ("invertx" %in% input$activityaxes){
          xaxis <- -xaxis
        }
        if ("inverty" %in% input$activityaxes){
          yaxis <- -yaxis
        }
        shiny::incProgress(1/5)
        
        activity_graph$nodes_df$x <- xaxis
        activity_graph$nodes_df$y <- yaxis
        
        shiny::incProgress(1/5)
        graph <- DiagrammeR::render_graph(
          activity_graph,
          width = "1250px",
          height = "650px",
          as_svg = TRUE
        )
        shiny::incProgress(1/5)
      })
      
      graph
    })
    
    output$activitytable <- DT::renderDataTable({
      shiny::req(!base::is.null(activity_graph()))
      activity_graph()$nodes_df |>
        dplyr::select(
          label, indegree, outdegree, closeness, betweenness, authority,
          hub, strength, constraint, harmonic, eigen, power, negpower
        )
    })
    
    
    
    
    # GENERATE PATHS ###########################################################
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    # ANALYSES OF DESIGN #######################################################
    
    prepactivities <- shiny::reactive({
      shiny::req(!base::is.null(outcomelist()))
      shiny::req(base::nrow(outcomelist()) > 1)
      shiny::req(!base::is.null(activity_labels()))
      
      outcomes <- outcomelist() |>
        dplyr::mutate(
          order = -base::as.numeric(order),
          label = stringr::str_replace_all(label, "_", " "),
          label = base::as.factor(label),
          label = forcats::fct_reorder(label, order, base::mean)
        )
      
      activity_labels() |>
        dplyr::mutate(
          order = base::as.numeric(order),
          label = stringr::str_replace_all(label, "_", " "),
          label = base::as.factor(label),
          label = forcats::fct_reorder(label, order, base::mean)
        ) |>
        dplyr::mutate(object = stringr::str_split(lmsid, pattern = " ")) |>
        tidyr::unnest(object) |>
        dplyr::mutate(outcome = stringr::str_split(outcomes, pattern = " ")) |>
        tidyr::unnest(outcome) |>
        dplyr::left_join(dplyr::select(outcomes, outcome, outcomelab = label), by = "outcome") |>
        dplyr::select(
          object, activity, activitylab = label, outcome = outcomelab, outcomecolor = color,
          type, requirement, level, time_space, social, duration, start, end
        ) |>
        tidyr::replace_na(base::list(
          start = base::as.character(base::Sys.Date()),
          end = base::as.character(base::Sys.Date())
        )) |>
        dplyr::mutate(
          type = base::factor(type, levels = c("Slide","Video","Textbook","Note","Tutorial","Game","Case","Test")),
          requirement = base::factor(requirement, levels = c("NEC","REC","OPT")),
          level = base::factor(level, levels = c("NOV","APP","PRO","MAS","EXP")),
          time_space = base::factor(time_space, levels = c("SOS","SOL","AOL")),
          social = base::factor(social, levels = c("GP","TM","IND")),
          duration = base::as.numeric(duration),
          start = base::as.Date(start),
          startweek = base::as.factor(lubridate::week(start)),
          startday = lubridate::as_date(start),
          startorder = base::as.numeric(start),
          end = base::as.Date(end),
          endweek = base::as.factor(lubridate::week(end)),
          endday = lubridate::as_date(end),
          endorder = base::as.numeric(end)
        ) |>
        dplyr::mutate(
          startweek = forcats::fct_reorder(startweek, startorder, base::mean),
          endweek = forcats::fct_reorder(endweek, endorder, base::mean)
        ) |>
        dplyr::select(-startorder, -endorder) |>
        base::unique()
    })
    
    output$design_selections <- shiny::renderUI({
      shiny::req(!base::is.null(prepactivities()))
      shiny::fluidRow(
        shiny::column(3, shinyWidgets::virtualSelectInput(
          ns("desfilttype"), "Types:",
          choices = base::levels(prepactivities()$type),
          selected = base::levels(prepactivities()$type),
          multiple = TRUE, width = "100%"
        )),
        shiny::column(2, shinyWidgets::virtualSelectInput(
          ns("desfiltreq"), "Requirements:",
          choices = base::levels(prepactivities()$requirement),
          selected = base::levels(prepactivities()$requirement),
          multiple = TRUE, width = "100%"
        )),
        
        shiny::column(1, shinyWidgets::virtualSelectInput(
          ns("desfiltlev"), "Levels:",
          choices = base::levels(prepactivities()$level),
          selected = base::levels(prepactivities()$level),
          multiple = TRUE, width = "100%"
        )),
        shiny::column(1, shinyWidgets::virtualSelectInput(
          ns("desfiltts"), "Time-space:",
          choices = base::levels(prepactivities()$time_space),
          selected = base::levels(prepactivities()$time_space),
          multiple = TRUE, width = "100%"
        )),
        shiny::column(1, shinyWidgets::virtualSelectInput(
          ns("desfiltsoc"), "Social:",
          choices = base::levels(prepactivities()$social),
          selected = base::levels(prepactivities()$social),
          multiple = TRUE, width = "100%"
        )),
        
        shiny::column(1, shinyWidgets::virtualSelectInput(
          ns("desslctperiod"), "Period:",
          choices = c("startweek","endweek","startday","endday"),
          selected = "startweek",
          multiple = FALSE, width = "100%"
        )),
        shiny::column(1, shinyWidgets::virtualSelectInput(
          ns("desslctcat"), "Category:",
          choices = c("type","requirement","level","time_space","social","startweek","endweek"),
          selected = "type",
          multiple = FALSE, width = "100%"
        )),
        shiny::column(1, shinyWidgets::virtualSelectInput(
          ns("desslctunit"), "Unit:",
          choices = c("duration","activities"),
          selected = "duration",
          multiple = FALSE, width = "100%"
        )),
        shiny::column(1, shinyWidgets::virtualSelectInput(
          ns("desslctstat"), "Statistic:",
          choices = c("Units","Deciles","Deviations"),
          selected = "Units",
          multiple = FALSE, width = "100%"
        ))
      )
    })
    
    desfiltactivities <- shiny::reactive({
      shiny::req(!base::is.null(prepactivities()))
      shiny::req(!base::is.null(input$desfilttype))
      shiny::req(!base::is.null(input$desfiltreq))
      shiny::req(!base::is.null(input$desfiltlev))
      shiny::req(!base::is.null(input$desfiltts))
      shiny::req(!base::is.null(input$desfiltsoc))
      
      desfiltactivities <- prepactivities() |>
        dplyr::filter(
          type %in% input$desfilttype,
          requirement %in% input$desfiltreq,
          level %in% input$desfiltlev,
          time_space %in% input$desfiltts,
          social %in% input$desfiltsoc
        )
      shiny::req(!base::is.null(input$desslctperiod))
      shiny::req(!base::is.null(input$desslctcat))
      desfiltactivities <- desfiltactivities[,c("outcome","activity",input$desslctperiod,input$desslctcat,"startweek","duration")]
      base::names(desfiltactivities) <- c("outcome","activity","period","category","startweek","duration")
      shiny::req(!base::is.null(input$desslctunit))
      if (input$desslctunit == "duration"){
        desfiltactivities <- desfiltactivities |>
          dplyr::select(outcome, activity, period, category, startweek, duration) |>
          dplyr::mutate(units = duration) |>
          base::unique()
      } else {
        desfiltactivities <- desfiltactivities |>
          dplyr::select(outcome, activity, period, category, startweek, duration) |>
          dplyr::mutate(units = 1) |>
          base::unique()
      }
      
      desfiltactivities
    })
    
    output$design_valueboxes <- shiny::renderUI({
      shiny::req(!base::is.null(desfiltactivities()))
      outcomenbr <- desfiltactivities() |>
        dplyr::select(outcome) |>
        base::unique() |>
        base::nrow()
      activitynbr <- desfiltactivities() |>
        dplyr::select(activity) |>
        base::unique() |>
        base::nrow()
      hoursnbr <- desfiltactivities() |>
        dplyr::select(activity, duration) |>
        base::unique()
      hoursnbr <- base::round(base::sum(hoursnbr$duration, na.rm = TRUE) / 60,1)
      weeknbr <- desfiltactivities() |>
        dplyr::select(startweek) |>
        base::unique() |>
        base::nrow()
      hpw <- base::round(hoursnbr/weeknbr, 1)
      credits <- base::round(hoursnbr / 25,1)
      shiny::fluidRow(
        shinydashboard::valueBox(
          outcomenbr, "Outcomes",
          shiny::icon("bullseye"),
          color = "blue", width = 2
        ),
        shinydashboard::valueBox(
          activitynbr, "Activities",
          shiny::icon("list-check"),
          color = "purple", width = 2
        ),
        shinydashboard::valueBox(
          hoursnbr, "Hours",
          shiny::icon("clock"),
          color = "red", width = 2
        ),
        shinydashboard::valueBox(
          weeknbr, "Weeks",
          shiny::icon("calendar-days"),
          color = "orange", width = 2
        ),
        shinydashboard::valueBox(
          hpw, "Hours per week",
          shiny::icon("stopwatch"),
          color = "green", width = 2
        ),
        shinydashboard::valueBox(
          credits, "ECTS Credits",
          shiny::icon("graduation-cap"),
          color = "aqua", width = 2
        )
      )
    })
    
    output$workload_density <- shiny::renderPlot({
      shiny::req(!base::is.null(desfiltactivities()))
      graphbasis <- desfiltactivities() |>
        dplyr::select(activity, period, category, units) |>
        base::unique() |>
        dplyr::group_by(period, category) |>
        dplyr::summarise(units = base::sum(units))
      graphbasis |>
        dplyr::mutate(category = forcats::fct_rev(category)) |>
        ggplot2::ggplot(ggplot2::aes(x = period, y = units, fill = category)) + 
        ggplot2::geom_bar(stat = "identity", position = "stack") +
        ggplot2::theme_minimal() +
        ggplot2::theme(text = ggplot2::element_text(size = 12))
    })
    
    output$outcomes_heatmap <- shiny::renderPlot({
      shiny::req(!base::is.null(desfiltactivities()))
      shiny::req(!base::is.null(input$desslctstat))
      graphbasis <- desfiltactivities() |>
        dplyr::select(outcome, activity, category, units) |>
        base::unique() |>
        tidyr::complete(category, outcome, fill = base::list(units = 0)) |>
        dplyr::group_by(activity, category, units) |>
        dplyr::mutate(outcomenbr = dplyr::n()) |>
        dplyr::ungroup() |>
        dplyr::mutate(units = units / outcomenbr) |>
        dplyr::group_by(outcome, category) |>
        dplyr::summarise(units = base::sum(units))
      if (input$desslctstat == "Deciles"){
        graphbasis <- graphbasis |>
          dplyr::mutate(units = base::round(dplyr::percent_rank(units)*10,0))
      } else if (input$desslctstat == "Deviations"){
        graphbasis <- graphbasis |>
          dplyr::mutate(units = base::as.numeric(base::scale(units)))
      } else {
        graphbasis <- graphbasis
      }
      graphbasis |>
        ggplot2::ggplot(ggplot2::aes(x = category, y = outcome, fill = units)) + 
        ggplot2::geom_tile(alpha = 0.8) +
        ggplot2::scale_fill_gradient(low="white", high="forestgreen") +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          text = ggplot2::element_text(size = 14),
          axis.text.x = ggplot2::element_text(angle = 60, vjust = 0.5, hjust=0.5),
          panel.grid.major = ggplot2::element_line(colour = "black", linewidth = 0.5, linetype = 1, lineend = "butt")
        )
    }, height = 1000)
    
    
    
    # ANALYSES OF INTERACTIONS #################################################
    
    preplogs <- shiny::reactive({
      shiny::req(!base::is.null(prepactivities()))
      shiny::req(base::nrow(reactval$students) > 1)
      logs <- reactval$interactions |>
        dplyr::left_join(dplyr::select(reactval$students, subject = userid, fullname), by  ="subject") |>
        dplyr::left_join(prepactivities(), by  ="object") |>
        dplyr::mutate(
          week = lubridate::week(date),
          day = lubridate::as_date(date),
          hour = lubridate::hour(date),
          order = base::as.numeric(date)
        ) |>
        dplyr::mutate(
          week = base::as.factor(week),
          day = base::as.factor(day),
        ) |>
        dplyr::mutate(
          week = forcats::fct_reorder(week, order, base::mean),
          day = forcats::fct_reorder(day, order, base::mean)
        ) |>
        dplyr::select(-order)
    })
    
    output$displaylogs <- DT::renderDataTable({
      shiny::req(!base::is.null(preplogs()))
      preplogs()
    }, filter="top", options = base::list(pageLength = 10, scrollX = TRUE), rownames= FALSE)
    
    output$log_selections <- shiny::renderUI({
      shiny::req(!base::is.null(preplogs()))
      
      prepstudentchoices <- preplogs() |>
        dplyr::select(subject, fullname) |>
        dplyr::arrange(fullname) |>
        base::unique()
      studentchoices <- prepstudentchoices$subject
      base::names(studentchoices) <- prepstudentchoices$fullname
      
      shiny::fluidRow(
        shiny::column(2, shinyWidgets::virtualSelectInput(
          ns("logslctstudent"), "Students:",
          choices = studentchoices,
          selected = studentchoices,
          multiple = TRUE, width = "100%"
        )),
        
        shiny::column(2, shinyWidgets::virtualSelectInput(
          ns("logfilttype"), "Types:",
          choices = base::levels(preplogs()$type),
          selected = base::levels(preplogs()$type),
          multiple = TRUE, width = "100%"
        )),
        
        shiny::column(1, shinyWidgets::virtualSelectInput(
          ns("logfiltreq"), "Requirements:",
          choices = base::levels(preplogs()$requirement),
          selected = base::levels(preplogs()$requirement),
          multiple = TRUE, width = "100%"
        )),
        shiny::column(1, shinyWidgets::virtualSelectInput(
          ns("logfiltlev"), "Levels:",
          choices = base::levels(preplogs()$level),
          selected = base::levels(preplogs()$level),
          multiple = TRUE, width = "100%"
        )),
        shiny::column(1, shinyWidgets::virtualSelectInput(
          ns("logfiltts"), "Time-space:",
          choices = base::levels(preplogs()$time_space),
          selected = base::levels(preplogs()$time_space),
          multiple = TRUE, width = "100%"
        )),
        shiny::column(1, shinyWidgets::virtualSelectInput(
          ns("logfiltsoc"), "Social:",
          choices = base::levels(preplogs()$social),
          selected = base::levels(preplogs()$social),
          multiple = TRUE, width = "100%"
        )),
        
        shiny::column(1, shinyWidgets::virtualSelectInput(
          ns("logslctcat"), "Category:",
          choices = c("type","requirement","level","time_space","social","week"),
          selected = "type",
          multiple = FALSE, width = "100%"
        )),
        shiny::column(1, shinyWidgets::virtualSelectInput(
          ns("logslctperiod"), "Period:",
          choices = c("week","day"),
          selected = "week",
          multiple = FALSE, width = "100%"
        )),
        shiny::column(1, shinyWidgets::virtualSelectInput(
          ns("logslctunit"), "Aggregation:",
          choices = c("day","hour","log"),
          selected = "hour",
          multiple = FALSE, width = "100%"
        )),
        shiny::column(1, shinyWidgets::virtualSelectInput(
          ns("logslctstat"), "Statistic:",
          choices = c("units","deciles","deviations"),
          selected = "units",
          multiple = FALSE, width = "100%"
        ))
      )
    })
    
    prefilteredlogs <- shiny::reactive({
      shiny::req(!base::is.null(preplogs()))
      shiny::req(!base::is.null(input$logfilttype))
      shiny::req(!base::is.null(input$logfiltreq))
      shiny::req(!base::is.null(input$logfiltts))
      shiny::req(!base::is.null(input$logfiltsoc))
      shiny::req(!base::is.null(input$logslctcat))
      shiny::req(!base::is.null(input$logslctperiod))
      shiny::req(!base::is.null(input$logslctunit))
      prefilteredlogs <- preplogs() |>
        dplyr::filter(
          type %in% input$logfilttype,
          requirement %in% input$logfiltreq,
          level %in% input$logfiltlev,
          time_space %in% input$logfiltts,
          social %in% input$logfiltsoc
        )
      prefilteredlogs <- prefilteredlogs[,c("subject","activity",input$logslctcat,input$logslctperiod,input$logslctunit)]
      base::names(prefilteredlogs) <- c("subject","activity","category","period","units")
      base::unique(prefilteredlogs)
    })
    
    filteredlogs <- shiny::reactive({
      shiny::req(!base::is.null(prefilteredlogs()))
      shiny::req(!base::is.null(input$logslctstudent))
      filteredlogs <- prefilteredlogs() |>
        dplyr::filter(subject %in% input$logslctstudent) |>
        base::unique()
    })
    
    output$interaction_count <- shiny::renderPlot({
      shiny::req(!base::is.null(filteredlogs()))
      filteredlogs() |>
        dplyr::mutate(units = 1) |>
        dplyr::group_by(category,period) |>
        dplyr::summarise(units = base::sum(units)) |>
        dplyr::ungroup() |>
        ggplot2::ggplot(ggplot2::aes(x = period, y = units, fill = category)) +
        ggplot2::geom_bar(stat = "identity", position = "stack") +
        ggplot2::theme_light() +
        ggplot2::theme(
          legend.position = "top",
          text = ggplot2::element_text(size = 12),
          axis.text.x = ggplot2::element_text(angle = 60, vjust = 0.5, hjust=0.5)
        )
    })
    
    output$weekslider <- shiny::renderUI({
      shiny::req(!base::is.null(filteredlogs()))
      periods <- base::levels(filteredlogs()$period)
      shinyWidgets::sliderTextInput(
        inputId = ns("temprange"),
        label = "Choose a time range:", 
        choices = periods,
        selected = periods[c(1, base::length(periods))],
        width = "100%"
      )
    })
    
    output$lognetwork <- shiny::renderUI({
      shiny::req(!base::is.null(activity_graph()))
      shiny::req(!base::is.null(filteredlogs()))
      shiny::req(!base::is.null(input$temprange))
      shiny::req(!base::is.null(input$logslctstat))
      
      activity_graph <- activity_graph()
      
      shiny::withProgress(message = "Creating a visualization of the activity map", {
        
        shiny::incProgress(1/6)
        
        removebelow <- base::match(input$temprange[1], base::levels(filteredlogs()$period))
        removeabove <- base::match(input$temprange[2], base::levels(filteredlogs()$period))
        
        sizes <- filteredlogs() |>
          dplyr::mutate(period = base::as.integer(period)) |>
          dplyr::filter(period >= removebelow, period <= removeabove) |>
          dplyr::mutate(units = 1) |>
          dplyr::group_by(activity) |>
          dplyr::summarise(units = base::sum(units)) |>
          dplyr::ungroup() |>
          dplyr::mutate(deciles = base::round(dplyr::percent_rank(units)*10,0)) |>
          dplyr::mutate(deviations = base::as.numeric(base::scale(units))) |>
          dplyr::mutate_if(base::is.numeric, function(x){
            0.1 + (x - base::min(x)) / (base::max(x) - base::min(x))
          }) |>
          dplyr::select(activity, width = dplyr::all_of(input$logslctstat)) |>
          dplyr::mutate(height = width, fontsize = 12 * width)
        
        shiny::incProgress(1/6)
        
        activity_graph <- activity_graph()
        if ("switchxy" %in% input$activityaxes){
          xaxis <- activity_graph$nodes_df$y
          yaxis <- activity_graph$nodes_df$x
        } else {
          xaxis <- activity_graph$nodes_df$x
          yaxis <- activity_graph$nodes_df$y
        }
        
        shiny::incProgress(1/6)
        
        if ("invertx" %in% input$activityaxes){
          xaxis <- -xaxis
        }
        if ("inverty" %in% input$activityaxes){
          yaxis <- -yaxis
        }
        shiny::incProgress(1/6)
        
        activity_graph$nodes_df$x <- xaxis
        activity_graph$nodes_df$y <- yaxis
        
        activity_graph$nodes_df <- activity_graph$nodes_df |>
          dplyr::select(-width, -height, -fontsize) |>
          dplyr::left_join(sizes, by = "activity") |>
          tidyr::replace_na(base::list(width = 0.1, height = 0.1, fontsize = 1))
        
        shiny::incProgress(1/6)
        graph <- DiagrammeR::render_graph(
          activity_graph,
          width = "1600px",
          height = "1000px",
          as_svg = TRUE
        )
        shiny::incProgress(1/6)
      })
      
      graph
    })
    
    output$learning_profiles <- shiny::renderPlot({
      shiny::req(!base::is.null(prefilteredlogs()))
      shiny::req(!base::is.null(input$logslctstudent))
      shiny::req(!base::is.null(input$temprange))
      
      removebelow <- base::match(input$temprange[1], base::levels(filteredlogs()$period))
      removeabove <- base::match(input$temprange[2], base::levels(filteredlogs()$period))
      
      profiles_in_window <- prefilteredlogs() |>
        stats::na.omit() |>
        dplyr::mutate(
          period = base::as.integer(period),
          units = base::as.numeric(units)
        ) |>
        dplyr::filter(period >= removebelow, period <= removeabove) |>
        dplyr::group_by(subject, category) |>
        dplyr::summarise(units = base::sum(units), .groups = "drop")
      
      aggregated_profiles <- profiles_in_window |>
        dplyr::group_by(category) |>
        dplyr::summarise(
          avg = base::mean(units),
          sdev = stats::sd(units),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          low = avg - sdev,
          high = avg + sdev
        ) |>
        dplyr::select(category, low, avg, high) |>
        tidyr::pivot_longer(
          cols = c("low","avg","high"),
          names_to = "subject", values_to = "units"
        ) |>
        dplyr::mutate(
          lwd = dplyr::case_when(
            subject == "avg" ~ 3,
            TRUE ~ 2
          ),
          lty = dplyr::case_when(
            subject == "avg" ~ 3,
            TRUE ~ 2
          ),
          col = dplyr::case_when(
            subject == "low" ~ 1,
            subject == "avg" ~ 2,
            TRUE ~ 3
          )
        ) |>
        dplyr::select(subject, category, units, lty, col, lwd)
      
      selected_profiles <- profiles_in_window |>
        dplyr::filter(subject %in% base::unique(base::sample(
          input$logslctstudent, base::min(base::length(input$logslctstudent),6)
        ))) |>
        dplyr::mutate(
          lwd = 10, lty = 1,
          col = 3 + base::as.numeric(base::as.factor(subject))
        ) |>
        dplyr::left_join(dplyr::select(reactval$students, subject = userid, fullname), by = "subject") |>
        dplyr::select(subject = fullname, category, units, lty, col, lwd)
      
      radarbase <- dplyr::bind_rows(aggregated_profiles, selected_profiles) |>
        base::unique() |>
        dplyr::mutate(
          subject = base::as.factor(subject),
          subject = forcats::fct_reorder(subject, col, mean),
          lty = base::factor(lty, levels = c(1:3))
        ) |>
        tidyr::replace_na(base::list(units = 0)) |>
        dplyr::mutate(
          category = base::as.factor(category),
          category = forcats::fct_reorder(category, units, base::mean)
        ) |>
        dplyr::arrange(category)
      
      colors <- c(
        "#999999","#333333","#999999",
        "#770000","#007700","#000077",
        "#770077","#007777","#777700"
      )
      
      coord_radar <- function (theta = "x", start = 0, direction = 1){
        theta <- base::match.arg(theta, c("x", "y"))
        r <- if (theta == "x") 
          "y"
        else "x"
        ggplot2::ggproto(
          "CordRadar", ggplot2::CoordPolar,
          theta = theta, r = r, start = start,
          direction = base::sign(direction),
          is_linear = function(coord) TRUE
        )
      }
      
      radarbase |>
        ggplot2::ggplot(ggplot2::aes(
          x = category, y = units,
          group = subject, color = subject, fill = subject,
          linewidth = lwd, linetype = lty
        )) +
        ggplot2::geom_polygon() +
        coord_radar() +
        ggplot2::scale_linewidth(limits = c(0,30)) +
        ggplot2::theme_minimal() +
        ggplot2::scale_color_manual(values = colors) +
        ggplot2::scale_fill_manual(values = c(
          ggplot2::fill_alpha(colors[1:3], 0.01),
          ggplot2::fill_alpha(colors[4:9], 0.1)
        )) +
        ggplot2::guides(linewidth = "none", linetype = "none") +
        ggplot2::ylim(-base::max(radarbase$units)/10, base::max(radarbase$units)) +
        ggplot2::theme(text = ggplot2::element_text(size = 16))
        
    }, height = 640)
    
    
    
    # EXPORT ###################################################################
    # Save on disk or export
    
    learning_journey <- shiny::reactive({
      
      outcomes <- reactval$outcomes |>
        dplyr::mutate(order = base::as.numeric(order)) |>
        dplyr::arrange(order)
      outcomelevels <- outcomes$outcome
      connections <- reactval$connections |>
        dplyr::mutate(
          origin = base::factor(origin, levels = outcomelevels),
          destination = base::factor(destination, levels = outcomelevels)
        ) |>
        dplyr::arrange(origin, destination) |>
        dplyr::mutate(
          origin = base::as.character(origin),
          destination = base::as.character(destination)
        )
      
      if (base::nrow(connections) > 0){
        connections <- connections |>
          dplyr::mutate(findbidir = purrr::map2_chr(origin, destination, function(x,y){
            base::paste0(base::sort(c(x,y)), collapse = "_")
          })) |>
          dplyr::group_by(findbidir) |>
          dplyr::sample_n(1) |>
          dplyr::ungroup() |>
          dplyr::select(-findbidir)
      }
      
      outlabels <- reactval$outlabels |>
        dplyr::mutate(outcome = base::factor(outcome, levels = outcomelevels)) |>
        dplyr::arrange(language, outcome) |>
        dplyr::mutate(outcome = base::as.character(outcome))
      
      activities <- reactval$activities |>
        dplyr::mutate(order = base::as.numeric(order)) |>
        dplyr::arrange(order)
      activitylevels <- activities$activity
      
      actlabels <- reactval$actlabels |>
        dplyr::mutate(activity = base::factor(activity, levels = activitylevels)) |>
        dplyr::arrange(language, activity) |>
        dplyr::mutate(activity = base::as.character(activity))
      
      base::list(
        outcomes = outcomes,
        connections = connections,
        outlabels = outlabels,
        activities = activities,
        actlabels = actlabels,
        attributes = reactval$attributes,
        languages = reactval$languages,
        students = reactval$students,
        interactions = reactval$interactions
      )
    })
    
    
    
    shiny::observeEvent(input$savepaths, {
      shiny::req(!base::is.na(pathfile()))
      quietly_write <- purrr::safely(writexl::write_xlsx)
      if (sourcetype() == "internal"){
        check <- quietly_write(learning_journey(), path = pathfile())
      } else {
        check <- quietly_write(learning_journey(), path = "www/coursemap.xlsx")
      }
      if (!base::is.null(check$error)){
        shinyalert::shinyalert(
          "Not saved!", "The map could not been saved probably because the file is open in another application.",
          type = "error"
        )
      } else {
        shinyalert::shinyalert(
          "Saved!", "Your learning map and paths have been saved on disk.",
          type = "success"
        )
      }
    })
    
    
    
    shiny::observeEvent(input$openpaths, {
      if (base::length(course_paths()) == 2) {
        folder <- course_paths()$subfolders$paths
        if (base::file.exists(pathfile())){
          if (base::Sys.info()[1] == "Windows"){
            base::shell.exec(pathfile())
          } else {
            base::system2(pathfile())
          }
        } else {
          shinyalert::shinyalert(
            "Sorry!", "The file does not exist.",
            type = "error"
          )
        }
      } else {
        shinyalert::shinyalert(
          "Sorry!", "You need to load a course and a tree first.",
          type = "error"
        )
      }
    })
    
    
    
    shiny::observeEvent(input$openfolder, {
      if (base::length(course_paths()) == 2) {
        folder <- course_paths()$subfolders$paths
        if (base::dir.exists(folder)){
          if (.Platform['OS.type'] == "windows"){
            shell.exec(folder)
          } else {
            system2("open", folder)
          }
        } else {
          shinyalert::shinyalert(
            "Sorry!", "The folder does not appear do exist.",
            type = "error"
          )
        }
      } else {
        shinyalert::shinyalert(
          "Sorry!", "You need to load a course and a tree first.",
          type = "error"
        )
      }
    })
    
    
    
    output$downloadpath <- shiny::downloadHandler(
      filename = function() {
        base::paste0("course_map", ".xlsx")
      },
      content = function(file) {
        writexl::write_xlsx(learning_journey(), path = file)
      }
    )
    
    
  })
}

