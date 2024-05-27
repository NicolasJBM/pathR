#' @name design_path_server
#' @title Design learning paths
#' @author Nicolas Mangin
#' @description Module facilitating the design and visualization or a complete learning experience.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param filtered Reactive. List of pre-selected documents.
#' @param course_data Reactive. Function containing all the course data loaded with the course.
#' @param tree Reactive. Function containing a list of documents as a classification tree compatible with jsTreeR
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @return Save the new or modified page in the folder "2_documents/main_language/".
#' @importFrom DiagrammeR add_edge
#' @importFrom DiagrammeR add_node
#' @importFrom DiagrammeR create_graph
#' @importFrom DiagrammeR render_graph
#' @importFrom DiagrammeR to_igraph
#' @importFrom dplyr add_row
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr count
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_all
#' @importFrom dplyr select
#' @importFrom dplyr ungroup
#' @importFrom editR selection_server
#' @importFrom forcats fct_reorder
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 geom_tile
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 scale_fill_gradient
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_light
#' @importFrom ggplot2 theme_minimal
#' @importFrom igraph layout_with_dh
#' @importFrom igraph layout_with_drl
#' @importFrom igraph layout_with_fr
#' @importFrom igraph layout_with_graphopt
#' @importFrom igraph layout_with_kk
#' @importFrom lubridate week
#' @importFrom purrr map
#' @importFrom purrr map_chr
#' @importFrom readxl read_excel
#' @importFrom rhandsontable hot_cols
#' @importFrom rhandsontable hot_context_menu
#' @importFrom rhandsontable hot_to_r
#' @importFrom rhandsontable renderRHandsontable
#' @importFrom rhandsontable rhandsontable
#' @importFrom shiny HTML
#' @importFrom shiny NS
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny h2
#' @importFrom shiny icon
#' @importFrom shiny moduleServer
#' @importFrom shiny numericInput
#' @importFrom shiny observe
#' @importFrom shiny observeEvent
#' @importFrom shiny reactive
#' @importFrom shiny reactiveValues
#' @importFrom shiny renderPlot
#' @importFrom shiny renderUI
#' @importFrom shiny req
#' @importFrom shiny selectInput
#' @importFrom shiny sliderInput
#' @importFrom shiny tagList
#' @importFrom shinyWidgets airDatepickerInput
#' @importFrom shinyWidgets radioGroupButtons
#' @importFrom shinyalert shinyalert
#' @importFrom stringr str_detect
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_split
#' @importFrom tibble tibble
#' @importFrom tidyr nest
#' @importFrom tidyr replace_na
#' @importFrom tidyr unnest
#' @importFrom writexl write_xlsx
#' @export


design_path_server <- function(id, filtered, tree, course_data, course_paths){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    
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
    
    # DATA #####################################################################
    # Load paths
    
    reactval <- shiny::reactiveValues()
    
    pathfile <- shiny::reactive({
      shiny::req(base::length(course_paths()) == 2)
      shiny::req(base::length(tree()) == 4)
      base::paste0(
        course_paths()$subfolders$paths, "/",
        stringr::str_replace(tree()$course$tree[[1]], "RData$", "xlsx")
      )
    })
    
    output$slctlanguage <- shiny::renderUI({
      shiny::req(base::length(course_paths()) == 2)
      shiny::req(base::length(tree()) == 4)
      languages <- course_data()$languages |>
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
    
    shiny::observe({
      if (!base::is.null(pathfile())){
        reactval$outcomes <- readxl::read_excel(pathfile(), sheet = "outcomes", col_types = "text")
        reactval$connections <- readxl::read_excel(pathfile(), sheet = "connections", col_types = "text")
        reactval$outlabels <- readxl::read_excel(pathfile(), sheet = "outlabels", col_types = "text")
        reactval$activities <- readxl::read_excel(pathfile(), sheet = "activities", col_types = "text")
        reactval$paths <- readxl::read_excel(pathfile(), sheet = "paths", col_types = "text")
        reactval$actlabels <- readxl::read_excel(pathfile(), sheet = "actlabels", col_types = "text")
        reactval$actattributes <- readxl::read_excel(pathfile(), sheet = "actattributes", col_types = "text")
        reactval$attributes <- readxl::read_excel(pathfile(), sheet = "attributes", col_types = "text")
        reactval$students <- readxl::read_excel(pathfile(), sheet = "students", col_types = "text")
        reactval$interactions <- readxl::read_excel(pathfile(), sheet = "interactions", col_types = c("text","date","text","text","text","text"))
      } else {
        reactval$outcomes <- tibble::tibble(
          outcome = base::as.character(NA),
          order = base::as.character(NA),
          type = base::as.character(NA),
          color = base::as.character(NA)
        )
        reactval$connections <- tibble::tibble(
          origin = base::as.character(NA),
          destination = base::as.character(NA)
        )
        reactval$outlabels <- tibble::tibble(
          outcome = base::as.character(NA),
          language = base::as.character(NA),
          label = base::as.character(NA),
          description = base::as.character(NA),
          lmsid = base::as.character(NA),
          URL = base::as.character(NA)
        )
        reactval$activities <- tibble::tibble(
          activity = base::as.character(NA),
          order = base::as.character(NA),
          type = base::as.character(NA)
        )
        reactval$paths <- tibble::tibble(
          origin = base::as.character(NA),
          destination = base::as.character(NA),
          condition = base::as.character(NA)
        )
        reactval$actlabels <- tibble::tibble(
          activity = base::as.character(NA),
          language = base::as.character(NA),
          label = base::as.character(NA),
          description = base::as.character(NA),
          lmsid = base::as.character(NA),
          URL = base::as.character(NA)
        )
        reactval$actattributes <- tibble::tibble(
          activity = base::as.character(NA),
          outcomes = base::as.character(NA),
          resource = base::as.character(NA),
          requirement = base::as.character(NA),
          weigth = base::as.character(NA),
          time_space = base::as.character(NA),
          social = base::as.character(NA),
          duration = base::as.character(NA),
          start = base::as.character(NA),
          end = base::as.character(NA)
        )
        reactval$attributes <- tibble::tibble(
          attribute = base::as.character(NA),
          value = base::as.character(NA),
          language = base::as.character(NA),
          label = base::as.character(NA),
          icon = base::as.character(NA)
        )
        reactval$students <- tibble::tibble(
          userid = base::as.character(NA),
          studentid = base::as.character(NA),
          lastname = base::as.character(NA),
          firstname = base::as.character(NA),
          fullname = base::as.character(NA),
          email = base::as.character(NA)
        )
        reactval$interactions <- tibble::tibble(
          log = base::as.character(NA),
          date = base::as.Date(NA),
          week = base::as.character(NA),
          subject = base::as.character(NA),
          action = base::as.character(NA),
          object = base::as.character(NA),
          context = base::as.character(NA)
        )
      }
    })
    
    # OUTCOMES #################################################################
    # Add, edit or remove learning outcomes and links between them
    
    # Combine outcome information for a selected language
    outcomelist <- shiny::reactive({
      shiny::req(base::length(course_paths()) == 2)
      shiny::req(base::length(tree()) == 4)
      shiny::req(!base::is.null(input$slctlang))
      labels <- reactval$outlabels |>
        dplyr::filter(language == input$slctlang) |>
        dplyr::select(-language)
      reactval$outcomes |>
        dplyr::left_join(labels, by = "outcome") |>
        dplyr::select(outcome, label, description, lmsid, URL, order, type, color)
    })
    
    # Edit the outcome (feature that are either specific or common to all languages)
    output$editoutcomes <- rhandsontable::renderRHandsontable({
      shiny::req(outcomelist())
      outcomelist() |>
        dplyr::add_row() |>
        dplyr::mutate(
          type = base::factor(type, levels = c("PRE", "SPE", "GEN")),
          order = base::as.numeric(order)
        ) |>
        dplyr::arrange(order) |>
        rhandsontable::rhandsontable(
          height = 500, width = "100%", rowHeaders = NULL, stretchH = "all"
        ) |>
        rhandsontable::hot_cols(
          colWidths = c("6%","25%","35%","6%","10%","6%","6%","6%")
        ) |>
        rhandsontable::hot_context_menu(
          allowRowEdit = TRUE, allowColEdit = FALSE
        )
    })
    
    shiny::observeEvent(input$updateoutcomes, {
      updated_outcomes <- rhandsontable::hot_to_r(input$editoutcomes) |>
        dplyr::mutate_all(base::as.character) |>
        dplyr::filter(!base::is.na(outcome), outcome != "")
      
      checkids <- updated_outcomes |>
        dplyr::group_by(outcome) |>
        dplyr::count() |>
        dplyr::filter(n > 1)
      
      if (base::nrow(checkids) == 0){
        
        updated_outlabels <- updated_outcomes |>
          dplyr::mutate(language = input$slctlang) |>
          dplyr::select(outcome, language, label, description, lmsid, URL) |>
          base::unique()
        
        updated_outcomes <- updated_outcomes |>
          dplyr::select(-language, -label, -description, -lmsid, -URL) |>
          base::unique()
        
        all_languages <- base::unique(course_data()$languages$langiso)
        outlabels <- dplyr::select(updated_outcomes, outcome) |>
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
        
        reactval$outlabels <- outlabels
        reactval$outcomes <- updated_outcomes
        reactval$connections <- connections
        
      } else {
        shinyalert::shinyalert(
          "Duplicated IDs",
          base::paste0("The ID(s) ", base::paste(checkids$outcome, collapse = ", "), " is/are duplicated. They should be unique."),
          type = "error"
        )
      }
    })
    
    # Create, modify or remove links between outcomes.
    output$editconnections <- rhandsontable::renderRHandsontable({
      reactval$connections |>
        stats::na.omit() |>
        dplyr::add_row() |>
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
        stats::na.omit()
      reactval$connections <- updated_connections
    })
    
    # Visualize the outcome map.
    outcome_labels <- shiny::reactive({
      shiny::req(outcomelist())
      reactval$outlabels |>
        dplyr::filter(language == input$slctlang) |>
        dplyr::select(-language)
    })
    
    output$outcomemap <- shiny::renderUI({
      
      shiny::req(base::nrow(reactval$outlabels) > 1)
      shiny::req(base::nrow(reactval$outcomes) > 1)
      shiny::req(base::nrow(reactval$connections) > 1)
      shiny::req(!base::is.null(input$slctlang))
      shiny::req(!base::is.null(outcome_labels()))
      
      outcomelabs <- outcome_labels() |>
        dplyr::mutate(label = stringr::str_replace_all(label, "_", "\n"))
      
      outcome_graph <- DiagrammeR::create_graph()
      
      nodes <- reactval$outcomes |>
        tidyr::replace_na(base::list(description = " ")) |>
        dplyr::left_join(outcomelabs, by = "outcome") |>
        dplyr::mutate(
          shape = dplyr::case_when(
            type == "PRE" ~ "rectangle",
            type == "GEN" ~ "star",
            TRUE ~ "ellipse"
          ),
          tooltip = base::paste(description, outcome, sep = "\n\n")
        ) |>
        dplyr::select(label, shape, color, tooltip, URL)
      
      for (i in base::seq_len(base::nrow(nodes))) {
        outcome_graph <- DiagrammeR::add_node(
          outcome_graph,
          label = nodes[[i,'label']]
        )
      }
      edges <- reactval$connections |>
        dplyr::left_join(dplyr::select(outcomelabs, origin = outcome, origlab = label), by = "origin") |>
        dplyr::left_join(dplyr::select(outcomelabs, destination = outcome, destlab = label), by = "destination") |>
        dplyr::select(origin = origlab, destination = destlab)
      
      for (i in base::seq_len(base::nrow(edges))) {
        outcome_graph <- DiagrammeR::add_edge(
          outcome_graph,
          from = edges$origin[[i]],
          to = edges$destination[[i]]
        )
      }
      
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
          URL = nodes$URL
        )
      
      base::set.seed(input$defseedoutcomes)
      
      layout_basis <- DiagrammeR::to_igraph(outcome_graph)
      
      layout <- base::switch(
        input$slctlayoutoutcomes,
        dh = igraph::layout_with_dh(layout_basis),
        drl = igraph::layout_with_drl(layout_basis),
        fr = igraph::layout_with_fr(layout_basis),
        graphopt = igraph::layout_with_graphopt(layout_basis),
        kk = igraph::layout_with_kk(layout_basis)
      )
      
      outcome_graph$nodes_df$x <- layout[,1] / input$defscalingoutcomes
      outcome_graph$nodes_df$y <- layout[,2] / input$defscalingoutcomes
      
      DiagrammeR::render_graph(
        outcome_graph,
        width = "1600px",
        height = "800px",
        as_svg = TRUE
      )
    })
    
    
    
    # ACTIVITIES ###############################################################
    # Add, edit or remove activities
    
    # Combine activity information for a selected language
    activitylist <- shiny::reactive({
      shiny::req(base::length(course_paths()) == 2)
      shiny::req(base::length(tree()) == 4)
      shiny::req(!base::is.null(input$slctlang))
      labels <- reactval$actlabels |>
        dplyr::filter(language == input$slctlang) |>
        dplyr::select(-language)
      reactval$activities |>
        dplyr::left_join(labels, by = "activity") |>
        dplyr::select(activity, label, description, lmsid, URL, order, type)
    })
    
    # Create activities and edit attributes specific to a language..
    output$createactivities <- rhandsontable::renderRHandsontable({
      shiny::req(!base::is.null(activitylist()))
      activitylist() |>
        dplyr::add_row() |>
        dplyr::mutate(
          type = base::factor(type, levels = c("Note", "Slide", "Video","Textbook","Tutorial","Game","Case","Test")),
          order = base::as.numeric(order)
        ) |>
        dplyr::arrange(order) |>
        rhandsontable::rhandsontable(
          height = 500, width = "100%", rowHeaders = NULL, stretchH = "all"
        ) |>
        rhandsontable::hot_cols(
          colWidths = c("10%","20%","30%","10%","10%","10%","10%")
        ) |>
        rhandsontable::hot_context_menu(
          allowRowEdit = TRUE, allowColEdit = FALSE
        )
    })
    
    shiny::observeEvent(input$updateactlist, {
      updated_activities <- rhandsontable::hot_to_r(input$createactivities) |>
        dplyr::mutate_all(base::as.character) |>
        dplyr::filter(!base::is.na(activity), activity != "")
      
      checkids <- updated_activities |>
        dplyr::group_by(activity) |>
        dplyr::count() |>
        dplyr::filter(n > 1)
      
      if (base::nrow(checkids) == 0){
        
        updated_actlabels <- updated_activities |>
          dplyr::mutate(language = input$slctlang) |>
          dplyr::select(activity, language, label, description, lmsid, URL) |>
          base::unique()
        
        updated_activities <- updated_activities |>
          dplyr::select(activity, order, type) |>
          base::unique()
        
        all_languages <- base::unique(course_data()$languages$langiso)
        actlabels <- dplyr::select(updated_activities, activity) |>
          dplyr::mutate(language = base::list(all_languages)) |>
          tidyr::unnest(language) |>
          dplyr::left_join(reactval$actlabels, by = c("activity", "language")) |>
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
        
        actattributes <- updated_activities |>
          dplyr::select(activity) |>
          base::unique() |>
          dplyr::left_join(reactval$actattributes, by = "activity") |>
          tidyr::replace_na(base::list(
            requirement = "OPT",
            weigth = "0",
            time_space = "AOL",
            social = "IND",
            duration = "0"
          ))
        
        paths <- reactval$paths |>
          dplyr::filter(
            origin %in% updated_activities$activity,
            destination %in% updated_activities$activity
          )
        
        reactval$actlabels <- actlabels
        reactval$activities <- updated_activities
        reactval$actattributes <- actattributes
        reactval$paths <- paths
        
      } else {
        shinyalert::shinyalert(
          "Duplicated IDs",
          base::paste0("The ID(s) ", base::paste(checkids$activity, collapse = ", "), " is/are duplicated. They should be unique."),
          type = "error"
        )
      }
    })
    
    # Edit the attributes common to all languages.
    chooseactivity <- shiny::reactive({
      shiny::req(!base::is.null(activitylist()))
      actchoices <- base::unique(activitylist()$activity)
      base::names(actchoices) <- activitylist()$label
      actchoices
    })
    
    selected_activity <- editR::selection_server("slctact", chooseactivity)
    
    output$editactivities <- shiny::renderUI({
      shiny::req(!base::is.null(activitylist()))
      shiny::req(!base::is.null(outcomelist()))
      shiny::req(!base::is.null(selected_activity()))
      shiny::req(!base::is.null(input$slctlang))
      
      activity <- activitylist() |>
        dplyr::filter(activity == selected_activity()) |>
        dplyr::mutate(label = stringr::str_replace_all(label, "_", " "))
      
      attributes <- reactval$actattributes |>
        dplyr::filter(activity == selected_activity())
      
      acttype <- activity$type[[1]]
      
      slctoutcomes <- outcomelist() |>
        dplyr::filter(type != "GEN")
      outcomes <- slctoutcomes$outcome
      base::names(outcomes) <- slctoutcomes$label
      
      preslctoutcomes <- attributes$outcomes[1] |>
        stringr::str_split(" ", simplify = TRUE) |>
        base::as.character()
      
      if (acttype == "Test"){
        resources <- course_paths()$subfolders$tests |>
          base::list.dirs(recursive = FALSE, full.names = FALSE) |>
          base::setdiff(c("archives","default"))
        tree <- stringr::str_remove_all(tree()$course$tree, ".RData$")
        resources <- resources[stringr::str_detect(resources, tree)]
      } else if (acttype == "Textbook") {
        resources <- base::unlist(tree()$textbook$link)
        base::names(resources) <- base::unlist(tree()$textbook$title)
      } else {
        tmp <- filtered() |>
          dplyr::filter(type == acttype) |>
          dplyr::select(code, title)
        resources <- base::unlist(tmp$code)
        base::names(resources) <- base::unlist(tmp$title)
        if (attributes$resource[1] %in% resources) resources <- resources else
          resources <- c(attributes$resource[1], resources)
      }
      
      attrchoices <- reactval$attributes |>
        dplyr::filter(language == input$slctlang) |>
        dplyr::mutate(label = base::paste0("<i class='fa fa-",icon,"'> | ",value,"</i>")) |>
          dplyr::select(attribute, label, value)
      
      tmp <- attrchoices |>
        dplyr::filter(attribute == "requirement")
      reqchoices <- base::unlist(tmp$value)
      base::names(reqchoices) <- base::unlist(tmp$label)
      
      tmp <- attrchoices |>
        dplyr::filter(attribute == "time_space")
      timspachoices <- base::unlist(tmp$value)
      base::names(timspachoices) <- base::unlist(tmp$label)
      
      tmp <- attrchoices |>
        dplyr::filter(attribute == "social")
      socchoices <- base::unlist(tmp$value)
      base::names(socchoices) <- base::unlist(tmp$label)
      
      base::list(
        shiny::h2(activity$label[[1]]),
        shiny::HTML(acttype),
        shiny::tags$hr(),
        shiny::selectInput(
          ns("defoutcomes"),
          "Outcomes:",
          choices = outcomes,
          selected = preslctoutcomes,
          multiple = TRUE
        ),
        shiny::selectInput(
          ns("defresource"),
          "Resource:",
          choices = resources,
          selected = attributes$resource[1],
          multiple = FALSE
        ),
        shiny::fluidRow(
          shiny::column(
            4,
            shinyWidgets::radioGroupButtons(
              inputId = ns("defreq"),
              label = "Requirement:", 
              choices = reqchoices,
              selected = attributes$requirement[1],
              justified = TRUE,
              status = "danger",
              size = "normal",
              checkIcon = base::list(yes = shiny::icon("check")),
              direction = "vertical"
            )
          ),
          shiny::column(
            4,
            shinyWidgets::radioGroupButtons(
              inputId = ns("deftimespace"),
              label = "Time and Space:", 
              choices = timspachoices,
              selected = attributes$time_space[1],
              justified = TRUE,
              status = "info",
              size = "normal",
              checkIcon = base::list(yes = shiny::icon("check")),
              direction = "vertical"
            )
          ),
          shiny::column(
            4,
            shinyWidgets::radioGroupButtons(
              inputId = ns("defsocial"),
              label = "Social:", 
              choices = socchoices,
              selected = attributes$social[1],
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
              value = base::as.numeric(attributes$weigth[1]),
              width = "100%"
            )
          ),
          shiny::column(
            3,
            shiny::numericInput(
              ns("defduration"), "Duration:",
              min = 5, max = 480, step = 5,
              value = base::as.numeric(attributes$duration[1]),
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
              value = base::as.Date(attributes$start[1]),
              width = "100%"
            )
          ),
          shiny::column(
            6,
            shinyWidgets::airDatepickerInput(
              inputId = ns("defend"),
              label = "End:", 
              value = base::as.Date(attributes$end[1]),
              width = "100%"
            )
          )
        )
      )
    })
    
    shiny::observeEvent(input$updateactattr, {
      shiny::req(!base::is.null(selected_activity()))
      shiny::req(!base::is.null(input$defoutcomes))
      shiny::req(!base::is.null(input$defresource))
      shiny::req(!base::is.null(input$defreq))
      shiny::req(!base::is.null(input$deftimespace))
      shiny::req(!base::is.null(input$defsocial))
      shiny::req(!base::is.null(input$defweight))
      
      update_actattributes <- tibble::tibble(
        activity = selected_activity(),
        outcomes = base::paste(input$defoutcomes, collapse = " "),
        resource = input$defresource,
        requirement = input$defreq,
        weigth = input$defweight,
        time_space = input$deftimespace,
        social = input$defsocial,
        duration = input$defduration,
        start = input$defstart,
        end = input$defend
      ) |>
        dplyr::mutate_all(base::as.character)
      
      actattributes <- reactval$actattributes |>
        dplyr::filter(activity != selected_activity()) |>
        dplyr::bind_rows(update_actattributes)
      
      reactval$actattributes <- actattributes
    })
    
    # Create, modify or remove links between activities.
    output$editpaths <- rhandsontable::renderRHandsontable({
      reactval$paths |>
        stats::na.omit() |>
        dplyr::add_row() |>
        dplyr::mutate(
          origin = base::factor(origin, reactval$activities$activity),
          destination = base::factor(destination, reactval$activities$activity),
          condition = base::factor(condition, levels = c("None","Completion","Failure","Success"))
        ) |>
        dplyr::arrange(origin, destination) |>
        rhandsontable::rhandsontable(
          height = 500, width = "100%", rowHeaders = NULL, stretchH = "all"
        ) |>
        rhandsontable::hot_cols(
          colWidths = c("35%","35%","30%")
        ) |>
        rhandsontable::hot_context_menu(
          allowRowEdit = TRUE, allowColEdit = FALSE
        )
    })
    
    shiny::observeEvent(input$updatepaths, {
      updated_paths <- rhandsontable::hot_to_r(input$editpaths) |>
        dplyr::mutate_all(base::as.character) |>
        stats::na.omit()
      reactval$paths <- updated_paths
    })
    
    # Visualize the outcome map.
    
    activity_labels <- shiny::reactive({
      outcomes <- dplyr::select(reactval$outcomes, outcome, color)
      req <- reactval$attributes |>
        dplyr::filter(attribute == "requirement", language == input$slctlang) |>
        dplyr::select(requirement = value, reqlab = label)
      ts <- reactval$attributes |>
        dplyr::filter(attribute == "time_space", language == input$slctlang) |>
        dplyr::select(time_space = value, tslab = label)
      soc <- reactval$attributes |>
        dplyr::filter(attribute == "social", language == input$slctlang) |>
        dplyr::select(social = value, soclab = label)
      
      reactval$actlabels |>
        dplyr::filter(language == input$slctlang) |>
        dplyr::left_join(reactval$activities, by = "activity") |>
        dplyr::left_join(reactval$actattributes, by = "activity") |>
        dplyr::mutate(
          label = stringr::str_replace_all(label, "_", "\n"),
          outcome = purrr::map_chr(outcomes, function(x){
            y <- stringr::str_split(x, " ", simplify = TRUE) |>
              base::as.character()
            y[1]
          })
        ) |>
        dplyr::left_join(outcomes, by = "outcome") |>
        tidyr::replace_na(base::list(color = "#FFFFFF")) |>
        dplyr::left_join(req, by = "requirement") |>
        dplyr::left_join(ts, by = "time_space") |>
        dplyr::left_join(soc, by = "social") |>
        dplyr::select(-language, -outcome) |>
        tidyr::replace_na(base::list(end = "-"))
    })
    
    output$activitymap <- shiny::renderUI({
      
      shiny::req(base::nrow(reactval$outcomes) > 1)
      shiny::req(base::nrow(reactval$actlabels) > 1)
      shiny::req(base::nrow(reactval$activities) > 1)
      shiny::req(base::nrow(reactval$paths) > 1)
      shiny::req(base::nrow(reactval$actattributes) > 1)
      shiny::req(base::nrow(reactval$attributes) > 1)
      shiny::req(!base::is.null(input$slctlang))
      shiny::req(!base::is.null(activity_labels()))
      
      activity_graph <- DiagrammeR::create_graph()
      
      nodes <- activity_labels() |>
        tidyr::replace_na(base::list(description = " ")) |>
        dplyr::mutate(weigth = base::as.numeric(weigth)) |>
        dplyr::mutate(
          shape = dplyr::case_when(
            type == "Note" ~ "circle",
            type == "Slide" ~ "square",
            type == "Video" ~ "rectangle",
            type == "Textbook" ~ "ellipse",
            type == "Tutorial" ~ "hexagon",
            type == "Game" ~ "septagon",
            type == "Case" ~ "octagon",
            type == "Test" ~ "star",
            TRUE ~ "point"
          ),
          alpha = dplyr::case_when(
            requirement == "OPT" ~ "33",
            TRUE ~ "77"
          ),
          peripheries = dplyr::case_when(
            time_space == "AOL" ~ 1,
            time_space == "SOL" ~ 2,
            TRUE ~ 3
          ),
          penwidth = 0.5 + weigth * 10,
          tooltip = base::paste0(
            description, "\n\n",
            reqlab, " | ", base::round(weigth * 100,0), "%\n",
            soclab, " | ", tslab, "\n",
            duration, " minutes | ", end, "\n\n",
            activity, " (", type, ")"
          )
        ) |>
        dplyr::mutate(
          bordercolor = base::paste(color, "CC", sep = ""),
          fillcolor = base::paste(color, alpha, sep = ""),
        ) |>
        dplyr::select(
          activity, requirement, label, shape, peripheries, penwidth, bordercolor, fillcolor, tooltip, URL
        )
      
      for (i in base::seq_len(base::nrow(nodes))) {
        activity_graph <- DiagrammeR::add_node(
          activity_graph,
          label = nodes[[i,'label']]
        )
      }
      
      edges <- reactval$paths |>
        dplyr::left_join(dplyr::select(nodes, origin = activity, laborig = label, reqorig = requirement), by = "origin") |>
        dplyr::left_join(dplyr::select(nodes, destination = activity, labdest = label, reqdest = requirement), by = "destination") |>
        dplyr::mutate(
          color = dplyr::case_when(
            condition == "Completion" ~ "#0000FF",
            condition == "Failure" ~ "#990000",
            condition == "Success" ~ "#006600",
            TRUE ~ "#333333"
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
        dplyr::select(origin = laborig, destination = labdest, color, style, penwidth)
      
      for (i in base::seq_len(base::nrow(edges))) {
        activity_graph <- DiagrammeR::add_edge(
          activity_graph,
          from = edges$origin[[i]],
          to = edges$destination[[i]]
        )
      }
      
      activity_graph$nodes_df <- activity_graph$nodes_df |>
        dplyr::mutate(
          shape = nodes$shape,
          width = 0.8,
          height = 0.7,
          fontsize = 10,
          style = "filled",
          color = base::paste0(nodes$bordercolor),
          fillcolor = base::paste0(nodes$fillcolor),
          fontcolor = "black",
          penwidth = nodes$penwidth,
          tooltip = nodes$tooltip,
          URL = nodes$URL,
          peripheries = nodes$peripheries
        )
      
      base::set.seed(input$defseedactivities)
      
      layout_basis <- DiagrammeR::to_igraph(activity_graph)
      
      layout <- base::switch(
        input$slctlayoutactivities,
        dh = igraph::layout_with_dh(layout_basis),
        drl = igraph::layout_with_drl(layout_basis),
        fr = igraph::layout_with_fr(layout_basis),
        graphopt = igraph::layout_with_graphopt(layout_basis),
        kk = igraph::layout_with_kk(layout_basis)
      )
      
      activity_graph$nodes_df$x <- layout[,1] / input$defscalingactivities
      activity_graph$nodes_df$y <- layout[,2] / input$defscalingactivities
      
      DiagrammeR::render_graph(
        activity_graph,
        width = "1600px",
        height = "800px",
        as_svg = TRUE
      )
    })
    
    
    
    # PREPARE ANALYSES #########################################################
    
    prepare_students <- shiny::reactive({
      reactval$students |>
        dplyr::select(subject = userid, fullname)
    })
    
    prepare_activities <- shiny::reactive({
      shiny::req(!base::is.null(activity_labels()))
      activity_labels() |>
        dplyr::mutate(object = stringr::str_split(lmsid, pattern = " ")) |>
        tidyr::unnest(object)
    })
    
    
    
    # ANALYSES OF DESIGN #######################################################
    
    output$outcomes_heatmap <- shiny::renderPlot({
      shiny::req(!base::is.null(outcome_labels()))
      shiny::req(!base::is.null(prepare_activities()))
      
      outcomes <- outcome_labels() |>
        dplyr::select(outcome, outcomelab = label) |>
        dplyr::mutate(outcomelab = stringr::str_replace_all(outcomelab, "_", " "))
      activities <- prepare_activities() |>
        dplyr::mutate(outcome = stringr::str_split(outcomes, pattern = " ")) |>
        tidyr::unnest(outcome) |>
        dplyr::left_join(outcomes, by = "outcome") |>
        dplyr::select(activity, outcome, outcomelab, type) |>
        base::unique()
      
      activities |>
        dplyr::group_by(outcome, outcomelab, type) |>
        dplyr::count() |>
        dplyr::mutate() |>
        ggplot2::ggplot(ggplot2::aes(x = type, y = outcomelab, fill = n)) + 
        ggplot2::geom_tile() +
        ggplot2::scale_fill_gradient(low="yellow", high="forestgreen") +
        ggplot2::theme_minimal() +
        ggplot2::theme(text = ggplot2::element_text(size = 15))
    }, height = 700)
    
    
    
    # ANALYSES OF INTERACTIONS #################################################
    
    output$interaction_count <- shiny::renderPlot({
      shiny::req(base::nrow(reactval$interactions) > 3)
      shiny::req(!base::is.null(prepare_students()))
      shiny::req(!base::is.null(prepare_activities()))
      
      logs <- reactval$interactions |>
        dplyr::left_join(prepare_students(), by = "subject") |>
        dplyr::left_join(prepare_activities(), by = "object")
        
        #dplyr::filter(subject == "13991") |>
        #dplyr::filter(type != "quiz") |>
      
      logs |>
        dplyr::mutate(
          week = base::as.factor(lubridate::week(date)),
          day = base::as.numeric(date)
        ) |>
        dplyr::mutate(week = forcats::fct_reorder(week, day, mean)) |>
        dplyr::select(week, log, category = type) |>
        base::unique() |>
        stats::na.omit() |>
        ggplot2::ggplot(ggplot2::aes(x = week, fill = category)) +
        ggplot2::geom_bar(stat = "count", position = "stack") +
        ggplot2::theme_light() +
        ggplot2::theme(legend.position = "top")
    })
    
    
    
    
    # EXPORT ###################################################################
    # Save on disk or export
    
    shiny::observeEvent(input$savepaths, {
      writexl::write_xlsx(
        base::list(
          outcomes = reactval$outcomes,
          connections = reactval$connections,
          outlabels = reactval$outlabels,
          activities = reactval$activities,
          paths = reactval$paths,
          actlabels = reactval$actlabels,
          actattributes = reactval$actattributes,
          attributes = reactval$attributes,
          students = reactval$students,
          interactions = reactval$interactions
        ),
        path = pathfile()
      )
      
      shinyalert::shinyalert(
        "Saved!", "Your learning map and paths have been saved on disk.",
        type = "success"
      )
    })
    
    shiny::observeEvent(input$openpaths, {
      if (base::Sys.info()[1] == "Windows"){
        base::shell.exec(pathfile())
      } else {
        base::system2(pathfile())
      }
    })
    
    shiny::observeEvent(input$exportpaths, {
      
    })
    
    
  })
}

