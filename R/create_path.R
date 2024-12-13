#' @name create_path
#' @title Create a learning path
#' @author Nicolas Mangin
#' @description Function creating a learning path from selected activities and connections between learning outcomes.
#' @param activities Tibble. List of pre-filtered activities with their attributes to be organized in a learning path.
#' @param connections Tibble. List of edges between learning outcomes.
#' @return Tibble. A list of link between activities (origin, destination, and condition)
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr lag
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr n
#' @importFrom dplyr select
#' @importFrom dplyr slice
#' @importFrom dplyr ungroup
#' @importFrom purrr map
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#' @export



create_path <- function(activities, connections){
  
  activity <- NULL
  condition <- NULL
  data <- NULL
  destination <- NULL
  destorder <- NULL
  origin <- NULL
  origout <- NULL
  outcomes <- NULL
  requirement <- NULL
  subgroup <- NULL
  type <- NULL
  destact <- NULL
  origact <- NULL
  
  
  base_for_links <- activities |>
    dplyr::mutate(order = base::as.numeric(order)) |>
    dplyr::select(activity, order, outcomes, subgroup, type, requirement) |>
    dplyr::mutate(outcomes = purrr::map(outcomes, stringr::str_split, pattern = " ", simplify = TRUE)) |>
    dplyr::mutate(outcomes = purrr::map(outcomes, base::as.character)) |>
    tidyr::unnest(outcomes)
  
  necessary_activities <- base_for_links |>
    dplyr::filter(requirement == "NEC")
  
  links_between_necessary <- necessary_activities |>
    dplyr::select(destination = activity, destorder = order, outcomes) |>
    dplyr::group_by(outcomes) |>
    dplyr::arrange(destorder) |>
    dplyr::mutate(origin = dplyr::lag(destination)) |>
    dplyr::left_join(dplyr::select(necessary_activities, origin = activity, origtype = "type"), by = "origin") |>
    stats::na.omit() |>
    dplyr::mutate(condition = dplyr::case_when(
      origtype == "Test" ~ "OK",
      TRUE ~ "Done"
    )) |>
    dplyr::ungroup() |>
    dplyr::select(origin, destination, condition)
  
  links_within_subgroups <- base_for_links |>
    dplyr::select(destination = activity, subgroup, destorder = order, desttype = type, destreq = requirement) |>
    dplyr::filter(subgroup != "NA") |>
    dplyr::group_by(subgroup) |>
    dplyr::arrange(destorder) |>
    dplyr::mutate(origin = dplyr::lag(destination)) |>
    dplyr::left_join(dplyr::select(base_for_links, origin = activity, origreq = requirement), by = "origin") |>
    stats::na.omit() |>
    dplyr::mutate(condition = "Done") |>
    dplyr::ungroup() |>
    dplyr::select(origin, destination, condition)
    
  links_within_outcomes <- base_for_links |>
    dplyr::select(destination = activity, outcomes, destorder = order, desttype = type, destreq = requirement) |>
    dplyr::group_by(outcomes, destorder) |>
    tidyr::nest() |>
    dplyr::ungroup() |>
    dplyr::group_by(outcomes) |>
    dplyr::arrange(destorder) |>
    dplyr::mutate(origorder = dplyr::lag(destorder)) |>
    dplyr::left_join(dplyr::select(base_for_links, origin = activity, origorder = order, origtype = type, origreq = requirement), by = "origorder") |>
    tidyr::unnest(data) |>
    stats::na.omit() |>
    dplyr::mutate(condition = dplyr::case_when(
      origtype == "Test" ~ "OK",
      TRUE ~ "Done"
    )) |>
    dplyr::ungroup() |>
    dplyr::select(origin, destination, condition)
  
  firstact <- base_for_links |>
    dplyr::filter(requirement == "NEC") |>
    dplyr::group_by(outcomes) |>
    dplyr::arrange(order) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
     dplyr::select(destination = outcomes, destact = activity)
  
  lastact <- base_for_links |>
     dplyr::filter(requirement == "NEC") |>
     dplyr::group_by(outcomes) |>
     dplyr::arrange(order) |>
     dplyr::slice(dplyr::n()) |>
     dplyr::ungroup() |>
     dplyr::select(origin = outcomes, origact = activity, origtype = type)
  
  links_across_outcomes <- connections |>
    dplyr::inner_join(lastact, by = "origin") |>
    dplyr::inner_join(firstact, by = "destination") |>
    dplyr::mutate(condition = dplyr::case_when(
      origtype == "Test" ~ "OK",
      TRUE ~ "Done"
    )) |>
    dplyr::select(origin = origact, destination = destact, condition)
  
  paths <- dplyr::bind_rows(
    links_between_necessary,
    links_within_subgroups,
    links_within_outcomes,
    links_across_outcomes
  ) |>
    base::unique()
  
  return(paths)
}
