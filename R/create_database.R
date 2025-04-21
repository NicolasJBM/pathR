#' @name create_database
#' @title Create a learning database
#' @author Nicolas Mangin
#' @description Function creating an empty learning database.
#' @return List of tibbles.
#' @importFrom tibble tibble
#' @importFrom tibble tribble
#' @export




create_database <- function(){
  
  outcomes <- tibble::tibble(
    outcome = base::as.character(NA),
    order = base::as.character(NA),
    type = base::as.character(NA),
    color = base::as.character(NA)
  )
  
  connections <- tibble::tibble(
    origin = base::as.character(NA),
    destination = base::as.character(NA)
  )
  
  outlabels <- tibble::tibble(
    outcome = base::as.character(NA),
    language = base::as.character(NA),
    label = base::as.character(NA),
    description = base::as.character(NA),
    lmsid = base::as.character(NA),
    URL = base::as.character(NA)
  )
  
  activities <- tibble::tibble(
    activity = base::as.character(NA),
    order = base::as.character(NA),
    type = base::as.character(NA),
    outcomes = base::as.character(NA),
    subgroup = base::as.character(NA),
    requirement = base::as.character(NA),
    weigth = base::as.character(NA),
    level = base::as.character(NA),
    time_space = base::as.character(NA),
    social = base::as.character(NA),
    duration = base::as.character(NA),
    start = base::as.character(NA),
    end = base::as.character(NA)
  )
  
  actlabels <- tibble::tibble(
    activity = base::as.character(NA),
    language = base::as.character(NA),
    label = base::as.character(NA),
    description = base::as.character(NA),
    lmsid = base::as.character(NA),
    URL = base::as.character(NA)
  )
  
  attributes <- tibble::tribble(
    ~"attribute", ~"value", ~"language", ~"label", ~"icon",
    "level", "NOV", "US", "Novice", "child-reaching",
    "level", "APP", "US", "Apprentice", "user",
    "level", "PRO", "US", "Professional", "user-tie",
    "level", "MAS", "US", "Master", "user-ninja",
    "level", "EXP", "US", "Expert", "user-doctor",
    "social", "IND", "US", "Individual", "user",
    "social", "TM", "US", "Team", "user-plus",
    "social", "GP", "US", "Group", "people-group",
    "time_space", "AOL", "US", "Asynchronous", "house-laptop",
    "time_space", "SOL", "US", "Synchronous online", "house-signal",
    "time_space", "SOS", "US", "Synchronous on site", "school",
    "requirement", "OPT", "US", "Optional", "circle-question",
    "requirement", "REC", "US", "Recommended", "circle-xmark",
    "requirement", "NEC", "US", "Necessary", "circle-exclamation",
    "level", "NOV", "FR", "Novice", "child-reaching",
    "level", "APP", "FR", "Apprenti", "user",
    "level", "PRO", "FR", "Professionnel", "user-tie",
    "level", "MAS", "FR", "Maitre", "user-ninja",
    "level", "EXP", "FR", "Expert", "user-doctor",
    "social", "IND", "FR", "Individuel", "user",
    "social", "TM", "FR", "Equipe", "user-plus",
    "social", "GP", "FR", "Groupe", "people-group",
    "time_space", "AOL", "FR", "Asynchrone", "house-laptop",
    "time_space", "SOL", "FR", "Synchrone en ligne", "house-signal",
    "time_space", "SOS", "FR", "Synchrone sur site", "school",
    "requirement", "OPT", "FR", "Facultatif", "circle-question",
    "requirement", "REC", "FR", "Recommande", "circle-xmark",
    "requirement", "NEC", "FR", "Necessaire", "circle-exclamation"
  )
  
  languages <- tibble::tribble(
    ~langiso, ~language, ~flag,
    "US","English","https://raw.githubusercontent.com/lipis/flag-icons/d6785f2434e54e775d55a304733d17b048eddfb5/flags/1x1/gb.svg",
    "FR","French","https://raw.githubusercontent.com/lipis/flag-icons/d6785f2434e54e775d55a304733d17b048eddfb5/flags/1x1/fr.svg"
  )
  
  students <- tibble::tibble(
    userid = base::as.character(NA),
    studentid = base::as.character(NA),
    lastname = base::as.character(NA),
    firstname = base::as.character(NA),
    fullname = base::as.character(NA),
    email = base::as.character(NA)
  )
  
  interactions <- tibble::tibble(
    log = base::as.character(NA),
    date = base::as.Date(NA),
    subject = base::as.character(NA),
    action = base::as.character(NA),
    object = base::as.character(NA),
    context = base::as.character(NA)
  )
  
  report <- tibble::tibble(
    report = base::as.character(NA),
    date = base::as.Date(NA),
    subject = base::as.character(NA),
    object = base::as.character(NA),
    status = base::as.character(NA)
  )
  
  base::list(
    outcomes = outcomes,
    connections = connections,
    outlabels = outlabels,
    activities = activities,
    actlabels = actlabels,
    attributes = attributes,
    languages = languages,
    students = students,
    interactions = interactions,
    report = report
  )
}
