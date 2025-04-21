#' @name prepare_moodle_data
#' @title Prepare Moodle data
#' @author Nicolas Mangin
#' @description Function creating a list of activities, users, and interactions from Moodle reports.
#' @param path_to_folder Character. Path to the fodler containing Moodle's report: activities.html, users.html, and logs.csv
#' @param course Character. Names of the course from which data is extracted.
#' @param language Character. US or FR
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom furrr future_map
#' @importFrom future plan
#' @importFrom rvest html_attr
#' @importFrom rvest html_elements
#' @importFrom rvest html_nodes
#' @importFrom rvest html_table
#' @importFrom rvest html_text2
#' @importFrom rvest read_html
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_split
#' @importFrom stringr str_to_lower
#' @importFrom stringr str_to_title
#' @importFrom tibble rowid_to_column
#' @importFrom tibble tibble
#' @importFrom tidyr separate
#' @importFrom tidyr unnest
#' @export


prepare_moodle_data <- function(
  path_to_folder = "www",
  course = "TIFAPAF",
  language = "FR"
){
  
  URL <- NULL
  action <- NULL
  activity <- NULL
  component <- NULL
  context <- NULL
  data <- NULL
  description <- NULL
  email <- NULL
  firstname <- NULL
  group <- NULL
  label <- NULL
  lastname <- NULL
  lmsid <- NULL
  object <- NULL
  student <- NULL
  studentid <- NULL
  studenturl <- NULL
  subject <- NULL
  userid <- NULL
  
  
  # Get data
  
  activities <- base::paste0(path_to_folder, "/activities.html")
  users <- base::paste0(path_to_folder, "/users.html")
  logs <- base::paste0(path_to_folder, "/logs.csv")
  
  users <- rvest::read_html(users)
  userlinks <- users |>
    rvest::html_nodes('.d-inline-block') |>
    rvest::html_attr("href") |>
    stats::na.omit()
  
  user_list <- users|>
    rvest::html_table()
  user_list <- user_list[[1]][,-1]
  base::names(user_list) <- c("student","studentid","email","role","group","lastseen","status")
  user_list <- user_list |>
    dplyr::filter(student != "", !base::is.na(student)) |>
    dplyr::mutate(studenturl = userlinks)
  
  activitylinks <- rvest::read_html(activities) |>
    rvest::html_nodes('.activity') |>
    rvest::html_elements("a") |>
    rvest::html_attr("href")
  
  activitytext <- rvest::read_html(activities) |>
    rvest::html_nodes('.activity') |>
    rvest::html_elements("a") |>
    rvest::html_text2()
  
  activity_list <- tibble::tibble(
    label = activitytext,
    URL = activitylinks
  ) |>
    dplyr::filter(!(label %in% c("Modifier","Supprimer","Edit","Delete")))
  
  logs <- utils::read.csv(logs)
  base::names(logs) <- c("date","subjectname","targetname","context","component","event","description","source","ip")
  
  logs <- logs |>
    dplyr::filter(!(component %in% c("Modifier","Supprimer","Edit","Delete")))
  
  base::rm(users, activities, activitylinks, activitytext, path_to_folder, userlinks)
  
  # Clean data
  
  user_list <- user_list |>
    dplyr::mutate(userid = stringr::str_remove_all(studenturl, "^.+id=")) |>
    dplyr::mutate(userid = stringr::str_remove_all(userid, "&.+$")) |>
    dplyr::mutate(student = stringr::str_remove_all(email, "@ehl.ch")) |>
    tidyr::separate(student, into = c("firstname", "lastname"), sep = "\\.") |>
    dplyr::mutate(
      firstname = stringr::str_to_title(firstname),
      lastname = stringr::str_to_title(lastname)
    ) |>
    dplyr::select(userid, studentid, lastname, firstname, email, group, studenturl)
  
  activity_list <- activity_list |>
    tibble::rowid_to_column("order") |>
    dplyr::mutate(
      language = language,
      description = NA,
      lmsid = stringr::str_remove_all(URL, "^.+id=")
    ) |>
    tibble::rowid_to_column("activity") |>
    dplyr::select(activity, order, language, label, description, lmsid, URL)
  
  parse_log_description <- function(x){
    
    actions <- c(
      "viewed","created","updated","started","reviewed","submitted","working",
      "moved","imported","deleted","graded","added","downloaded","exported",
      "posted","subscribed","uploaded","removed","assigned","enrolled","restored"
    )
    
    x <- x |>
      stringr::str_replace_all("&#039; ", "; ") |>
      stringr::str_remove_all("&#039;") |>
      stringr::str_to_lower() |>
      stringr::str_split("; ")
    x <- x[[1]]  
    y <- stringr::str_extract_all(x, "[0-9]+", simplify = TRUE)
    z1 <- x[2:base::length(x)]
    z2 <- y[2:base::length(y)]
    tibble::tibble(
      subject = y[1],
      action = actions[stringr::str_detect(x[2],actions)],
      activity = c(z2[stringr::str_detect(z1, "module")], NA)[1],
      target = c(z2[stringr::str_detect(z1, "user")], NA)[1],
      attempt = c(z2[stringr::str_detect(z1, "attempt")], NA)[1],
      grade = c(z2[stringr::str_detect(z1, "grade")], NA)[1],
      item = c(z2[stringr::str_detect(z1, "item")], NA)[1],
      course = c(z2[stringr::str_detect(z1, "course")], NA)[1]
    )
  } 
  
  future::plan("multisession")
  
  logs <- logs |>
    dplyr::mutate(
      context = course,
      data = furrr::future_map(description, parse_log_description)
    ) |>
    tibble::rowid_to_column("log") |>
    tidyr::unnest(data) |>
    dplyr::select(log, date, subject, action, object = activity, context) |>
    dplyr::filter(
      subject %in% user_list$userid,
      object %in% activity_list$lmsid
    )
  
  # Results
  
  results <- base::list(
    actlabels = activity_list,
    students = user_list,
    interactions = logs
  )
  
  return(results)
  
}

