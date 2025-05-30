% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepare_moodle_data.R
\name{prepare_moodle_data}
\alias{prepare_moodle_data}
\title{Prepare Moodle data}
\usage{
prepare_moodle_data(
  path_to_folder = "www",
  course = "TIFAPAF",
  language = "FR"
)
}
\arguments{
\item{path_to_folder}{Character. Path to the fodler containing Moodle's report: activities.html, users.html, and logs.csv}

\item{course}{Character. Names of the course from which data is extracted.}

\item{language}{Character. US or FR}
}
\description{
Function creating a list of activities, users, and interactions from Moodle reports.
}
\author{
Nicolas Mangin
}
