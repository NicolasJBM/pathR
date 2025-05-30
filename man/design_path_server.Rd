% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/design_path_server.R
\name{design_path_server}
\alias{design_path_server}
\title{Design learning paths}
\usage{
design_path_server(id, intake = NULL, course_data = NULL, course_paths = NULL)
}
\arguments{
\item{id}{Character. ID of the module to connect the user interface to the appropriate server side.}

\item{intake}{Reactive. Function containing all information about the selected intake.}

\item{course_data}{List. description}

\item{course_paths}{Reactive. Function containing a list of paths to the different folders and databases on local disk.}
}
\value{
Save the new or modified page in the folder "2_documents/main_language/".
}
\description{
Module facilitating the design and visualization or a complete learning experience.
}
\author{
Nicolas Mangin
}
