% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FileOrganizer.R
\name{FileOrganizer}
\alias{FileOrganizer}
\title{File Organizer}
\usage{
FileOrganizer(x, filetype = ".mzML", by = "Batch", remove = FALSE)
}
\arguments{
\item{x}{Datapath character, the path of the metadata file (.csv).}

\item{filetype}{Character, extension of any kind of files (".mzML", ".mzXML", ".CDF", ".raw", ".txt", ".netCDF", etc.).}

\item{by}{Character, defines if the organization will be by "Batch" or "Class"; "Batch" by default.}

\item{remove}{Logical, determines whether files should be removed from the root folder; FALSE by default.}
}
\value{
Directories will be created by batch or class and the files will be placed according to the metadata information.
}
\description{
Organize files into folders by batch or class.
}
\examples{
\dontrun{
FileOrganizer(x, filetype = c(".mzML", ".mzXML", ".CDF", ".raw", ".txt"), by= c("batch", "class"), remove = FALSE)
}

}
\author{
Estevan Bruginski \email{estevan.bruginski@ufpr.br}
Universidade Federal do Paraná
License: MIT + file LICENSE
}
