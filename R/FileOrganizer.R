#' File Organizer
#' @description Organize files into folders by batch or class.
#'
#' @param x Datapath character, the path of the metadata file (.csv).
#' @param filetype Character, extension of any kind of files (".mzML", ".mzXML", ".CDF", ".raw", ".txt", ".netCDF", etc.).
#' @param by Character, defines if the organization will be by "Batch" or "Class"; "Batch" by default.
#' @param remove Logical, determines whether files should be removed from the root folder; FALSE by default.
#' @return  Directories will be created by batch or class and the files will be placed according to the metadata information.
#'
#' @examples
#' \dontrun{
#' FileOrganizer(x, filetype = c(".mzML", ".mzXML", ".CDF", ".raw", ".txt"), by= c("batch", "class"), remove = FALSE)
#' }
#'
#' @import utils
#' @import progress
#' @export
#' @author Estevan Bruginski \email{estevan.bruginski@ufpr.br}
#' Universidade Federal do Paraná
#' License: MIT + file LICENSE
#'
FileOrganizer <- function(x,
                          filetype = ".mzML",
                          by = "Batch",
                          remove = FALSE) {
  ## import the metadata
  metadata <- read.csv(x, header = TRUE, as.is = TRUE)

  ## select the filenames and insert the extension
  snames <- metadata$Samples

  snames <- paste0(snames, filetype)

  if (by == "Batch") {
    label <- metadata$Batch
  } else{
    label <- metadata$Class
  }


  ## create the folders
  dnames <- levels(as.factor(label))

  lapply(dnames, dir.create)


  ## progress bar
  pb <-
    progress_bar$new(
      format = "Files Transfering [:bar] :percent Time left: :eta",
      total = length(snames),
      clear = T,
      width = 100
    )

  ## sort the files into their respective batch folders
  for (i in 1:length(dnames)) {
    pb$tick()

    tmp <- dnames[i]


    select <- label == tmp


    selfiles <- snames[select]


    file.copy(selfiles, tmp)

  }
  if (remove == TRUE) {
    ## remove files after sorting
    lapply(snames, file.remove)


    message(
      "\nAll files have been moved to their respective folders and delete of the root directory."
    )
  } else{
    message("\nAll files have been moved to their respective folders.")

  }
}
