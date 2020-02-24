#' Organize the samples in folders (by batch)
#'
#' Put the spectra of the sample files into their respective batch folder. The .csv file must be the "name" and "batch" column.
#' The values in the column name need to have the same name of the sample files including the pattern (e.g.: .mzML, .CDF, .mzXML). Before perform this function, use the "setwd()" to the folder where the files are.
#'
#' @param x path to a .csv file with the experiment metadata.
#' @param remove logical value (FALSE by default), if TRUE the files will be deleted after the sorting.
#'
#' @return  Directories will be created (by batch) and the files will be placed according to the metadata information.
#'
#' @example BatchOrganize(metadata, remove = TRUE)
#'
#' @importFrom utils read.csv
#'
#' @export
#'
BatchOrganize <- function(x, remove = FALSE){
  metadata <- read.csv(x, header = TRUE, as.is = TRUE);
  snames <- metadata$name;
  blabel <- metadata$batch;
  dnames <- levels(as.factor(blabel));
  lapply(dnames, dir.create); # create the folders

  for (i in 1:length(dnames)){ # for loop
    tmp <- dnames[i]; # get the dir name
    select <- blabel == tmp; #test, if the dir name is true or false
    selfiles <- snames[select]; #
    file.copy(selfiles, tmp);
  }
  if (remove == TRUE){
    lapply(snames,file.remove);
    return("All samples were sorted in their batch folders and deleted from origin!"); # remove files after copying
  }else{
    return("All samples were sorted in their batch folders!");
  }
}
