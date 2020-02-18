#' Organize the samples in folders (by batch)
#'
#' Put the spectra of the sample files into their respective batch folder. The .csv file must be the "name" and "batch" column.
#' The values in the column name need to have the same name of the sample files including the pattern (e.g.: .mzML, .CDF, .mzXML). Before perform this function, use the "setwd()" to the folder where the files are.
#'
#' @param metadata path to a .csv file with the experiment metadata.
#' @param remove logical value (TRUE by default), if TRUE the files will be deleted after the sorting.
#'
#' @return  Directories will be created (by batch) and the files will be placed according to the metadata information.
#'
#' @example BatchOrganize(metadata, remove = TRUE)
#'
#' @importFrom utils read.csv
#'
#' @export
#'
BatchOrganize <- function(metadata, remove){
  meta.data <- read.csv(metadata, header = TRUE, as.is = TRUE);
  samp.names <- meta.data$name;
  batch.label <- meta.data$batch;
  dir.names <- levels(as.factor(batch.label));
  lapply(dir.names, dir.create); # create the folders

  for (i in 1:length(dir.names)){ # for loop
    tmp <- dir.names[i]
    select <- batch.label == tmp
    sel.files <- samp.names[select];
    file.copy(sel.files, tmp);
  }
  if (remove == TRUE){
    lapply(samp.names,file.remove);
    return("All the samples were sorted and deleted from origin!"); # remove files after copying
  }else{
    return("All the samples were sorted in their batch folders!");
  }
}
