#' Format Peak List
#'
#' @description Format and merge the sample list with the generated peak list.
#'
#' @param samplelist Datapath character, sample list in .csv with the columns names as follow: Samples, Class, Batch and, Order.
#' @param peaklist Datapath character, peaktable output from MetaboAnalystR (metaboanalyst_input.csv).
#' @param export Logical, if TRUE export the formated peaklist to a .csv file, TRUE by default.
#' @param expname Logical, plot the gradient profile, TRUE by default.
#' @return Formated samplelist dataframe
#'
#' @examples
#' \dontrun{
#' FormatPeakList("data/samplelist.csv", "data/metaboanalyst_input.csv", export = TRUE, expname = "BC_peaklist.csv)
#'}
#' @importFrom utils read.csv
#' @importFrom utils write.csv
#' @export
#' @author Estevan Bruginski \email{estevan.bruginski@ufpr.br}, Letícia Meier \email{leticiameier@ufpr.br}
#'
#' Universidade Federal do Paraná
#' License: MIT + file LICENSE
#'

FormatPeakList <- function(samplelist, peaklist, export = TRUE, expname = "BC_peaklist.csv") {
  # input
  slist <- read.csv(samplelist)

  plist <- read.csv(peaklist)

  # transpose peaklist
  tplist <- t(plist)

  # remove feature names
  toplist <- tplist[-1,]

  fnames <- tplist[1,]

  fnames <- fnames[-1]

  # ordering
  toplist <- toplist[match(slist$Samples, rownames(toplist)), ];

  clnames <- toplist[,1];
  toplist <- toplist[,-1]

  # bind lists
  BC_peaklist <- cbind(slist$Samples, clnames, slist$Batch, slist$Order, toplist);
  colnames(BC_peaklist) <- c("Samples", "Class", "Batch", "Order", fnames)

  if(export == TRUE){
    write.csv(BC_peaklist, expname, quote = FALSE, row.names = FALSE)
  }

  return(as.data.frame(BC_peaklist))
}
