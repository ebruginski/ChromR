#' Plot Gradiente profile
#'
#' Plot the LC gradient profile.
#'
#' @param x data frame, the first column is the id of the samples and the second column is the groups of each sample.
#' @param gnumber numeric, number of groups in the sample, need to be between 2 and 6.
#' @param gnames character, names of the groups, need to be the same of the input data frame.
#'
#' @return A randomized sample list.
#'
#' @examples
#' \dontrun{
#' SampleRand (x, gnumber = 3, gnames = c("g1", "g2", "g3"))
#' }
#' @importFrom stats na.omit
#'
#' @export
#' @author Estevan Bruginski \email{estevan.bruginski@ufpr.br}
#' Universidade Federal do Paraná
#' License: MIT + file LICENSE
#'
SampleRand <- function(x, gnumber, gnames){
  if (gnumber == 2){
    g1 <- x$id[which(x$groups == gnames[1])];
    g2 <- x$id[which(x$groups == gnames[2])];

    if (length(g1) != length(g2)){
      maxval <- as.numeric(max(c(length(g1),length(g2))))
      g1t <- c(g1,rep(NA, maxval-length(g1)))
      g2t <- c(g2,rep(NA, maxval-length(g2)))
      rdata <- data.frame(g1t, g2t)

    } else {
      g1 <- sample(g1);
      g2 <- sample(g2);

      rdata <- data.frame(g1, g2);
    };
  }
  else if (gnumber == 3){

    g1 <- x$id[which(x$groups == gnames[1])];
    g2 <- x$id[which(x$groups == gnames[2])];
    g3 <- x$id[which(x$groups == gnames[3])];

    if (length(g1) != length(g2) || length(g1) != length(g3) || length(g2) != length(g3)){

      maxval <- as.numeric(max(c(length(g1),length(g2),length(g3))))

      g1t <- c(g1,rep(NA, maxval-length(g1)));
      g2t <- c(g2,rep(NA, maxval-length(g2)));
      g3t <- c(g3,rep(NA, maxval-length(g3)));

      g1t <- sample(g1t);
      g2t <- sample(g2t);
      g3t <- sample(g3t);

      rdata <- data.frame(g1t, g2t, g3t);

    } else {

      g1 <- sample(g1);
      g2 <- sample(g2);
      g3 <- sample(g3);

      rdata <- data.frame(g1, g2, g3);
    };
  }
  else if (gnumber == 4){

    g1 <- x$id[which(x$groups == gnames[1])];
    g2 <- x$id[which(x$groups == gnames[2])];
    g3 <- x$id[which(x$groups == gnames[3])];
    g4 <- x$id[which(x$groups == gnames[4])];

    if (length(g1) != length(g2) || length(g1) != length(g3) || length(g2) != length(g3) ||
        length(g1) != length(g4) || length(g2) != length(g4) || length(g3) != length(g4)){

      maxval <- as.numeric(max(c(length(g1),length(g2),length(g3),length(g4))));

      g1t <- c(g1,rep(NA, maxval-length(g1)));
      g2t <- c(g2,rep(NA, maxval-length(g2)));
      g3t <- c(g3,rep(NA, maxval-length(g3)));
      g4t <- c(g4,rep(NA, maxval-length(g4)));

      g1t <- sample(g1t);
      g2t <- sample(g2t);
      g3t <- sample(g3t);
      g4t <- sample(g4t);

      rdata <- data.frame(g1t, g2t, g3t, g4t);

    } else {

      g1 <- sample(g1);
      g2 <- sample(g2);
      g3 <- sample(g3);
      g4 <- sample(g4);

      rdata <- data.frame(g1, g2, g3, g4);
    };
  }
  else if(gnumber == 5){

    g1 <- x$id[which(x$groups == gnames[1])];
    g2 <- x$id[which(x$groups == gnames[2])];
    g3 <- x$id[which(x$groups == gnames[3])];
    g4 <- x$id[which(x$groups == gnames[4])];
    g5 <- x$id[which(x$groups == gnames[5])];


    if (length(g1) != length(g2) || length(g1) != length(g3) || length(g2) != length(g3) ||
        length(g1) != length(g4) || length(g2) != length(g4) || length(g3) != length(g4) ||
        length(g1) != length(g5) || length(g2) != length(g5) || length(g3) != length(g5) ||
        length(g4) != length(g5)){

      maxval <- as.numeric(max(c(length(g1),length(g2),length(g3),length(g4), length(g5))));

      g1t <- c(g1,rep(NA, maxval-length(g1)));
      g2t <- c(g2,rep(NA, maxval-length(g2)));
      g3t <- c(g3,rep(NA, maxval-length(g3)));
      g4t <- c(g4,rep(NA, maxval-length(g4)));
      g5t <- c(g5,rep(NA, maxval-length(g5)));

      g1t <- sample(g1t);
      g2t <- sample(g2t);
      g3t <- sample(g3t);
      g4t <- sample(g4t);
      g5t <- sample(g5t);

      rdata <- data.frame(g1t, g2t, g3t, g4t, g5t);

    } else {

      g1 <- sample(g1);
      g2 <- sample(g2);
      g3 <- sample(g3);
      g4 <- sample(g4);
      g5 <- sample(g5);

      rdata <- data.frame(g1, g2, g3, g4, g5);
    };
  }
  else if(gnumber == 6){

    g1 <- x$id[which(x$groups == gnames[1])];
    g2 <- x$id[which(x$groups == gnames[2])];
    g3 <- x$id[which(x$groups == gnames[3])];
    g4 <- x$id[which(x$groups == gnames[4])];
    g5 <- x$id[which(x$groups == gnames[5])];
    g6 <- x$id[which(x$groups == gnames[6])];

    if (length(g1) != length(g2) || length(g1) != length(g3) || length(g2) != length(g3) ||
        length(g1) != length(g4) || length(g2) != length(g4) || length(g3) != length(g4) ||
        length(g1) != length(g5) || length(g2) != length(g5) || length(g3) != length(g5) ||
        length(g4) != length(g5) || length(g1) != length(g6) || length(g2) != length(g6) ||
        length(g3) != length(g6) || length(g5) != length(g6)){

      maxval <- as.numeric(max(c(length(g1),length(g2),length(g3),length(g4), length(g5), length(g6))));

      g1t <- c(g1,rep(NA, maxval-length(g1)));
      g2t <- c(g2,rep(NA, maxval-length(g2)));
      g3t <- c(g3,rep(NA, maxval-length(g3)));
      g4t <- c(g4,rep(NA, maxval-length(g4)));
      g5t <- c(g5,rep(NA, maxval-length(g5)));
      g6t <- c(g6,rep(NA, maxval-length(g6)));

      g1t <- sample(g1t);
      g2t <- sample(g2t);
      g3t <- sample(g3t);
      g4t <- sample(g4t);
      g5t <- sample(g5t);
      g6t <- sample(g6t);

      rdata <- data.frame(g1t, g2t, g3t, g4t, g5t, g6t);

    } else {

      g1 <- sample(g1);
      g2 <- sample(g2);
      g3 <- sample(g3);
      g4 <- sample(g4);
      g5 <- sample(g5);
      g6 <- sample(g6);

      rdata <- data.frame(g1, g2, g3, g4, g5, g6);
    };
  }
  else {
    print("This function only works with 2, 3, 4, 5, 6 groups!")
  };

  # randomize between the groups
  blocks.ord <- c(apply(rdata, 1, sample));

  # put in order to injeciont
  randata <- data.frame(blocks.ord, x$groups[match(blocks.ord, x$id)])

  randata <- na.omit(randata)

  randata <- cbind(seq(1:nrow(randata)), randata)

  colnames(randata) <- c("order","id", "groups")

  return(randata)
}
