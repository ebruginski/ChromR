#' Import gradient
#'
#' Import the gradient information from a .csv file.
#'
#' @param path filepath to a .csv file.
#'
#' @return dataframe with the gradient information.
#'
#' @example GradInput("path/to/your/grad/file.csv)
#'
#' @importFrom utils read.csv
#'
#' @export
#'
GradInput <- function(path){

  data <- read.csv(path, header = TRUE, as.is = TRUE, check.names = FALSE);
  test.comp.t <- (data$A+data$B) <= 100;
  test.time <- data$Time

  if (data$Time[1] >= 1){

    print("The initial time need to be zero!");

  }else if(all(diff(test.time)>0) == FALSE){

    print("The time of gradient need to increase!");

  }else if (sum(test.comp.t) == length(test.comp.t)){

    time.grad <- as.numeric(data$Time);
    flow.grad <- as.numeric(data$Flow);
    comp.A <- as.numeric(data$A);
    comp.B <- as.numeric(data$B);

    return(data.frame(time.grad, flow.grad, comp.A, comp.B));

  }else{
    print("The compositions A and B can't be more than 100%");
  }
}


