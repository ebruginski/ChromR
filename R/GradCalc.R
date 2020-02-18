#' Gradient mobile phase calculation
#'
#' @description Calculate the A and B mobile phases of a LC gradient.
#'
#' @param grad data frame, gradient information previously imported with GradInput function.
#' @param runs numeric, number of chromatographic runs in the batch.
#' @param over numeric, is an overage of mobile phase to keep the input of the pump submerged to avoid air entrance in the system. The default value is 15\%. Set according with your system.
#'
#' @return a list with the total batch time and the amount, in ml, of solvents A and B.
#'
#' @example GradCalc(grad, runs = 15, over = 15)
#'
#' @importFrom utils read.csv
#' @importFrom utils tail
#' @importFrom lubridate seconds_to_period
#'
#' @export
#'
GradCalc <- function(grad, runs, over = 15){
  #empty vector to store the vol of each gradient step
  steps.A <- NULL;
  steps.B <- NULL;
  #compute vol of each gradient step
  for (i in 1:nrow(grad)){
    steps.A[i] <- grad[i,2]*(grad[i+1,1]-grad[i,1])*(grad[i,3]+grad[i+1,3])/200;
    steps.B[i] <- grad[i,2]*(grad[i+1,1]-grad[i,1])*(grad[i,4]+grad[i+1,4])/200;
  };
  #sum the the vol of each phase and multiply by the number of runs to get the total vol A and B
  total.A.tmp <- sum(steps.A, na.rm = TRUE)*runs;
  total.B.tmp <- sum(steps.B, na.rm = TRUE)*runs;
  #mobile phase overage to secure the LC system
  over <- over/100;
  total.A <- round(total.A.tmp+(total.A.tmp*over),0);
  total.B <- round(total.B.tmp+(total.B.tmp*over),0);

  #get the total batch time
  run.time <- tail(grad,1);
  run.time <- as.numeric(run.time[1])*60
  batch.time <- run.time*runs
  batch.time <- seconds_to_period(batch.time)

  #list of results
  result <- list(total.A, total.B, batch.time);
  names(result) <- c("Total of solvent A (ml):", "Total of solvent B (ml):", "Total batch time:");

  return(print(result))
}
