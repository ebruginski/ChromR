#' Gradient mobile phase calculation
#'
#' @description Calculate the volume of A and B mobile phases on a LC gradient.
#'
#' @param x Datapath character, gradient profile in .csv format
#' @param runs Numeric, number of chromatographic runs in the batch.
#' @param over Numeric, overage of mobile phase to keep the system secure. The default value is 15\%. Need to be set according with your system.
#' @param plot Logical, plot the gradient profile, TRUE by default.
#' @return a list with the total batch time and the amount, in ml, of solvents A and B.
#'
#' @examples
#' \dontrun{
#' GradCalc(x, runs = 15, over = 15, plot = TRUE)
#'}
#' @importFrom utils read.csv
#' @importFrom utils tail
#' @importFrom lubridate seconds_to_period
#'
#' @export
#' @author Estevan Bruginski \email{estevan.bruginski@ufpr.br}
#' Universidade Federal do Paraná
#' License: MIT + file LICENSE
#'
CalcGrad <- function(x, runs, over = 15, plot = TRUE) {
  ## Import the gradient profile

  gradprof <- read.csv(x, header = TRUE, check.names = FALSE)

  test.comp.t <- (gradprof$A + gradprof$B) == 100

  test.time <- gradprof$Time
  ## Check the initial time = 0
  if (gradprof$Time[1] >= 1) {
    message("The initial time need to be zero!")

    ## Check the increasing time
  } else if (all(diff(test.time) > 0) == FALSE) {
    message("The time of gradient need to increase!")

    ## Build the checked gradient profile
  } else if (sum(test.comp.t) == length(test.comp.t)) {

    time.grad <- as.numeric(gradprof$Time)

    flow.grad <- as.numeric(gradprof$Flow)

    comp.A <- as.numeric(gradprof$A)

    comp.B <- as.numeric(gradprof$B)

    gradprof_check <- data.frame(time.grad, flow.grad, comp.A, comp.B);

    ## Empty vectors to store the vol of each gradient step
    steps.A <- NULL
    steps.B <- NULL

    ## Compute the volume for each gradient step
    for (i in 1:nrow(gradprof_check)) {
      steps.A[i] <-
        gradprof_check[i, 2] * (gradprof_check[i + 1, 1] - gradprof_check[i, 1]) * (gradprof_check[i, 3] + gradprof_check[i + 1, 3]) / 200
      steps.B[i] <-
        gradprof_check[i, 2] * (gradprof_check[i + 1, 1] - gradprof_check[i, 1]) * (gradprof_check[i, 4] + gradprof_check[i + 1, 4]) / 200
    }

    ## Sum the the volume of each phase and multiply by the number of runs to get the total vol A and B
    total.A.tmp <- sum(steps.A, na.rm = TRUE) * runs
    total.B.tmp <- sum(steps.B, na.rm = TRUE) * runs

    #mobile phase overage to secure the LC system
    over <- over / 100

    total.A <- round(total.A.tmp + (total.A.tmp * over), 1)
    total.B <- round(total.B.tmp + (total.B.tmp * over), 1)

    ## Calculate the total batch time
    run.time <- tail(gradprof_check, 1)

    run.time <- as.numeric(run.time[1]) * 60

    batch.time <- run.time * runs

    batch.time <- lubridate::seconds_to_period(batch.time)

    ## List of results
    result <- list(total.A, total.B, batch.time)

    names(result) <- c("solvent_A", "solvent_B", "total_time")

    result_t <- paste("Total volume of phase A:", result$solvent_A,"ml. Total volume of phase B:", result$solvent_B,"ml. The total time will be", result$total_time,"." )

    if (plot == TRUE){

      pgrad <- PlotGrad(gradprof_check);
      plot(pgrad)
      return(print(result_t))

    } else {

      return(print(result_t))
    }


  } else{
    message("The compositions A and B can't be more than 100%.")

  }
}

