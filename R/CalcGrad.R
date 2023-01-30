#' Cálculo do volume de fase móvel e tempo total de um batch cromatográfico
#'
#' @description Cálculo do volume de fase móvel e tempo total de um batch cromatográfico.
#'
#' @param x Datapath character, arquivo em .csv do gradiente.
#' @param runs Numeric, número de corridas no batch.
#' @param over Numeric, limite de segurança para o sistema cromatográfico não secar. Valor padrão é 15%.
#' @return uma lista com o volume total de cada fase móvel em mL (A e B) e tempo total
#'
#' @examples
#' \dontrun{
#' GradCalc(x, runs = 15, over = 15)
#'}
#' @import lubridate
#' @import dplyr
#' @import data.table
#'
#' @export
#' @author Estevan Bruginski \email{estevan.bruginski@ufpr.br}
#' Universidade Federal do Paraná
#' License: MIT + file LICENSE
#'
CalcGrad <- function(x, runs, over = 15) {

  ## importa o gradiente
  gradprof <-
    fread(x,
          header = TRUE,
          check.names = FALSE)

  ## valida a soma das fases A e B é 100
  val.grad <- gradprof %>%
    filter((A + B) == 100) %>%
    nrow() == nrow(gradprof)

  ## valida o tempo inicial como 0
  val.intime <- gradprof %>%
    filter(row_number() == 1) %>%
    filter(Time == 0) %>%
    nrow() > 0

  ## valida o aumento do tempo
  val.time <- gradprof %>%
    mutate(maior = Time > lag(Time)) %>%
    select(maior) %>%
    filter(row_number() != 1) %>%
    all()

  ## Cálcula o o volume de fases móveis e o tempo total do batch cromatográfico
  if (val.grad == TRUE &
      val.intime == TRUE &
      val.time == TRUE) {

    gradient <- gradprof %>%
      select(Time, Flow, A, B) %>%
      mutate(dt = Time - lag(Time))

    steps.A <- gradient %>%
      mutate(vol = Flow * dt * (A + lag(A)) / 200) %>%
      pull(vol)

    steps.B <- gradient %>%
      mutate(vol = Flow * dt * (B + lag(B)) / 200) %>%
      pull(vol)

    total.A <- sum(steps.A, na.rm = TRUE) * runs

    total.B <- sum(steps.B, na.rm = TRUE) * runs

    batch.time <- gradprof %>%
      slice_tail(n = 1) %>%
      mutate(batch.time = Time * 60 * runs) %>%
      mutate(batch.time = lubridate::seconds_to_period(batch.time)) %>%
      select(batch.time)

    result <- list(total.A, total.B, batch.time)

    names(result) <- c("solvent_A", "solvent_B", "total_time")

    result_t <-
      paste(
        "Volume total de fase A:",
        result$solvent_A,
        "ml. Volume total de fase B:",
        result$solvent_B,
        "ml. O tempo total será de:",
        result[["total_time"]][["batch.time"]],
        "."
      )

    return(print(result_t))

  } else{
    message("Verifique o seu gradiente!")
  }
  }
