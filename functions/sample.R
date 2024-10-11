#' Sample (possibly weighted) time relative to a time series 
#' 
#' @param x a vector of Date values (may be unordered)
#' @param size num, the number of samples
#' @param by char, the time step to sample ("day", "month" or "year")
#' @param weighted logical, if TRUE use the distribution of \code{x} to create a sampling weight
#' @param ... any other arguments for \code{sample}
#' @return Date class vector
sample_time = function(x = read_obis(form = "sf") |>
                         dplyr::filter(date >= as.Date("2000-01-01")) |>
                         dplyr::pull(date), 
                       size = 100,
                       by = c("day", "month", "year")[2],
                       weighted = FALSE,
                       ...){
  
  d = switch(tolower(by[1]),
             "month" = format(x, "%Y-%m-01") |> as.Date(),
             "year" = format(x, "%Y-01-01") |> as.Date(),
             x)
  r = range(d)
  b = seq(from = r[1], to = r[2], by = by)
  nb = length(b)
  if (weighted){
    H = hist(d, breaks = b, plot = FALSE)
    s = sample(b[seq(from = 1, to = nb-1, by = 1)], size = size, prob = H$density, ...)
  } else {
    s = sample(b[seq(from = 1, to = nb-1, by = 1)], size = size, ...)
  }
  s
}