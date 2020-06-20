
#' Return the date of a given class number
#'
#' @param class_num The number of the class, starting at 1
#' @param base The starting date for the class
#' @param pattern See the function `make_next_class_date`
#' @param readable if TRUE, return a string; if FALSE, return a date object (defaults to TRUE)
#'
#' @return Either a string or a date object with the date of the `class_num`th class
#' @export
class_date <- function(class_num, base = base_date, pattern, readable=TRUE) {

  if (! pattern %in% c("M", "Tu", "W", "Th", "F", "MW", "TuTh", "MWF")) {
    stop("Unsupported class meeting pattern.")
  }

  if(pattern %in% c("M", "Tu", "W", "Th", "F")) {
    num_weeks <- ceiling((class_num - 1))

    intervals <- cumsum(rep(7, num_weeks))

  } else if (pattern %in% c("TuTh", "MW")) {

    num_weeks <- ceiling((class_num-1)/2)

    if(pattern == 'TuTh') {

      if (lubridate::wday(base_date) == 3) {
        # base_date is tuesday
        intervals <- cumsum(rep(c(2,5), num_weeks))
      } else {
        # base_date is thursday
        intervals <- cumsum(rep(c(5,2), num_weeks))
      }

    } else {
      # pattern is MW
      if (lubridate::wday(base_date) == 2) {
        # base_date is monday
        intervals <- cumsum(rep(c(2,5), num_weeks))
      } else {
        # base_date is wednesday
        intervals <- cumsum(rep(c(5,2), num_weeks))
      }
    }
  } else {
    num_weeks <- ceiling((class_num-1)/3)

    # pattern is MWF
    if (lubridate::wday(base_date) == 2) {
      # base_date is monday
      intervals <- cumsum(rep(c(2,2,3), num_weeks))
    } else if (lubridate::wday(base_date) == 4) {
      # base_date is wednesday
      intervals <- cumsum(rep(c(2,3,2), num_weeks))
    } else {
      # base_date is friday
      intervals <- cumsum(rep(c(3,2,2), num_weeks))
    }
  }

  class_dates <- purrr::map(intervals, ~ base_date + lubridate::ddays(.x))

  if (class_num == 1) {
    newdate <- base_date
  } else {
    newdate <- class_dates[[class_num-1]]
  }

  if (readable) {
    # return a readable string
    return(glue::glue('{weekday}, {month} {day}',
                weekday=lubridate::wday(newdate, label=TRUE, abbr=TRUE),
                month=lubridate::month(newdate, label=TRUE),
                day=lubridate::day(newdate)))
  } else {
    # return the date object
    return(newdate)
  }
}

#' Given a date, calculate which week of class it takes place in
#'
#' @param date The date
#' @param base The date of the first class
#'
#' @return The week in which the given class takes place. (The week with the first class is week 1)
#' @export
week_num <- function(date, base = base_date) {
  if (! is.Date(date)) {
    date <- as.Date(date)
  }

  return(lubridate::epiweek(date) - lubridate::epiweek(base) + 1)

}

#' Create a function that returns date of subsequent classes each time it's called
#'
#' @param base The date of the first class
#' @param pattern See `make_class_date`
#'
#' @return A function that, when called, returns the date of the next class
#' @export
make_next_class_date <- function(base = base_date, pattern) {
  cur_cd <- 0
  get_cd <- function(x) {
    cur_cd <<- cur_cd + 1
    return(class_date(cur_cd, base, pattern))
  }
  return(get_cd)
}

#' Create a function that returns date of the given class number
#'
#' @param base The date of the first class
#' @param pattern See details
#'
#' @return A function that takes one argument: class_num.
#' When called, the function returns the date of the class
#' with the given number.
#'
#' @details Pattern has a string with the class meeting pattern.
#' Options currently supported are
#' * "M", "Tu", "W", "Th", or "F" for weekly classes
#' * "TuTh", "MW" for classes that meet twice a week
#' * "MWF" for classes that meet three times a week
#' @export
make_class_date <- function(base = base_date, pattern) {
  return(purrr::partial(class_date, base=base, pattern=pattern))
}

