
#' Return the date of a given class number
#'
#' @param class_num The number of the class, starting at 1
#' @param base The starting date for the class
#' @param pattern See the function `make_next_class_date`
#' @param readable if TRUE, return a string; if FALSE, return a date object (defaults to TRUE)
#'
#' @return Either a string or a date object with the date of the `class_num`th class
class_date <- function(class_num, base = base_date, pattern, readable=TRUE) {

  if (! pattern %in% c("M", "Tu", "W", "Th", "F", "MW", "TuTh", "MWF")) {
    stop("Unsupported class meeting pattern.")
  }

  if(pattern %in% c("M", "Tu", "W", "Th", "F")) {
    num_weeks <- ceiling((class_num - 1))

    dow <- switch(pattern,
                  M=1, Tu=2, W=3, Th=4, F=5)
    intervals <- cumsum(dow, num_weeks))

  } else if (pattern %in% c("TuTh", "MW")) {

    num_weeks <- ceiling((class_num-1)/2)

    if(pattern == 'TuTh') {

      if (wday(base_date) == 'Tue') {
        intervals <- cumsum(rep(c(2,5), num_weeks))
      } else {
        # base_date is thursday
        intervals <- cumsum(rep(c(5,2), num_weeks))
      }

    } else {
      # pattern is MW
      if (wday(base_date) == 'Mon') {
        intervals <- cumsum(rep(c(2,5), num_weeks))
      } else {
        # base_date is wednesday
        intervals <- cumsum(rep(c(5,2), num_weeks))
      }
    }
  } else {
    num_weeks <- ceiling((class_num-1)/3)

    # pattern is MWF
    if (wday(base_date) == 'Mon') {
      intervals <- cumsum(rep(c(2,2,3), num_weeks))
    } else if (wday(base_date) == 'Wed') {
      intervals <- cumsum(rep(c(2,3,2), num_weeks))
    } else {
      # base_date is friday
      intervals <- cumsum(rep(c(3,2,2), num_weeks))
    }
  }

  class_dates <- map(intervals, ~ base_date + ddays(.x))

  if (class_num == 1) {
    newdate <- base_date
  } else {
    newdate <- class_dates[[class_num-1]]
  }

  if (readable) {
    # return a readable string
    return(glue('{weekday}, {month} {day}',
                weekday=wday(newdate, label=TRUE, abbr=TRUE),
                month=month(newdate, label=TRUE),
                day=day(newdate)))
  } else {
    # return the date object
    return(newdate)
  }
}

#' Given a date, calculate which week of class it takes place in
#'
#' @param date The date
#' @param base The date of the first class
week_num <- function(date, base = base_date) {
  if (! is.Date(date)) {
    date <- as.Date(date)
  }

  return(epiweek(date) - epiweek(base) + 1)

}

#' Create a function that returns date of subsequent classes each time it's called
#'
#' @param base The date of the first class
#' @param pattern See details
#'
#' @return A function that, when called, returns the date of the next class
#'
#' @details Pattern has a string with the class meeting pattern.
#' Options currently supported are
#' * "M", "Tu", "W", "Th", or "F" for weekly classes
#' * "TuTh", "MW" for classes that meet twice a week
#' * "MWF" for classes that meet three times a week
make_next_class_date <- function(base = base_date, pattern) {
  cur_cd <- 0
  get_cd <- function(x) {
    cur_cd <<- cur_cd + 1
    return(class_date(cur_cd, base, pattern))
  }
  return(get_cd)
}

next_class <- make_next_class_date(base_date)

# i use this for grad classes, which meet once a week
week_date <- function(week_num, base = base_date) {
  newdate <- base + dweeks(week_num - 1)
  #return(format(newdate, "%B %d"))
  return(glue('{weekday}, {month} {day}',
              weekday=wday(newdate, label=TRUE, abbr=TRUE),
              month=month(newdate, label=TRUE),
              day=day(newdate)))
}

#' Create a function that returns date of subsequent classes each time it's called
#'
#' @param base The date of the first class
#'
#' @details
#' see
#' https://stackoverflow.com/questions/1088639/static-variables-in-r
#' for helpful info on static functions in R
make_next_week <- function(base = base_date) {
  cur_week <- 0
  get_week <- function(x) {
    cur_week <<- cur_week + 1
    return(week_date(cur_week, base))
  }
  return(get_week)
}

