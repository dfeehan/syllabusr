
#' Return the date of a given class number
#'
#' @param class_num The number of the class, starting at 1
#' @param base The starting date for the class
#' @param readable if TRUE, return a string; if FALSE, return a date object (defaults to TRUE)
#'
#' @return Either a string or a date object with the date of the `class_num`th class
class_date <- function(class_num, base = base_date, readable=TRUE) {

  num_weeks <- ceiling((class_num-1)/2)

  if (wday(base_date) == 'Tue') {
    intervals <- cumsum(rep(c(2,5), num_weeks))
  } else {
    # base_date is thursday
    intervals <- cumsum(rep(c(5,2), num_weeks))
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
make_next_class_date <- function(base = base_date) {
  cur_cd <- 0
  get_cd <- function(x) {
    cur_cd <<- cur_cd + 1
    return(class_date(cur_cd, base))
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

# see
# https://stackoverflow.com/questions/1088639/static-variables-in-r
make_next_week <- function(base = base_date) {
  cur_week <- 0
  get_week <- function(x) {
    cur_week <<- cur_week + 1
    return(week_date(cur_week, base))
  }
  return(get_week)
}

