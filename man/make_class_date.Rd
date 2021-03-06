% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/syllabus_dates.R
\name{make_class_date}
\alias{make_class_date}
\title{Create a function that returns date of the given class number}
\usage{
make_class_date(base = base_date, pattern)
}
\arguments{
\item{base}{The date of the first class}

\item{pattern}{See details}
}
\value{
A function that takes one argument: class_num.
When called, the function returns the date of the class
with the given number.
}
\description{
Create a function that returns date of the given class number
}
\details{
Pattern has a string with the class meeting pattern.
Options currently supported are
* "M", "Tu", "W", "Th", or "F" for weekly classes
* "TuTh", "MW" for classes that meet twice a week
* "MWF" for classes that meet three times a week
}
