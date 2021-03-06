% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_summarize_years}
\alias{fars_summarize_years}
\title{Summarize FARS data by year}
\usage{
fars_summarize_years(years)
}
\arguments{
\item{years}{A character string or vector of character strings representing
the year(s) of the FARS data. An integer or vector of integers may also
be used for input; which represent the year(s) of the FARS data.}
}
\value{
This function returns a tbl_df object containing FARS summary data.
The first column is named 'MONTH' and will have an element for the integer value
of each month. The second and subsequent columns will be named by the year
of FARS data they represent, and each element in these columns will contain the
value of fatalities for that month.
}
\description{
This function will summarize the FARS data for one or more years.
}
\examples{
\dontrun{
# Using character strings as input ...
farsYearsListSummaryA <- fars_summarize_years("2014")
farsYearsListSummaryB <- fars_summarize_years(c("2013","2014","2015"))

# Using integers as input ...
farsYearsListSummaryA <- fars_summarize_years(2014)
farsYearsListSummaryB <- fars_summarize_years(c(2013,2014,2015))

# Using an invalid year will result in an error ...
farsYearsListSummaryC <- fars_summarize_years("1892")
# Error in grouped_df_impl(data, unname(vars), drop) :
# Column `year` is unknown
}

}
