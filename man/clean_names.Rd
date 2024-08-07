% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean.R
\name{clean_names}
\alias{clean_names}
\title{Clean the most generic errors in species names}
\usage{
clean_names(DF, SP_COL)
}
\arguments{
\item{DF}{The dataframe to run the function on}

\item{SP_COL}{The column with the species names}
}
\value{
Returns the dataframe with the cleans species name
in the original column
}
\description{
This function fixes the most common errors found in species names,
mainly being:
removing spaces at the begining or ends of the words,
removing any numbers or dots,
removing any instances of spp or sp,
removing any punctuation,
capitalising the first word.
}
\examples{
## example code
# clean_names(species_names, scientific_name)

}
