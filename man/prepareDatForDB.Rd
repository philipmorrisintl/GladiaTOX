% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepareDatForDB.R
\name{prepareDatForDB}
\alias{prepareDatForDB}
\title{Assign default processing methods}
\usage{
prepareDatForDB(asid, dat)
}
\arguments{
\item{asid}{Integer, the asid value(s) to assign the default methods to}

\item{dat}{Data.table containing metadata and data to load in DB}
}
\value{
Data table with data and metadata to store in database
}
\description{
This function is a wrapper to ease the creation of the 
dataframe containing data and metadata to be loaded in the database
}
\details{
This function formats a dat table to be loaded in DB
}
\examples{

\dontrun{
## Load sample data
load(system.file("extdata", "data_for_vignette.rda", package="GladiaTOX"))

# Build assay table
assay <- buildAssayTab(plate, chnmap)

## Set study parameters
std.nm <- "SampleStudy" # study name
phs.nm <- "PhaseII" # study phase

## Delete previously loaded study data
asid = gtoxLoadAsid(fld=c("asnm", "asph"), val=list(std.nm, phs.nm))$asid
if(length(asid)>0){ deleteStudy(asid=asid) }

## Load annotation in gtoxDB
loadAnnot(plate, assay, NULL)

## Get the created study ID
asid = gtoxLoadAsid(fld = c("asnm", "asph"), val = list(std.nm, phs.nm))$asid

## Prepare and load data
dat <- prepareDatForDB(asid, dat)
}

}
