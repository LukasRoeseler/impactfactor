get_jif <- function(issn) { # years = 2 means last two full years, i.e., 2022 and 2023

  years <- 2

  # get last 1000 works from journal
  cat("Retrieving publications")
  ds <- rcrossref::cr_journals(issn, works = TRUE, limit = 1000)$data

  # if nrow(ds) == 1000
  # WARNING: not all works could be retrieved due to a crossref limit of 1000. The JIF is likely overestimated.

  ## process publications
  cat(" - processing")

  # filter by year
  ds <- ds[!is.na(ds$published.online), ]
  ds$published.online.year <- substr(ds$published.online, 1, 4)
  ds <- ds[ds$published.online.year >= (lubridate::year(Sys.Date()) - years), ]

  # exclude publications from current year
  ds <- ds[ds$published.online.year != lubridate::year(Sys.Date()), ]

  # get citations for chosen articles
  cat(" - retrieving citation counts")
  cit <- rcrossref::cr_citation_count(ds$doi)

  # compute JIF
  cat(" - computing JIF")
  citations_sum <- sum(cit$count, na.rm = TRUE)
  citations_median <- median(cit$count, na.rm = TRUE)
  citable_objects <- nrow(ds)

  j <- citations_sum/citable_objects

  # format and create output data
  output <- data.frame("issn" = issn
                       , "citations_sum" = citations_sum
                       , "citations_median" = citations_median
                       , "works" = citable_objects
                       , "jif" = j)

  cat(" - DONE")

  return(output)

  # WARNING: The computed value includes all citations of the works from the last two years until the last update of the crossref database.


}


# # test function (not run)
# jif_fnp <-  get_jif("2699-4445")
# jif_mp <-   get_jif("2003-2714")
# jif_jcre <- get_jif("2749-988X")
# jif_nhb <-  get_jif("2397-3374")
