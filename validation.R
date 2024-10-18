
# Comparison of reported and computed JIFs --------------------------------

# load packages and functions
source("packages.R")
source("get_jif.R")
require(ggplot2)
require(plotly)

## Get baseline data
# https://www.apa.org/pubs/journals/resources/impact-factors
ifs <- read.csv2("jif2023_apa.csv")
ifs <- ifs[ifs$issn != "", ]

apa_if <- data.frame("issn" = NULL
                        , "citations_sum" = NULL
                        , "citations_median" = NULL
                        , "works" = NULL
                        , "jif" = NULL)

# get impact factors
for (i in 1:nrow(ifs)) {
  print(paste(i, "of", nrow(ifs)))
  apa_ifi <- get_jif(ifs$eissn[i])
  apa_if <- rbind(apa_if, apa_ifi)
}
apa_if

# combine data
ds <- cbind(ifs[1:nrow(apa_if), ], apa_if)
names(ds)[1] <- "journal"
names(ds)[5] <- "issn2"

# check
ds$eissn == ds$issn2

### analyze data
ggplot(ds, aes(x = if_2023, y = jif, label = journal)) + geom_point() + theme_bw() +
  xlab("Impact Factor reported by APA") + ylab("Impact Factor computed via Crossref")
