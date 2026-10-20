
# all published NICE guidance, advice and quality standards is from
# https://www.nice.org.uk/guidance/published?sp=on&ps=9999

library(readr)
library(purrr)
library(tidyr)
library(dplyr)
nicedf <- read_delim("c:\\users\\jmatt\\repos\\visNICE\\data-raw\\NICEpublished.csv")
colnames(nicedf)[1] <- "Title"

nicedf$Title <- iconv(nicedf$Title, from = "ISO-8859-1", to = "UTF-8")

nicedf$reptype <- substr(nicedf$`Reference number`,1,2) # first two letters for type of report

nicedf <- nicedf |>
  filter(reptype=="TA") #restrict to Technology Assessments

nicedf$drug <- map_chr( nicedf$Title,
                        .f=function(x) {
                        x <- gsub("<96>"," ",x)
                        substr(x,1,regexpr(" ", x)-1)})
# assume first word is main drug

nicedf$ATC <- map_chr(nicedf$drug, .f=function(x) ATCdata[match(tolower(x), names(ATCdata))])
# locate ATC code if possible

# TODO when final dataset is ready
# usethis::use_data(DATASET, overwrite = TRUE)
