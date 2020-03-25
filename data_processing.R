library(readxl)
library(httr)
# Process csv
# reader <- reactiveFileReader(1000, NULL, 'data.csv', read.csv)

# 2017

get_xl_url <- function(url) {
  GET(url1, write_disk(tf <- tempfile(fileext = ".xlsx")))
  df <- read_excel(tf, 1L)
  
  return(df)
}

# df17 <- get_xl_url("https://data.gov.au/data/dataset/a1f8626c-fefb-4c4d-86ea-deaa04fb1f6e/resource/8d020b50-700f-4bc4-8c78-79f83d99be7a/download/datadotgov_ais17.xlsx")



# Get rankings
get_ranking <- function(inputdf) {
  rank1 <- inputdf %>%
  group_by(`main activity`) %>%
  select(`donations and bequests`,
         `revenue from government`,
         `revenue from goods and services`,
         `revenue from investments`,
         `other income`,
         `total revenue`,
         `total assets`,
         `total full time equivalent staff`) %>%
  summarise_all(sum)

  rank2 <- select_if(rank1, is.numeric)
  rank2 <- -rank2
  rank2$`main activity` <- rank1$`main activity`
  
  rank3 <- rank2 %>%
    lapply(rank, ties.method = 'min') %>%
    as.data.frame()
  
  rank3$`main activity` <- rank1$`main activity`
  
  colnames(rank3) <- paste(colnames(rank3), 'rank', sep = '.')
  
  return(rank3)
}

rank3 <- get_ranking(df17)
