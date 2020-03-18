library(readxl)
library(httr)
# Process csv
# reader <- reactiveFileReader(1000, NULL, 'data.csv', read.csv)

# 2017
url1 <- "https://data.gov.au/data/dataset/a1f8626c-fefb-4c4d-86ea-deaa04fb1f6e/resource/8d020b50-700f-4bc4-8c78-79f83d99be7a/download/datadotgov_ais17.xlsx"
GET(url1, write_disk(tf <- tempfile(fileext = ".xlsx")))
df17 <- read_excel(tf, 1L)


