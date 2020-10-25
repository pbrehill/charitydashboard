library(tidyverse)
library(magrittr)
library(lubridate)

# Define download excel function
download_sheet <- function(url, sheetno = 1) {
  ext <- paste0('\\.', gsub("^.*\\.","", url))
  temp = tempfile(fileext = ext)
  download.file(url, destfile=temp, mode='wb')
  output <- readxl::read_excel(temp, sheet = sheetno)
  
  # Standardise names
  names(output) <- tolower(names(output))
  names(output) <- gsub("_","\\.", names(output))
  names(output) <- gsub(" ","\\.", names(output))
  
  # Clean types
  
  output
}

remove_posix <- function (x) {
  x %<>% mutate_if(is.POSIXt, as.character)
  x
}


# Columns with same name, different types
join_year_dfs <- function (x, y) {
  x %<>% remove_posix()
  y %<>% remove_posix()
  
  # Get variable types
  x_types <- unlist(map(x, class))
  y_types <- unlist(map(y, class))
  
  # Create dataframe
  typesdf <- x_types %>% 
    as.data.frame %>%
    rownames_to_column(var = "variable")
  
  # Iterate through variables
  ## This loop is awful but since only I will see it, I must learn to forgive myself
  typesdf$type_match <- NA
  for (i in 1:nrow(typesdf)) {
    typesdf$type_match[i] <- typesdf$. == y_types[typesdf$variable[i]]
  }
  
  # Change variable types where needed
  change_var_names <- typesdf %>% 
    filter(!type_match) %>% 
    select(variable) %>% 
    pull()
  
  x %<>% 
    mutate_at(vars(change_var_names), as.character)
  
  y %<>% 
    mutate_at(vars(change_var_names), as.character)
  
  # Bind rows
  joined_df <- bind_rows(x, y)
  
  write_csv(joined_df, 'temp.csv')
  
  output <- read_csv('temp.csv')
  
  return(output)
}

# Load from online

# 2018
df18 <- 'https://data.gov.au/data/dataset/cfc1a18e-f4e0-4ed8-9a19-36b59b7a3d5b/resource/9312452f-fced-476e-a6ec-2b2327796a34/download/datadotgov_ais18.xlsx' %>%
  download_sheet()


# 2017
## Non-group
df17ng <- 'https://data.gov.au/data/dataset/a1f8626c-fefb-4c4d-86ea-deaa04fb1f6e/resource/8d020b50-700f-4bc4-8c78-79f83d99be7a/download/datadotgov_ais17.xlsx' %>%
  download_sheet()

# Group
df17g <- 'https://data.gov.au/data/dataset/a1f8626c-fefb-4c4d-86ea-deaa04fb1f6e/resource/8a18e71a-58d1-414f-adee-c9066560b05c/download/acnc-2017-group-ais-dataset-approved-reporting-groups.xlsx' %>%
  download_sheet()

# Combine
df17 <- join_year_dfs(df17ng, df17g)


# 2016
## Non-group
df16ng <- 'https://data.gov.au/data/dataset/7e073d71-4eef-4f0c-921b-9880fb59b206/resource/b4a08924-af4f-4def-96f7-bf32ada7ee2b/download/datadotgov_ais16.xlsx' %>%
  download_sheet()

# Group
df16g <- 'https://data.gov.au/data/dataset/7e073d71-4eef-4f0c-921b-9880fb59b206/resource/8932e61f-0124-48ac-b3be-f3e49f03b33f/download/group-ais16-datagov-final.xlsx' %>%
  download_sheet()

# Combine
df16 <- join_year_dfs(df16ng, df16g)


# 2015
## Non-group
df15ng <- 'https://data.gov.au/data/dataset/86cad799-2601-4f23-b02c-c4c0fc3b6aff/resource/569b8e48-a0ad-4008-9d95-7f91b6cfa2aa/download/datadotgov_ais15.xlsx' %>%
  download_sheet()

## Group
df15g <- 'https://data.gov.au/data/dataset/86cad799-2601-4f23-b02c-c4c0fc3b6aff/resource/3d57d625-b183-4677-85b1-009b2000ed02/download/2015-ais-group-registered-charities.xls' %>%
  download_sheet()

# Combine
df15 <- join_year_dfs(df15ng, df15g)


# 2014
## Non-group
df14ng <- 'https://data.gov.au/data/dataset/d7992845-5d3b-4868-b012-71f672085412/resource/4d65259d-1ccf-4c78-a223-e2bd49dc5fb1/download/datadotgov_ais14.xlsx' %>%
  download_sheet()

## Group
df14g <- 'https://data.gov.au/data/dataset/d7992845-5d3b-4868-b012-71f672085412/resource/b3b49610-7f47-41b2-9350-49b7fd8acd93/download/2014-ais-data-for-group-reporting-charities.xlsx' %>%
  download_sheet(sheetno = 2)

# Combine
df14 <- join_year_dfs(df14ng, df14g)



# Turn them into a list
years <- list(df14 = df14, df15 = df15, df16 = df16, df17 = df17, df18 = df18)


# Filter for env
env_years <- years %>%
  map(function (x) {
    x %<>%
      filter(`main.activity` == 'Environmental activities')
  }
)


# Get year sums for all numeric vars
year_sums <- map(years, function (x) {
  x %>%
    select_if(is.numeric) %>%
    colSums(na.rm = TRUE)
  }
)
year_sums <- data.frame(do.call(bind_rows, year_sums))


# Get year sums for env
env_year_sums <- map(env_years, function (x) {
  x %>%
    select_if(is.numeric) %>%
    colSums(na.rm = TRUE)
}
)
env_year_sums <- data.frame(do.call(bind_rows, env_year_sums))


# Get average top 25 2016 - 18
## Total donations for each company
env_years$df18$abn %<>% as.numeric

three_yr_total <- env_years %>%
  map(~select(.x, abn, charity.name, donations.and.bequests)) %>%
  bind_rows(.id = 'year') %>%
  group_by(abn) %>%
  summarise(sum_donation = sum(donations.and.bequests, na.rm = TRUE)) %>%
  arrange(desc(sum_donation)) %>%
  left_join(env_years$df18[c('abn', 'charity.name')], by = 'abn')

top20 <- head(three_yr_total, 20)
write_csv(top20, 'top20orgs.csv')


# Top perc

top_sum <- env_years$df18 %>%
  filter(abn %in% top$abn) %>%
  select(donations.and.bequests) %>%
  arrange(desc(donations.and.bequests)) %>%
  sum

perc_represented <- top_sum / sum(env_years$df18$donations.and.bequests, na.rm = TRUE)

# Giving deciles in 2018

env_years$df18 %<>%
  mutate(giving.decile = as.factor(ntile(donations.and.bequests, 10)))

# Graph it
env_years$df18 %>%
  group_by(giving.decile) %>%
  summarise(don_sum = sum(donations.and.bequests)) %>%
  mutate(prop.n = prop.table(.$don_sum)) %>%
  ggplot(aes(fill = giving.decile, y = prop.n, x = 1)) +
    geom_col(position = 'stack')
