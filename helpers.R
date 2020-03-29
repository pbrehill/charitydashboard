# Significant figures function
round_dollars <- function(x) {
  x <- round((signif(x, 3)))
  
  #Check it is below billions
  if (nchar(format(x, scientific = FALSE)) < 7) paste0("$", x)
  else if (nchar(format(x, scientific = FALSE)) < 10) paste0("$", x/1000000, "M")
  else paste0("$", x/1000000000, "B")
}

# Capitalise first letter
capFirst <- function(s) {
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}

# Active data categories
active_categories <- function(variable_array) {
  
  selected_cats <- variable_groupings[variable_array == variable_groupings$Var, 'Categorisation']
  
  selected_cats <- unique(selected_cats) %>%
    pull()
  
  return(selected_cats)
}

# Create collapsable menu

collapseVarBox <- function(variable_type, reactive_values, selected = NULL) {
  
  selected_style <- ifelse(!is.null(rv[[variable_type]]), 'primary', 'secondary')
  
  bsCollapsePanel(
    title = capFirst(variable_type),
    checkboxGroupInput(
      inputId = paste0("vs_", variable_type),
      "select variables to show",
      choices = dplyr::pull(variable_groupings[variable_groupings$Categorisation == variable_type, 'Var']),
      selected = isolate(reactive_values[[variable_type]])
    ),
    style = selected_style
    
  )
}

# Check for each value of a vector if it is an element of another vector

vec_in_vec <- function(longer, shorter){
  unlist(
    lapply(
      longer,
      FUN = function(x) {
        is.element(x, shorter)
      }
    )
  )
}


