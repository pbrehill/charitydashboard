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
