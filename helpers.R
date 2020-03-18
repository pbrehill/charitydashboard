# Significant figures function
# TODO Add support for non-million denoms
round_dollars <- function(x) {
  x <- round((signif(x, 3)))
  
  #Check it is below billions
  if (nchar(format(x, scientific = FALSE)) < 7) paste0("$", x)
  else if (nchar(format(x, scientific = FALSE)) < 10) paste0("$", x/1000000, "M")
  else paste0("$", x/1000000000, "B")
}
