letter2num <- function(x) {utf8ToInt(x) - utf8ToInt("a") + 1L}
my_name <- "rebecca"
set.seed( sum( letter2num( my_name)))
sample(1:4,1)