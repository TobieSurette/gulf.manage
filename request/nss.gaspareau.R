

x <- read.nssset(2000:2019, species = 62, survey = "regular", valid = 1)
y <- read.nsscat(2000:2019, species = 62, survey = "regular")
import(x, fill = 0, var = c("number.caught", "weight.caught")) <- y
x$gear <- gear(x$gear)


