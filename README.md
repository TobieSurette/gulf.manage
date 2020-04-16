# gulf.manage
Gulf package data management 

This package contains functions useful for validating, correcting and accessing data in the southern Gulf of Saint Lawrence's crustacean and groundfish species.

## String substitutions:
```
str <- gsub("^[ ]", "", str)  # Remove starting blanks from string.
str <- gsub(" $", "", str)    # Remove end blanks from string.
```

## Parsing structured strings:

Given a set of dates:

```
str <- c("2010-06-19", "2019-08-01", "2015-07-10")
```

The goal is to obtain the years months and days which comprise it. There are a number of ways to do so. Some are specific to dates, while others are generally applicable.

```
year <- unlist(lapply(strsplit(str, sep = ","), function(x) x[1]))
```

The way to understand this is that the strings are first split using the "-" character, which generates a list of 3-element character vectors, in this case. The \code{lapply} applies a function which extracts the first elements of each character vector. The unlist then collates all the elements, and the \code{as.numeric} does the numeric conversion. 

Why use such a convoluted process? Because the function definition is *very* flexible.

```
month <- as.numeric(unlist(lapply(strsplit(str, sep = ","), function(x) x[2])))
day <- as.numeric(unlist(lapply(strsplit(str, sep = ","), function(x) x[3])))
```


