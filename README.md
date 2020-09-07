# gulf.manage
Gulf package data management

This package contains functions useful for validating, correcting and accessing data in the southern Gulf of Saint Lawrence's crustacean and groundfish species.

## Checking data frame contents:

The contents of a data frame may be examined using a number of standard methods, such as `summary` and `str`. A slightly more detailed summary is available via `describe`, which also includes decriptions of unique and missing values and index keys.
=======
The contents of a data frame may be examined using a number of standard methods, such as `summary` and `str`. A slightly more detailed summary is available via `describe`, which also includes decriptions of unique and missing values and index keys.
>>>>>>> baaadeca1954d4377846cc3c23f3553d243be0c2

The list of unique values for a given field may also be obtained using the `unique` function. Similarly, the frequencies of unique values can be obtained via the `table` function.

Another useful function includes the `lexicon` function which returns the list of unique words in a vector of character strings. This may be used to check for spelling variations in a comment field, for example.

## Exporting to Excel

Data can be quickly exported to MS Excel via the `excel` function.

## Search and replace:

Character strings
```
str <- gsub("^[ ]", "", str)  # Remove starting spaces from string.
str <- gsub(" $", "", str)    # Remove end spaces from string.
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

The way to understand this is that the strings are first split using the "-" character, which generates a list of 3-element character vectors, in this case. The `lapply` applies a function which extracts the first elements of each character vector. The unlist then collates all the elements, and the `as.numeric` does the numeric conversion.

Why use such a convoluted process? Because the function definition is *very* flexible, enabling us to use any `R` function, including one of our own design.

```
month <- as.numeric(unlist(lapply(strsplit(str, sep = ","), function(x) x[2])))
day <- as.numeric(unlist(lapply(strsplit(str, sep = ","), function(x) x[3])))
```
