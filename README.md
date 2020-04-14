# gulf.manage
Gulf package data management 

This package contains functions useful for validating, correcting and accessing data in the southern Gulf of Saint Lawrence's crustacean and groundfish species.

## String substitutions:

\code{
str <- gsub("^[ ]", "", str) 
str <- gsub(" $", "", str)
}

## Parsing structured strings:

Given a set of dates:

str <- c("2010-06-19", "2019-08-01", "2015-07-10")

The goal is to obtain the years months and days which comprise it. There are a number of ways to do so. Some are specific to dates, while others are generally applicable.





