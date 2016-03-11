---
title: "Working with the KRSP database in R"
author: "Matt Strimas-Mackey"
date: "2016-03-10"
output: html_document
vignette: >
  %\VignetteIndexEntry{Working with the KRSP database in R}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



The `krsp` package offers a set of tools for working with the KRSP MySQL database in R. 
Functions fall into two categories: standard database queries and helper functions for writing custom queries. The former require very little knowledge of R or the database to run. For writing your own queries, this package takes a SQL-free approach, instead using the `dplyr` MySQL interface. Therefore, if you're not already familiar with `dplyr`, it will be useful to read through one of the many [tutorials online](http://seananderson.ca/2014/09/13/dplyr-intro.html) or look through the vignette with `vignette("introduction", "dplyr")`. I provide an extremely brief introduction below.

# `dplyr` basics

`dplyr` is a package designed for manipulating data in data frames quickly and easily. The functions in this package are more consistent, intuitive, and predictable than the equivalent base R functions. There are five core **verbs** (aka functions) for working with single data frames:

- `select()`: select a subset of columns in a data frame.
- `filter()`: select a subset of rows in a data frame.
- `arrange()`: sort the rows in a data frame by the values in a column.
- `mutate()`: add new columns to a data frame that are functions of existing columns.
- `summarise()`: collapse a data frame and summarize data.

Here's a brief demonstration of how these verbs work.


```r
#install.packages("dplyr")
library(dplyr)
mtc <- tbl_df(mtcars)
mtc <- select(mtc, mpg, cyl, horse_power = hp)
mtc
```

```
## Source: local data frame [32 x 3]
## 
##      mpg   cyl horse_power
##    (dbl) (dbl)       (dbl)
## 1   21.0     6         110
## 2   21.0     6         110
## 3   22.8     4          93
## 4   21.4     6         110
## 5   18.7     8         175
## 6   18.1     6         105
## 7   14.3     8         245
## 8   24.4     4          62
## 9   22.8     4          95
## 10  19.2     6         123
## ..   ...   ...         ...
```

```r
filter(mtc, cyl == 4, horse_power < 70)
```

```
## Source: local data frame [5 x 3]
## 
##     mpg   cyl horse_power
##   (dbl) (dbl)       (dbl)
## 1  24.4     4          62
## 2  32.4     4          66
## 3  30.4     4          52
## 4  33.9     4          65
## 5  27.3     4          66
```

```r
arrange(mtc, desc(cyl), horse_power)
```

```
## Source: local data frame [32 x 3]
## 
##      mpg   cyl horse_power
##    (dbl) (dbl)       (dbl)
## 1   15.5     8         150
## 2   15.2     8         150
## 3   18.7     8         175
## 4   19.2     8         175
## 5   16.4     8         180
## 6   17.3     8         180
## 7   15.2     8         180
## 8   10.4     8         205
## 9   10.4     8         215
## 10  14.7     8         230
## ..   ...   ...         ...
```

```r
mutate(mtc, mpg_per_hp = mpg / horse_power)
```

```
## Source: local data frame [32 x 4]
## 
##      mpg   cyl horse_power mpg_per_hp
##    (dbl) (dbl)       (dbl)      (dbl)
## 1   21.0     6         110 0.19090909
## 2   21.0     6         110 0.19090909
## 3   22.8     4          93 0.24516129
## 4   21.4     6         110 0.19454545
## 5   18.7     8         175 0.10685714
## 6   18.1     6         105 0.17238095
## 7   14.3     8         245 0.05836735
## 8   24.4     4          62 0.39354839
## 9   22.8     4          95 0.24000000
## 10  19.2     6         123 0.15609756
## ..   ...   ...         ...        ...
```

```r
summarize(mtc, mean_mpg = mean(mpg), num_records = n())
```

```
## Source: local data frame [1 x 2]
## 
##   mean_mpg num_records
##      (dbl)       (int)
## 1 20.09062          32
```

Note that in all cases the first arguement is a data frame, the subsequent arguements describe what you want to do with the data frame, and the result is always another data frame. Also, columns in the data frame are referenced directly without quotes.

## Chaining operations with `%>%`

The pipe operator, `%>%`, is used to chain multiple operations together in a way that avoids multiple, nested function calls. It can be used with any R functions, but it is particularly useful when working with `dplyr`. Specifically, `x %>% f(y)` is equivalent to `f(x, y)`. Let's look at a simple example, the following two operations are equivalent:


```r
mean(sqrt(exp(mtc$mpg)))
```

```
## [1] 1368477
```

```r
mtc$mpg %>% 
  exp %>% 
  sqrt %>% 
  mean
```

```
## [1] 1368477
```

However, the approach using `%>%` is much easier to read. Let's look at a `dplyr` example:


```r
mtc %>% 
  filter(cyl == 4, horse_power < 70) %>% 
  mutate(mpg_per_hp = mpg / horse_power) %>% 
  arrange(mpg_per_hp)
```

```
## Source: local data frame [5 x 4]
## 
##     mpg   cyl horse_power mpg_per_hp
##   (dbl) (dbl)       (dbl)      (dbl)
## 1  24.4     4          62  0.3935484
## 2  27.3     4          66  0.4136364
## 3  32.4     4          66  0.4909091
## 4  33.9     4          65  0.5215385
## 5  30.4     4          52  0.5846154
```

## Grouped operations

Often you'll want to apply some operation independently to groups of rows in a data frame. The `dplyr` function `group_by()` splits a data frame into groups such that all subsequent operations occur within these groups. `group_by` works with all of the verbs described above, but it's most commonly used with `summarise()`


```r
mtc %>% 
  group_by(cyl) %>% 
  summarise(mean_mpg = mean(mpg))
```

```
## Source: local data frame [3 x 2]
## 
##     cyl mean_mpg
##   (dbl)    (dbl)
## 1     4 26.66364
## 2     6 19.74286
## 3     8 15.10000
```

## Working with databases

The functions within `dplyr` are usually applied to data frames; however, they can be applied to tables within a SQL database in almost exactly the same way. The only additional steps are to define a connection to the database (i.e. specify where the database is and your login credentials), a process addresssed in the next section. Under the hood, `dplyr` converts function calls to SQL code, but this process is invisible to the user.

One nice feature of `dplyr` is that it tries to be as "lazy" as possible when dealing with databases. This means:

- Data are never pulled from the database unless explicitly asked for, e.g. passed to a function outside `dplyr`.
- The database is only queried at the last minute possible, `dplyr` keeps track of all the intermediate operations and only converts them to SQL and queries the database when results are explicitly requested.
- Only the number of rows explicitly needed are pulled from the database.

This is nice because it means queries to the database and downloading data are kept to an absolute minimum. While `dplyr` documentation calls this behaviour "lazy", I'm inclined to just call it "smart".

# Connecting to the KRSP database

