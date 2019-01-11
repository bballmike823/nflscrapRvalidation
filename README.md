<!-- README.md is generated from README.Rmd. Please edit that file -->
Overview
========

This package is for testing and validating ideas for improving nflscrapR. It tests the accuracy of the ideas against the current implementation and measures the speed of various parts of the program, and what impact the ideas would have on that speed. The ideas will address a single section of the current program and are contained in a single function in the new\_function.R file. new\_function may do quite a bit differently, but everything will be detailed below in smaller independently implementable updates.

This package will contain mechanisms to demonstrate that: a)new\_function runs. b)new\_function runs right. This means exactly replicating all data created or modified by the current code. c)new\_function runs fast. new\_function may not be the fastest possible implementation, but it should provide a noticible benefit to nflscrapR.

Structure of new\_function
--------------------------

The ideas for potential changes will consist of a single function named new\_function that intends to exactly replicate a single portion of the current code. new\_function will exist by itself in its own file. The function inputs any data it needs that exists at the starting point and returns a list containg reproductions of all variables created or modified by the current code that it intends to replicate. new\_function will not do anything to access any data not explicitly given to it as an input. No functions from other packages will be used that are not already used elsewhere in nflscrapR.

new\_function is isolated from the existing code and any variable or function will have the prefix new\_ to avoid any possibility of a naming conflict that affects the functioning of the existing code. new\_function is designed such that when the return statement and all occurrences of the new\_ prefix are removed, it's internals can work if dropped in place of the current code. This isn't so that it can immediately replace the current code, but to make sure that everything the code currently does is reproduced in a way that won't impact functionality.

Confirming new\_function
------------------------

First and foremost, It should be established that new\_function does indeed produce exactly what is currently produced. new\_function will exactly reproduce the current implementation for all available games from the NFL. The NFL has a lot of quirks and sometimes bugs in their data, and there will certainly be new quirks and bugs in the future. new\_function quite possibly will repond differently to future issues, but will always behave the same when the NFL does it's job correctly and handle any current issues in the same manner.

Since the current focus is the get\_play\_data function, I've written a testing function that throws every play from every available game at both new\_function and the current code. If any variable isn't exactly reproduced on any play, the test fails with an error. The current code required access to nfl\_stat\_map, which was defined elsewhere, so that is provided to it. This test obviously takes a while to run. The test can also be run for a specified number of random games. Normally I will include a chunk that shows I've passed the most robust test on my computer. I ran the test before I thought to include it here, and I don't want to run it again. It took a few hours. So here's 25 random games instead: (note: the timing log is wiped every time it is reported here. tictoc starts to slow things down when it logs a lot of events. A better solution will be used in the future, but tictoc was really simple to set up and use) (The timings regarding play\_player\_data and matrices are to demonstrate point 3 later on.)

``` r
library(nflscrapR)
#> Loading required package: nnet
#> Loading required package: magrittr
nflscrapRvalidation::test_new_function_validity(number_of_games = 25, smoke_test = FALSE)
#> Loading required package: XML
#> Loading required package: RCurl
#> Loading required package: bitops
#> 5 games tested
#> creating_new_matrices: number of timings 855 total time 0.09
#> average time 0.000105263157894737
#> creating_new_matrices_and_new_play_player_data: number of timings 855 total time 1.32
#> average time 0.00154385964912281
#> creating_play_player_data_currently: number of timings 855 total time 8.83
#> average time 0.010327485380117
#> smoke_test_current_code: number of timings 855 total time 23.96
#> average time 0.0280233918128655
#> smoke_test_new_function: number of timings 855 total time 3.21
#> average time 0.00375438596491228
#> 10 games tested
#> creating_new_matrices: number of timings 873 total time 0.03
#> average time 3.43642611683849e-05
#> creating_new_matrices_and_new_play_player_data: number of timings 873 total time 1.33
#> average time 0.00152348224513173
#> creating_play_player_data_currently: number of timings 873 total time 8.29
#> average time 0.00949599083619702
#> smoke_test_current_code: number of timings 873 total time 23.25
#> average time 0.0266323024054983
#> smoke_test_new_function: number of timings 873 total time 3.74
#> average time 0.00428407789232532
#> 15 games tested
#> creating_new_matrices: number of timings 907 total time 0.1
#> average time 0.000110253583241455
#> creating_new_matrices_and_new_play_player_data: number of timings 907 total time 1.54
#> average time 0.00169790518191841
#> creating_play_player_data_currently: number of timings 907 total time 8.13
#> average time 0.00896361631753032
#> smoke_test_current_code: number of timings 907 total time 24.07
#> average time 0.0265380374862183
#> smoke_test_new_function: number of timings 907 total time 4.13
#> average time 0.00455347298787211
#> 20 games tested
#> creating_new_matrices: number of timings 873 total time 0.09
#> average time 0.000103092783505155
#> creating_new_matrices_and_new_play_player_data: number of timings 873 total time 1.63
#> average time 0.00186712485681558
#> creating_play_player_data_currently: number of timings 873 total time 7.87
#> average time 0.00901489117983963
#> smoke_test_current_code: number of timings 873 total time 23.1
#> average time 0.0264604810996564
#> smoke_test_new_function: number of timings 873 total time 4.2
#> average time 0.00481099656357388
#> 25 games tested
#> creating_new_matrices: number of timings 842 total time 0.06
#> average time 7.12589073634204e-05
#> creating_new_matrices_and_new_play_player_data: number of timings 842 total time 1.51
#> average time 0.00179334916864608
#> creating_play_player_data_currently: number of timings 842 total time 8.21
#> average time 0.00975059382422803
#> smoke_test_current_code: number of timings 842 total time 22.9
#> average time 0.0271971496437055
#> smoke_test_new_function: number of timings 842 total time 3.81
#> average time 0.0045249406175772
#> new_function passed test of 25 games
#> [1] TRUE
```

validation\_ functions
----------------------

The validation\_ functions are for providing insight on how the ideas would effect the package as a whole. They test and time new\_function against the current code in context of their use in nflscrapR. They operate exactly the same as their nflscrapR equivalents, with small for tweaks recording time and confirming that new\_function matches what is currently produced.

The validation functions test new\_function using the following modifications: At the point new\_function intends to begin, a copy will be made of any variables needed by new\_function. At the point where new\_function rejoins the current code, new function will be run with the copied data. If new\_function does not exactly reproduce all variables created or modified by the current code, execution will be halted immediately with an error. If all variables are reproduced, the new variables are thrown out and the program continues exactly as it normally would.

The function timing\_summary() report a summary of the time used by various parts of the program while running the validation functions during the current R session. This includes both new\_function and the existing code, as well as a few other functions to provide context.

What's different?
=================

The pipe operator and nnet are imported in the namespace
========================================================

This change is pretty minor to implement, but has a significant effect on the operation of the package. It eliminates the need to attatch the nflscrapR package(or at least nnet and magrittr/dplyr) in order to use most of its functions to be used. In the Description file, nnet can be moved from Depends to Imports, and magrittr is unneeded as %&gt;% is provided by dplyr. Attatching nflscrapR would also no longer attach nnet and magrittr.

All that needs to be done to implement this is including the following two lines somewhere in the package using whatever roxygen2/devtools command is normally used to generate documentation.

``` r
#' @importFrom dplyr %>%
#' @import nnet
```

For example, the validation\_scrape\_json\_play\_by\_play function will run fine up until the expected points functions from nflscrapR are called at which point this will become a problem when nnet or magrittr/dplyr isn't loaded. validation\_scrape\_json\_play\_by\_play makes heavy use of the pipe operator, but has no issues because it is provided in the namespace.

What new\_function does differently
===================================

### 1. dplyr and pipes are not used to interact with the play\_data data frame

dplyr and the pipe operator are great for interacting with data frames and manipulating data at a high level. Their clear, simple syntax is unmatched. They aren't blazing fast, but are more than fast enough most of the time. For heavy use on single row dataframes, not so much.

``` r
#currently
df <- df %>% dplyr::mutate(new_stat = stuff)
#becomes
df$new_stat <- stuff
#or
df["new_stat"] <- stuff
```

### 2. if\_else isn't used to assign the indicators

This format was used for the touchback variable in the current package and could be used package wide. It's faster and more concise. This should be pretty straight forward.

``` r
#currently
new_stat = dplyr::if_else(indicator_condition,1,0)
#becomes
new_stat <- as.numeric(condition)
```

### 3. Creates an integer matrix and character matrix to contain the stats and players for the play.

This speeds up builing play\_player\_data considerably. Ultimately, play\_player\_data could be replaced by these matrices, but that is far beyond the scope of these changes.

Matrices are faster than data frames in R. But are limited in that they can only contain a single data type. All interaction can occur with the numeric play\_stats\_matrix and then the sequence variable used to pull the data for the player from the player\_id\_matrix. This is facilitated by the next change, since much of the interaction with stats is currently done by their name, instead of the numeric id. With virtually free conversion of the numeric id to the name and vice versa, the need to store the nfl\_stat is eliminated, and all data needed can be contained in an integer matrix, with player data represented by the sequence variable.

### 4. Uses a named integer vector instead of hashmap

this vector is defined using the two vectors that currently define hashmap The value of this change is that it eliminates the need for hashmap package in nflscrapR.

The lookup process is also significantly faster with the vector than it is with the hashmap, but hashmap was more than fast enough for its purpose, so any performance gained from this change is negligible. A character vector using the statId as the index is even faster. The highest statId is 420, so this wouldn't be especially large. This would give the cleanest syntax of all. I've included a function named hashmap\_vs\_vector\_test using microbenchmark to demonstrate the speed difference of the various options. The mean time for any of the possible vector notations is much faster than the fastest time for hashmap. The character vector is far and away the fastest, too fast for microbenchmark to usefully measure. character vector is the way to go.

``` r
#currently
nfl_stat_map$find(x)
#becomes
names(new_stat_map)[x == new_stat_map)]
#or for vectorized use
names(new_stat_map)[match(x, new_stat_map)]
#with character vector
new_vector_map[x]
```

These are the benchmark results on my computer:

``` r
nflscrapRvalidation::hashmap_vs_vector_test()
#> Warning in microbenchmark::microbenchmark(names(new_stat_map[new_stat_map
#> == : Could not measure a positive execution time for 218 evaluations.
#> Unit: nanoseconds
#>                                                 expr   min    lq
#>       names(new_stat_map[new_stat_map == rand_stat])  1058  1059
#>       names(new_stat_map)[new_stat_map == rand_stat]   705   706
#>           nfl_stat_map$find(as.character(rand_stat)) 12694 13400
#>                    nfl_stat_map$find(rand_stat_char) 10579 11989
#>  names(new_stat_map)[match(rand_stat, new_stat_map)]  1058  1411
#>  names(new_stat_map[match(rand_stat, new_stat_map)])  1058  1411
#>                      character_vector_map[rand_stat]     0     0
#>        mean median    uq     max neval   cld
#>   1438.0962   1411  1411   46899 10000  bc  
#>   1015.7307   1058  1059   21863 10000 ab   
#>  14372.2196  13753 14105  544441 10000     e
#>  13004.9502  11990 12343 4375624 10000    d 
#>   2190.5076   1412  1764 4263844 10000   c  
#>   1952.8437   1411  1763 3558258 10000  bc  
#>     30.3941      1     1   32442 10000 a
```

### 5. The matching used to determine indicators is done with the integer value of the stat, instead of the name

I prefer to use numbers, particularly integers instead of strings when possible. Integer matching should be faster than string matching for these sets. Like the hashmap/vector difference, I doubt the performance gain is particularly meaningful at the moment.

### 6. The sets used to test for the indicators are now defined in vector variables

There isn't much practical reason for this, It just makes code where the actual indicators are set a bit cleaner. It also make new indicator logic more possible. This is detailed in comments inside the code.

### 7. The player stats used for matching the various indicators are stored in a vector containing only the unique values

Instead of pulling a column from a data frame every time that an indicator check occurs, a vector is used, and the duplicates are filtered out using unique(). Any performance gain from this is beyond negligible. I just don't like the idea of checking if an assisted tackle was a touchdown and then checking whether that other assisted tackle was a touchdown.
