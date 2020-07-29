# esHelp

DSL statement for generating elastic search

In the process of improvement

```R

library(magrittr)
library(rjson)

boolQ( a %in% LETTERS[4:9],?b) %>% 
  c( elsticS(inc(letters[1:4])) ) %>%
  toJSON %>% cat 

```
 