# esHelp

DSL statement for generating elastic search

In the process of improvement

```R
library(esHelp)
 
jsonlite::toJSON(
	bool_q( 
		a %in% LETTERS[4:9],?b) %+% 
		elastic_s(inc(letters[1:4]))  %+%
		c(size = 1000
	), 
	pretty=T, 
	auto_unbox=T
)  

```
 