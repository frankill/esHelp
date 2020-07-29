# esHelp

DSL statement for generating elastic search

In the process of improvement

```R
library(esHelp)
 
jsonlite::toJSON(
	boolQ( 
		a %in% LETTERS[4:9],?b) %+% 
		elsticS(inc(letters[1:4]))  %+%
		list(size = 1000
	), 
	pretty=T, 
	auto_unbox=T
)  

```
 