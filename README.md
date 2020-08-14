# esHelp

DSL statement for generating elastic search

In the process of improvement

```R
library(esHelp)
 
a <- bool_q(a %in% LETTERS[4:9],?b) %+% 
		elastic_s(inc(letters[1:4]))  %+%
		list(size = 1000)

b <- elastic_p(
	bool_q(a %in% LETTERS[4:9],?b),
	elastic_s(inc(letters[1:4])),
	size=1000
)

identical(a,b)
 
```
 