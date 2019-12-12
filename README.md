# esHelp

DSL statement for generating elastic search

In the process of improvement

```R

with_query(

	query(bool(filter(

		a == 1, 
		?b ,
		d %in% letters

		)))

	)
```

```json
{
  "query": {
    "bool": {
      "filter": [
        {
          "term": {
            "a": 1
          }
        },
        {
          "exists": {
            "field": "b"
          }
        },
        {
          "terms": {
            "d": ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"]
          }
        }
      ]
    }
  }
} 
```