library(esHelp)

with_query(

		query(bool(filter(

				a == 1, 
				?b ,
				d %in% letters

			)))

	)

with_query(

		query(bool(

			must(user == 'kimchy'),
			filter(tag == 'tech'),
			must_not( between(age, 10, 20) ),
			should(tag == 'wow', tag == 'elasticsearch'),
			minimum_should_match = 1, 
			boost = 1.0

			))

	)

with_query(
	nested(driver, 
		nested(driver.vehicle, 
			bool(
				must(
					match(driver.vehicle.make, 'Powell Motors')
					)
				)
			)
		)
	)

with_query( 
	has_child(child, 
		match(a,4),
		 max_children= 10, 
		 min_children = 2, 
		 score_mode= min))

with_query(has_parent(parent, tag== 'Elasticsearch'))

with_query(parent_id(`my-child`, id=1))


