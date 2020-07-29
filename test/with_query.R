library(esHelp)

elsticQ(

		query(bool(filter(

				a == 1, 
				?b ,
				d %in% letters

			)))

	)

elsticQ(

		query(bool(

			must(user == 'kimchy'),
			filter(tag == 'tech'),
			must_not( between(age, 10, 20) ),
			should(tag == 'wow', tag == 'elasticsearch'),
			minimum_should_match = 1, 
			boost = 1.0

			))

	)

elsticQ(
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

elsticQ( 
	has_child(child, 
		match(a,4),
		 max_children= 10, 
		 min_children = 2, 
		 score_mode= min))

elsticQ(has_parent(parent, tag== 'Elasticsearch'))

elsticQ(parent_id(`my-child`, id=1))


