library(esHelp)

elastic_q(

		query(bool(filter(

				a == 1, 
				?b ,
				d %in% letters

			)))

	)

elastic_q(

		query(bool(

			must(user == 'kimchy'),
			filter(tag == 'tech'),
			must_not( between(age, 10, 20) ),
			should(tag == 'wow', tag == 'elasticsearch'),
			minimum_should_match = 1, 
			boost = 1.0

			))

	)

bool_q(
	nested(driver.vehicle, 
		bool(
			must(
				match(driver.vehicle.make, 'Powell Motors')
				)
			)
		)
	)

bool_q( 
	has_child(
		child,  match(a,4),
		max_children= 10, 
		min_children = 2, 
		score_mode= 'min'
	)
)

bool_q(has_parent(parent, tag== 'Elasticsearch'))

elastic_q(parent_id(`my-child`, id=1))


