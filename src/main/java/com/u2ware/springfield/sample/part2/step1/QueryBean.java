package com.u2ware.springfield.sample.part2.step1;

import lombok.Getter;
import lombok.Setter;

import com.u2ware.springfield.config.Springfield;
import com.u2ware.springfield.config.Springfield.Strategy;
import com.u2ware.springfield.repository.QueryMethod;

@Springfield(
	strategy=Strategy.JPA,
	entity=QueryEntity.class
)
@QueryMethod("findByIdAndPassword")
public class QueryBean {

	@Getter @Setter private String id;
	@Getter @Setter private String password;	
}
