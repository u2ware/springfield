package com.u2ware.springfield.sample.part1;

import com.u2ware.springfield.repository.QueryMethod;

import lombok.Getter;
import lombok.Setter;

@QueryMethod("findByNameAndAgeOrderByIdAsc")
public class MyQuery {

	@Getter @Setter public String name;	
	@Getter @Setter public Integer age;
}
