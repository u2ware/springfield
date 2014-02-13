package com.u2ware.springfield.controller.test3;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import com.u2ware.springfield.config.Springfield;
import com.u2ware.springfield.repository.QueryMethod;

@Springfield(
		entity=SpringfieldQuery.class,
		topLevelMapping="/controller/test33", 
		methodLevelMapping="find"
)
@QueryMethod("findByIdAndNameOrderByAgeAsc")
public @ToString @NoArgsConstructor @AllArgsConstructor  class  MyQuery {

	@Getter @Setter public String id;
	@Getter @Setter public String name;
}
