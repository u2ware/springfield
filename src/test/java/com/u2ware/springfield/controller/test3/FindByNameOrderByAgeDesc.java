package com.u2ware.springfield.controller.test3;

import lombok.Getter;
import lombok.Setter;

import com.u2ware.springfield.config.Springfield;

@Springfield(
		entity=SpringfieldQuery.class,
		topLevelMapping="/controller/test32", 
		methodLevelMapping="find"
)
public class FindByNameOrderByAgeDesc {
	
	@Getter @Setter private String name;
}
