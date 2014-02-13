package com.u2ware.springfield.controller.test3;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import com.u2ware.springfield.config.Springfield;


@Springfield(
		entity=SpringfieldQuery.class,
		topLevelMapping="/controller/test31", 
		methodLevelMapping="find"
)
public @ToString @NoArgsConstructor @AllArgsConstructor class FindByAgeBetween {

	private @Getter @Setter Integer[] age;
}
