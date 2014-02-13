package com.u2ware.springfield.sample.part2.step2;

import lombok.Getter;
import lombok.Setter;

import com.u2ware.springfield.config.Springfield;
import com.u2ware.springfield.config.Springfield.Strategy;

@Springfield(
	strategy=Strategy.JPA,
	entity=FindEntity.class,
	topLevelMapping="/part2/step21"
)
public class FindByName {
	
	@Getter @Setter private String name;
}
