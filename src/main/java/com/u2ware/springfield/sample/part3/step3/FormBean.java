package com.u2ware.springfield.sample.part3.step3;

import lombok.Getter;
import lombok.Setter;

import com.u2ware.springfield.config.Springfield;
import com.u2ware.springfield.config.Springfield.Strategy;


@Springfield(
	strategy=Strategy.DTO,
	identity={"id"}
)
public class FormBean {

	@Getter @Setter private String id;
	@Getter @Setter private Integer age;
	
}