package com.u2ware.springfield.sample.part3.step2;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import com.u2ware.springfield.config.Springfield;
import com.u2ware.springfield.config.Springfield.Strategy;
import com.u2ware.springfield.repository.QueryMethod;
import com.u2ware.springfield.sample.part3.step1.TargetBean;

@Springfield(
	strategy=Strategy.JPA, 
	entity=TargetBean.class
)
@QueryMethod("findByIdAndPasswordOrderByAgeDesc")
public @ToString class CustomBean {
	
	@Getter @Setter private String id;
	@Getter @Setter private String password;
}