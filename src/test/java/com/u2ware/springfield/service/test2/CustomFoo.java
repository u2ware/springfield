package com.u2ware.springfield.service.test2;

import javax.persistence.Entity;
import javax.persistence.Id;

import lombok.Getter;
import lombok.Setter;

import com.u2ware.springfield.config.Springfield;
import com.u2ware.springfield.config.Springfield.Strategy;

@Springfield(strategy=Strategy.JPA_REPOSITORY_ONLY)
@Entity
public class CustomFoo {

	@Id
	private @Getter @Setter String name;
	private @Getter @Setter Integer age;

}
