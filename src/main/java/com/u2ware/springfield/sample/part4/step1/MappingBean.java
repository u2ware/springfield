package com.u2ware.springfield.sample.part4.step1;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import com.u2ware.springfield.config.Springfield;
import com.u2ware.springfield.config.Springfield.Strategy;

@Springfield(
	strategy=Strategy.JPA, 
	methodLevelMapping={
		"*","*.do",
		"findForm.json","findForm.xml","findForm.xls",
		"read.json","read.xml","read.xls"
	}
)
@Entity
public @ToString class MappingBean {

	@Id
	@Getter @Setter private @NotNull String id;
	@Getter @Setter private @NotNull String password;
	@Getter @Setter private @NotNull String name;
	@Getter @Setter private @NotNull Integer age;
}
