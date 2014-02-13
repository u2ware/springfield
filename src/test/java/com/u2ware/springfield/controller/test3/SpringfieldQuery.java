package com.u2ware.springfield.controller.test3;

import javax.persistence.Entity;
import javax.persistence.Id;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;


@Entity
public @ToString @NoArgsConstructor @AllArgsConstructor class SpringfieldQuery {

	@Id
	@Getter @Setter private String id;
	@Getter @Setter private String name;
	@Getter @Setter private Integer age;
	@Getter @Setter private Boolean sex;
	
}
