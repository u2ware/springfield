package com.u2ware.springfield.sample.part2.step2;

import javax.persistence.Entity;
import javax.persistence.Id;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Entity
public @ToString class FindEntity {

	@Id
	@Getter @Setter private String id;
	@Getter @Setter private String password;
	@Getter @Setter private String name;
	@Getter @Setter private Integer age;
}