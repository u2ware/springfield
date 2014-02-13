package com.u2ware.springfield.sample.part3.step1;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;

import com.u2ware.springfield.config.Springfield;
import com.u2ware.springfield.config.Springfield.Strategy;

@Springfield(
	strategy=Strategy.JPA
)
@Entity
public class TargetBean {

	@Id
	@Getter @Setter private @NotNull String id;
	@Getter @Setter private @NotNull String password;
	@Getter @Setter private @NotNull String name;
	@Getter @Setter private @NotNull Integer age;
}