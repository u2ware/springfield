package com.u2ware.springfield.repository.sqlsession.test2;

import javax.persistence.Entity;
import javax.persistence.Id;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import com.u2ware.springfield.config.Springfield;
import com.u2ware.springfield.config.Springfield.Strategy;

@Springfield(strategy=Strategy.SQLSESSION_REPOSITORY_ONLY)
@Entity
public @ToString @NoArgsConstructor @AllArgsConstructor class SpringfieldQuery {

	@Id
	private @Getter @Setter String name;
	private @Getter @Setter Integer age;
}