package com.u2ware.springfield.repository.sqlsession.test3;

import javax.persistence.Entity;
import javax.persistence.Id;

import com.u2ware.springfield.config.Springfield;
import com.u2ware.springfield.config.Springfield.Strategy;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

@Springfield(strategy=Strategy.SQLSESSION_REPOSITORY_ONLY)
@Entity
public @AllArgsConstructor @NoArgsConstructor @ToString class SpringfieldQueryTemplate {

	@Id
	private @Getter @Setter String name;
	private @Getter @Setter Integer age;
	
}
