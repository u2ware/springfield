package com.u2ware.springfield.repository.jpa.test1;

import javax.persistence.Entity;
import javax.persistence.Id;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import com.u2ware.springfield.config.Springfield;
import com.u2ware.springfield.config.Springfield.Strategy;

@Springfield(strategy=Strategy.JPA_REPOSITORY_ONLY)
@Entity
public @AllArgsConstructor @NoArgsConstructor @ToString class SpringfieldPagination {

	@Id
	private @Getter @Setter String name;
	private @Getter @Setter Integer age;

	
}
