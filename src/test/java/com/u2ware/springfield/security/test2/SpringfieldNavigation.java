package com.u2ware.springfield.security.test2;

import javax.persistence.Entity;
import javax.persistence.Id;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import com.u2ware.springfield.config.Springfield;


@Springfield(topLevelMapping="/security/test1")
@Entity
public @AllArgsConstructor @NoArgsConstructor @ToString class SpringfieldNavigation {

	@Id
	private @Getter @Setter String name;
	private @Getter @Setter Integer age;
	

}
