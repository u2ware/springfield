package com.u2ware.springfield.controller.test1;

import javax.persistence.Entity;
import javax.persistence.Id;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import com.u2ware.springfield.config.Springfield;

@Springfield(topLevelMapping="/controller/test1")
@Entity
public @AllArgsConstructor @NoArgsConstructor @ToString class SpringfieldPagination {

	@Id
	private @Getter @Setter String name;
	private @Getter @Setter Integer age;
	

}
