package com.u2ware.springfield.validation.test1;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;

import org.joda.time.DateTime;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.format.annotation.NumberFormat;

import com.u2ware.springfield.config.Springfield;



@Springfield(topLevelMapping="/validation/test1")
@Entity
public class SpringfieldValidation {

	@Id
	@NotNull 
	private @Getter @Setter Integer intValue;

	@NotNull
	private @Getter @Setter String stringValue;	

	@NotNull @NumberFormat(pattern="0.0000")
	private @Getter @Setter Float floatValue;	
	
	@org.hibernate.annotations.Type(type="org.joda.time.contrib.hibernate.PersistentDateTime")
	@NotNull @DateTimeFormat(pattern="yyyy-MM-dd")
	private @Getter @Setter DateTime dateTimeValue;	
}
