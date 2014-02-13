package com.u2ware.springfield.validation;

import org.springframework.validation.SmartValidator;

public class EntityValidatorImpl<T,Q> extends AbstractEntityValidator<T,Q>{

	protected EntityValidatorImpl(){
	}
	
	protected EntityValidatorImpl(SmartValidator smartValidator){
		this.smartValidator = smartValidator;
	}
	

}
