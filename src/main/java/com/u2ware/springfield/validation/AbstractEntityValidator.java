package com.u2ware.springfield.validation;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.Errors;
import org.springframework.validation.SmartValidator;
import org.springframework.validation.ValidationUtils;

public abstract class AbstractEntityValidator<T,Q> implements EntityValidator<T,Q>{

	protected static final Logger logger = LoggerFactory.getLogger(EntityValidatorImpl.class);

	@Autowired(required=false)
	protected SmartValidator smartValidator;

	public SmartValidator getSmartValidator() {
		return smartValidator;
	}

	/*
	public void home(Q target, Errors errors) {
	}
	*/

	public void find(Q target, Errors errors) {
		if(getSmartValidator() != null){
			ValidationUtils.invokeValidator(getSmartValidator(), target, errors);
		}
	}

	public void read(T target, Errors errors) {
		
	}

	public void createForm(T target, Errors errors) {
		
	}

	public void create(T target, Errors errors) {
		if(getSmartValidator() != null){
			ValidationUtils.invokeValidator(getSmartValidator(), target, errors);
		}
	}

	public void updateForm(T target, Errors errors) {
		
	}

	public void update(T target, Errors errors) {
		if(getSmartValidator() != null){
			ValidationUtils.invokeValidator(getSmartValidator(), target, errors);
		}
	}

	public void delete(T target, Errors errors) {
	}
}
