package com.u2ware.springfield.validation.test1;

import org.springframework.validation.Errors;

import com.u2ware.springfield.validation.AbstractEntityValidator;

//@Component("springfieldValidationValidator")
public class CustomValidator extends AbstractEntityValidator<SpringfieldValidation, SpringfieldValidation>{

	
	
	
	@Override
	public void create(SpringfieldValidation target, Errors errors) {
		logger.debug("AbstractEntityValidator extends create");
		logger.debug("AbstractEntityValidator extends create");
		logger.debug("AbstractEntityValidator extends create");
		logger.debug("AbstractEntityValidator extends create");
		//super.create(target, errors);
	}
}
