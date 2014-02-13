package com.u2ware.springfield.validation.test1;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.validation.Errors;

import com.u2ware.springfield.validation.EntityValidator;
import com.u2ware.springfield.validation.EntityValidatorImpl;

//@Component("springfieldValidationValidator")
public class CustomValidator2 implements EntityValidator<SpringfieldValidation, SpringfieldValidation>{

	protected static final Logger logger = LoggerFactory.getLogger(EntityValidatorImpl.class);

	@Override
	public void find(SpringfieldValidation target, Errors errors) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void read(SpringfieldValidation target, Errors errors) {
		// TODO Auto-generated method stub
	}

	@Override
	public void createForm(SpringfieldValidation target, Errors errors) {
		// TODO Auto-generated method stub
	}

	@Override
	public void create(SpringfieldValidation target, Errors errors) {
		logger.debug("EntityValidator implements create");
		logger.debug("EntityValidator implements create");
		logger.debug("EntityValidator implements create");
		logger.debug("EntityValidator implements create");
	}

	@Override
	public void updateForm(SpringfieldValidation target, Errors errors) {
		// TODO Auto-generated method stub
	}

	@Override
	public void update(SpringfieldValidation target, Errors errors) {
		// TODO Auto-generated method stub
	}

	@Override
	public void delete(SpringfieldValidation target, Errors errors) {
		// TODO Auto-generated method stub
	}


}
