package com.u2ware.springfield.validation;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.validation.Errors;
import org.springframework.validation.SmartValidator;
import org.springframework.validation.ValidationUtils;

public class EntityValidatorImpl<T,Q> implements EntityValidator<T,Q>{

	protected final Log logger = LogFactory.getLog(getClass());

	//@Autowired(required=false)
	//protected javax.validation.Validator jsr303Validator;

	@Autowired @Qualifier("validator")
	protected SmartValidator smartValidator;
	
	public void home(Q target, Errors errors) {
	}

	public void findForm(Q target, Errors errors) {
	}

	public void find(Q target, Errors errors) {
		if(smartValidator != null){
			ValidationUtils.invokeValidator(smartValidator, target, errors);
		}
	}

	public void read(T target, Errors errors) {
		
	}

	public void createForm(T target, Errors errors) {
		
	}

	public void create(T target, Errors errors) {
		if(smartValidator != null){
			ValidationUtils.invokeValidator(smartValidator, target, errors);
		}
	}

	public void updateForm(T target, Errors errors) {
		
	}

	public void update(T target, Errors errors) {
		if(smartValidator != null){
			ValidationUtils.invokeValidator(smartValidator, target, errors);
		}
	}

	public void delete(T target, Errors errors) {
	}
}
