package com.u2ware.springfield.sample.part3.step2;

import org.springframework.stereotype.Service;
import org.springframework.validation.Errors;
import org.springframework.validation.ValidationUtils;

import com.u2ware.springfield.sample.part3.step1.TargetBean;
import com.u2ware.springfield.validation.EntityValidatorImpl;


@Service
public class CustomBeanValidator extends EntityValidatorImpl<TargetBean, CustomBean>{

	@Override
	public void create(TargetBean target, Errors errors) {
		super.create(target, errors); //JSR-303
		logger.debug("Overide create ");
		ValidationUtils.rejectIfEmpty(errors, "password", "errorCode");
	}
}