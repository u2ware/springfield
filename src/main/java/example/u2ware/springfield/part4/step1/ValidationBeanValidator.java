package example.u2ware.springfield.part4.step1;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.ValidationUtils;

import com.u2ware.springfield.controller.EntityValidatorImpl;

@Component
public class ValidationBeanValidator extends EntityValidatorImpl<ValidationBean,ValidationBean>{

	protected final Log logger = LogFactory.getLog(getClass());

	@Override
	public void create(ValidationBean target, Errors errors) {
		
		super.create(target, errors);

		ValidationUtils.rejectIfEmpty(errors, "stringValue", null, "empty");
		
		ValidationBean bean = (ValidationBean)target;
		if("NO".equalsIgnoreCase(bean.getStringValue())){
			errors.rejectValue("stringValue" , null, "'NO' equals Ignore Case");
		}
	}
}
