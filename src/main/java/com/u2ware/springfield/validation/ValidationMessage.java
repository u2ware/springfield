package com.u2ware.springfield.validation;

import org.springframework.context.support.MessageSourceAccessor;
import org.springframework.context.support.ReloadableResourceBundleMessageSource;

public class ValidationMessage extends ReloadableResourceBundleMessageSource {

    public ValidationMessage() {
		setBasename("classpath:/com/u2ware/springfield/validation/messages");
    }
   
    
    public static MessageSourceAccessor getAccessor() {
        return new MessageSourceAccessor(new ValidationMessage());
    }
}

