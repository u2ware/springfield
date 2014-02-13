package com.u2ware.springfield.validation;

import java.util.Locale;

import javax.validation.MessageInterpolator;

import org.springframework.context.MessageSource;
import org.springframework.context.MessageSourceAware;
import org.springframework.context.support.MessageSourceAccessor;

public class ValidationMessageInterpolator implements MessageInterpolator, MessageSourceAware{
	
	//private static final Logger logger = LoggerFactory.getLogger(ValidationMessageInterpolator.class);
	
	protected final MessageSourceAccessor validationMessageSource = ValidationMessage.getAccessor();
	private MessageSourceAccessor messageSourceAccessor;

	public void setMessageSource(MessageSource messageSource) {
		this.messageSourceAccessor = new MessageSourceAccessor(messageSource);
	}

    public String interpolate(String messageTemplate, Context context) {
    	try{
    		return messageSourceAccessor.getMessage(convertMessageCode(messageTemplate));
    	}catch(Exception e){
    		return validationMessageSource.getMessage(convertMessageCode(messageTemplate));
    	}
    }

    public String interpolate(String messageTemplate, Context context, Locale locale) {
    	try{
    		return messageSourceAccessor.getMessage(convertMessageCode(messageTemplate), locale);
    	}catch(Exception e){
    		return validationMessageSource.getMessage(convertMessageCode(messageTemplate), locale);
    	}
    }

    private String convertMessageCode(String messageTemplate){
    	String code = messageTemplate.substring(1 , messageTemplate.length()-1);
    	return code;
	}
}
