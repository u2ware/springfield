package com.u2ware.springfield.support.validation;

import java.util.Locale;

import javax.validation.MessageInterpolator;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.context.MessageSource;
import org.springframework.context.MessageSourceAware;

public class ValidationMessageInterpolator implements MessageInterpolator, MessageSourceAware, InitializingBean{
	
	protected final Log logger = LogFactory.getLog(getClass());
	private MessageSource messageSource;

    public void setMessageSource(MessageSource messageSource) {
        this.messageSource = messageSource;
    }
	public void afterPropertiesSet() {
		if (messageSource == null) {
            throw new IllegalStateException("MessageSource was not injected, could not initialize "+ this.getClass().getSimpleName());
        }
	}
	
    public String interpolate(String messageTemplate, Context context) {
        return messageSource.getMessage(convertMessageCode(messageTemplate), new Object[]{}, Locale.getDefault());
    }

    public String interpolate(String messageTemplate, Context context, Locale locale) {
        return messageSource.getMessage(convertMessageCode(messageTemplate), new Object[]{}, locale);
    }

    private String convertMessageCode(String messageTemplate){
    	//logger.debug("messageTemplate : "+messageTemplate);
    	String code = messageTemplate.substring(1 , messageTemplate.length()-1);
    	//logger.debug("messageCode : "+code);
    	return code;
	}
}
