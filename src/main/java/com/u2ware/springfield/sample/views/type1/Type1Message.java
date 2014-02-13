package com.u2ware.springfield.sample.views.type1;

import org.springframework.context.support.MessageSourceAccessor;
import org.springframework.context.support.ReloadableResourceBundleMessageSource;

public class Type1Message extends ReloadableResourceBundleMessageSource {

    public Type1Message() {
		setBasename("classpath:/com/u2ware/springfield/sample/views/type1/messages");
    }
    
    public static MessageSourceAccessor getAccessor() {
        return new MessageSourceAccessor(new Type1Message());
    }
}