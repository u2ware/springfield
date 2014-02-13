package com.u2ware.springfield.sample.views.type1;

import org.springframework.context.support.MessageSourceAccessor;
import org.thymeleaf.Arguments;
import org.thymeleaf.dom.Element;
import org.thymeleaf.processor.attr.AbstractTextChildModifierAttrProcessor;

public class Type1MessageAttrProcessor extends AbstractTextChildModifierAttrProcessor{

	private final MessageSourceAccessor message = Type1Message.getAccessor();
	
	public Type1MessageAttrProcessor() { 
	    super("message"); 
	} 

	@Override
	public int getPrecedence() {
		return 10000;
	}

	@Override
	protected String getText(Arguments arguments, Element element, String attributeName) {
		String code = element.getAttributeValue(attributeName);
	    return message.getMessage(code);
	}


}
