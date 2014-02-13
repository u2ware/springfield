package com.u2ware.springfield.support.conversion;

import java.util.Locale;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.expression.ParseException;
import org.springframework.format.Formatter;
import org.springframework.util.StringUtils;

public class StringTrimFormatter implements Formatter<String>{
	
	protected final Log logger = LogFactory.getLog(getClass());
	
	private final boolean emptyAsNull;
	
	public StringTrimFormatter(){
		this.emptyAsNull = true;
	}
	
	public StringTrimFormatter(boolean emptyAsNull){
		this.emptyAsNull = emptyAsNull;
		
		//DeferredResult d;
	}
	public String print(String object, Locale locale) {
		//logger.debug("print-----------------------------------------------------"+object);
		//logger.debug("print-----------------------------------------------------"+object);
		//logger.debug("print-----------------------------------------------------"+object);
		//logger.debug("print-----------------------------------------------------"+object);
		return object;
	}
	public String parse(String text, Locale locale) throws ParseException {
		
		//logger.debug("parse-----------------------------------------------------"+text);
		//logger.debug("parse-----------------------------------------------------"+text);
		//logger.debug("parse-----------------------------------------------------"+text);
		//logger.debug("parse-----------------------------------------------------"+text);
		
		String source = StringUtils.trimLeadingWhitespace(text);
		source = StringUtils.trimTrailingWhitespace(source);
		if(this.emptyAsNull && "".equals(source)){
			return null;
		}
		return source;
	}
}	