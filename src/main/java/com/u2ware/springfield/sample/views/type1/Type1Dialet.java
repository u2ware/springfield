package com.u2ware.springfield.sample.views.type1;

import java.util.HashSet;
import java.util.Set;

import org.thymeleaf.dialect.AbstractDialect;
import org.thymeleaf.processor.IProcessor;


public class Type1Dialet extends AbstractDialect {

	public static final String DEFAULT_PREFIX = "type1";

	@Override
	public String getPrefix() {
		return DEFAULT_PREFIX;
	}

	@Override
	public boolean isLenient() {
		return false;
	}

	@Override 
	public Set<IProcessor> getProcessors() { 
	    final Set<IProcessor> processors = new HashSet<IProcessor>(); 
	    processors.add(new Type1MessageAttrProcessor()); 
	    return processors; 
	} 	
}
//	
//	
//	private Type1 type1 = new Type1();
//	
//	@Override
//	public String getPrefix() {
//		return DEFAULT_PREFIX;
//	}
//
//	@Override
//	public boolean isLenient() {
//		return false;
//	}
//
//	@Override
//	public Map<String, Object> getAdditionalExpressionObjects(IProcessingContext processingContext) {
//
//		final IContext context = processingContext.getContext();
//		final IWebContext webContext = (context instanceof IWebContext ? (IWebContext) context : null);
//		final Map<String, Object> objects = new HashMap<String, Object>(1, 1.0f);
//		if(webContext != null) {
//			objects.put(JODA_EXPRESSION_OBJECT_NAME, type1);
//		}
//		return objects;
//	}


