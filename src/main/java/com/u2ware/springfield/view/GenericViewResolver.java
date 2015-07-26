package com.u2ware.springfield.view;

import java.util.Locale;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.web.servlet.View;
import org.springframework.web.servlet.view.AbstractCachingViewResolver;

public class GenericViewResolver extends AbstractCachingViewResolver{

    protected Log logger = LogFactory.getLog(getClass());


    private View view;

    public GenericViewResolver(View view){
    	this.view = view;
	}
    
	@Override
	protected View loadView(String viewName, Locale locale) throws Exception {
    	//logger.debug(viewName+" "+getClass());
		return view;
	}

}
