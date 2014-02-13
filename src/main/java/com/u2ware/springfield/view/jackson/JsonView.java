package com.u2ware.springfield.view.jackson;

import java.util.Map;

import org.springframework.web.servlet.view.json.MappingJackson2JsonView;

import com.u2ware.springfield.view.ViewResolverSupport;


public class JsonView extends MappingJackson2JsonView{

	protected Object filterModel(Map<String, Object> model) {
		return ViewResolverSupport.getResponseModel(model);
	}
}

/*
public void afterPropertiesSet() throws Exception {
	ConversionService conversionService = BeanFactoryUtils.beanOfTypeIncludingAncestors(
	wifi		getApplicationContext(), ConversionService.class, true, false);
	super.setExtractValueFromSingleKeyModel(true);
	super.setObjectMapper(new ConversionServiceAwareObjectMapper(conversionService));
}
*/