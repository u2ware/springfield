package com.u2ware.springfield.controller;

import com.u2ware.springfield.domain.EntityInformation;
import com.u2ware.springfield.service.EntityService;
import com.u2ware.springfield.validation.EntityValidator;

public class EntityControllerImpl<T,Q> extends AbstractEntityController<T,Q>{
	
	protected EntityControllerImpl(EntityInformation<T,Q> information, EntityService<T,Q> service, EntityValidator<T,Q> validator) {
		setInformation(information);
		setService(service);
		setValidator(validator);
	}

	/////////////////////////////////////////
	// 
	////////////////////////////////////////
	private EntityInformation<T,Q> information; 
	private EntityService<T,Q> service;
	private EntityValidator<T,Q> validator;

	protected EntityInformation<T, Q> getInformation() {
		return information;
	}
	protected EntityService<T,Q> getService() {
		return service;
	}
	protected EntityValidator<T,Q> getValidator() {
		return validator;
	}
	protected void setInformation(EntityInformation<T, Q> information) {
		this.information = information;
	}
	protected void setService(EntityService<T, Q> service) {
		this.service = service;
	}
	protected void setValidator(EntityValidator<T, Q> validator) {
		this.validator = validator;
	}

	

	/*
	@RequestMapping(method={RequestMethod.GET, RequestMethod.POST}, value="/")
	public String home(Model model, @ModelAttribute(MODEL_QUERY)Q query,BindingResult errors) throws Exception{

		logger.warn("request method: home()");
		logger.warn("request model : "+query);	
		
		getValidator().home(query, errors);
		if(errors.hasErrors()){
			return resolveViewName(model, errors, "home", null, query, null, null);
		}

		try{
			Object result = getService().home(query);
			return resolveViewName(model, errors, "home", null, query, null, result);

		}catch(RejectableException e){
			super.validate(errors, e);
			return resolveViewName(model, errors, "home", null, query, null, null);
		}
	}
	*/
	

}