package com.u2ware.springfield.controller;

import org.springframework.http.HttpStatus;
import org.springframework.ui.Model;
import org.springframework.util.ClassUtils;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.client.HttpClientErrorException;

import com.u2ware.springfield.domain.EntityPageImpl;
import com.u2ware.springfield.domain.EntityPageable;
import com.u2ware.springfield.domain.ResultContainable;
import com.u2ware.springfield.domain.ValidationRejectableException;
import com.u2ware.springfield.service.EntityService;

/**
 *                        /{topLevelMapping}/{methodLevelMapping}
 * 		Home:         GET /{path}/
 * 		List        : GET /{path}
 * 		Create Form : GET /{path}/new
 * 		Create     : POST /{path}/new
 * 		Read        : GET /{path}/{id}
 * 		Update Form : GET /{path}/{id}/edit
 * 		Update      : PUT /{path}/{id}/edit
 * 		Delete   : DELETE /{path}/{id}/edit
 * 
 * @author admin
 *
 * @param <T>
 * @param <Q>
 */
public class EntityHandler<T,Q> extends EntityController<T,Q>{
	
	public EntityHandler(EntityService<T, Q> service, HandlerMetamodel<T, Q> metamodel) {
		super(service, metamodel);
	}
	public EntityHandler(String serviceName, EntityService<T,Q> service, String metamodelName, HandlerMetamodel<T,Q> metamodel){
		super(serviceName, service, metamodelName, metamodel);
	}
	public EntityHandler(EntityService<T, Q> service,HandlerMetamodel<T, Q> metamodel, EntityValidator<T, Q> validator) {
		super(service, metamodel, validator);
	}
	public EntityHandler(String serviceName, EntityService<T, Q> service,String metamodelName, HandlerMetamodel<T, Q> metamodel, String validatorName, EntityValidator<T, Q> validator) {
		super(serviceName, service, metamodelName, metamodel, validatorName, validator);
	}

	@RequestMapping(method=RequestMethod.GET, value="/")
	public String home(Model model,  @ModelAttribute(MODEL_QUERY)Q query,BindingResult errors) throws Exception{

		getValidator().home(query, errors);
		if(errors.hasErrors()){
			return resolveViewName(model, errors, "home", null, query, null, null);
		}

		try{
			Object result = getService().home(query);
			return resolveViewName(model, errors, "home", null, query, null, result);

		}catch(ValidationRejectableException e){
			super.validate(errors, e);
			return resolveViewName(model, errors, "home", null, query, null, null);
		}
	}
	
	
	@RequestMapping(method=RequestMethod.GET, value="")
	public String findForm(EntityPageable pageable, Model model, @ModelAttribute(MODEL_QUERY) Q query,BindingResult errors) throws Exception{
		
		logger.info("pageable : "+pageable);

		getValidator().findForm(query, errors);
		if(errors.hasErrors()){
			return resolveViewName(model, errors, "findForm", null, query, pageable, new EntityPageImpl<T>());
		}
		
		try{
			Object result = getService().findForm(query, pageable);

			if(ClassUtils.isAssignable(ResultContainable.class, query.getClass())){
				ResultContainable target = (ResultContainable)query;
				target.setResult(result);
				logger.info("result copied in query object : ");
			}
			
			return resolveViewName(model, errors, "findForm", null, query, pageable, result);
		}catch(ValidationRejectableException e){
			super.validate(errors, e);
			return resolveViewName(model, errors, "findForm", null, query, pageable, new EntityPageImpl<T>());
		}
	}

	
	@RequestMapping(method=RequestMethod.POST, value="")
	public String find(EntityPageable pageable, Model model, @ModelAttribute(MODEL_QUERY) Q query,BindingResult errors) throws Exception{
		
		logger.info("pageable : "+pageable);

		getValidator().find(query, errors);
		if(errors.hasErrors()){
			return resolveViewName(model, errors, "findForm", null, query, pageable, new EntityPageImpl<T>());
		}

		try{
			Object result = getService().find(query, pageable);

			if(ClassUtils.isAssignable(ResultContainable.class, query.getClass())){
				ResultContainable target = (ResultContainable)query;
				target.setResult(result);
				logger.info("result copied in query object : ");
			}
			
			return resolveViewName(model, errors, "find", null, query, pageable, result);
		}catch(ValidationRejectableException e){
			super.validate(errors, e);
			return resolveViewName(model, errors, "findForm", null, query, pageable, new EntityPageImpl<T>());
		}
	}
	
	
	@RequestMapping(method=RequestMethod.GET, value="/"+COMMAND_ID+"")
	public String read(Model model, @ModelAttribute(MODEL_ENTITY)T entity,BindingResult errors) throws Exception{
		
		getValidator().read(entity, errors);
		if(errors.hasErrors()){
			return resolveViewName(model, errors, "read", entity, null, null, null);
		}

		try{
			logger.debug("entity : "+entity);
			
			T newEntity = getService().read(entity);
			logger.debug("newEntity : "+newEntity);
			if(newEntity == null) 
				throw new HttpClientErrorException(HttpStatus.NOT_FOUND);		

			return resolveViewName(model, errors, "read", newEntity, null, null, null);

		}catch(ValidationRejectableException e){
			super.validate(errors, e);
			return resolveViewName(model, errors, "read", entity, null, null, null);
		}
	}

	
	@RequestMapping(method=RequestMethod.GET, value="/new")
	public String createForm(Model model, @ModelAttribute(MODEL_ENTITY)T entity, BindingResult errors) throws Exception{

		getValidator().createForm(entity, errors);
		if(errors.hasErrors()){
			return resolveViewName(model, errors, "createForm", entity, null, null, null);
		}
		
		try{
			T newEntity = getService().createForm(entity);
			return resolveViewName(model, errors, "createForm", newEntity, null, null, null);
			
		}catch(ValidationRejectableException e){
			super.validate(errors, e);
			return resolveViewName(model, errors, "createForm", entity, null, null, null);
		}
	}
	
	
	@RequestMapping(method=RequestMethod.POST, value="/new")
	public String create(Model model, @ModelAttribute(MODEL_ENTITY) T entity, BindingResult errors) throws Exception{
		
		getValidator().create(entity, errors);
		if(errors.hasErrors()){
			return resolveViewName(model, errors, "createForm", entity, null, null, null);
		}

		try{
			T newEntity = getService().create(entity);		
			return resolveViewName(model, errors,"create", newEntity, null, null, null);
			
		}catch(ValidationRejectableException e){
			super.validate(errors, e);
			return resolveViewName(model, errors, "createForm", entity, null, null, null);
		}
	}
	
	
	@RequestMapping(method=RequestMethod.GET, value="/"+COMMAND_ID+"/edit")
	public String updateForm(Model model, @ModelAttribute(MODEL_ENTITY)T entity, BindingResult errors) throws Exception{
		
		getValidator().updateForm(entity, errors);
		if(errors.hasErrors()){
			return resolveViewName(model, errors, "updateForm", entity, null, null, null);
		}

		try{
			T newEntity = getService().updateForm(entity);
			if(newEntity == null) 
				throw new HttpClientErrorException(HttpStatus.NOT_FOUND);

			return resolveViewName(model, errors, "updateForm", newEntity, null, null, null);

		}catch(ValidationRejectableException e){
			super.validate(errors, e);
			return resolveViewName(model, errors, "updateForm", entity, null, null, null);
		}
	}
	
	
	@RequestMapping(method=RequestMethod.PUT, value="/"+COMMAND_ID+"/edit")
	public String update(Model model, @ModelAttribute(MODEL_ENTITY) T entity,BindingResult errors) throws Exception{

		getValidator().update(entity, errors);
		if(errors.hasErrors()){
			return resolveViewName(model, errors, "updateForm", entity, null, null, null);
		}
		
		try{
			T newEntity = getService().update(entity);
			return resolveViewName(model, errors, "update", newEntity, null, null, null);
			
		}catch(ValidationRejectableException e){
			super.validate(errors, e);
			return resolveViewName(model, errors, "updateForm", entity, null, null, null);
		}
	}

	
	@RequestMapping(method=RequestMethod.DELETE, value="/"+COMMAND_ID+"/edit")
	public String delete(Model model, @ModelAttribute(MODEL_ENTITY)T entity, BindingResult errors) throws Exception{
		
		getValidator().delete(entity, errors);
		if(errors.hasErrors()){
			return resolveViewName(model, errors, "read", entity, null, null, null);
		}

		try{
			T newEntity = getService().delete(entity);
			return resolveViewName(model, errors, "delete", newEntity, null, null, null);
		}catch(ValidationRejectableException e){
			super.validate(errors, e);
			return resolveViewName(model, errors, "read", entity, null, null, null);
		}
	}
}

/*
@RequestMapping(method=RequestMethod.POST, value="/append")
public String append(Model model, @ModelAttribute(MODEL_ENTITY_KEY) @Validated T entity, BindingResult errors) throws Exception{
	if(errors.hasErrors()){
		logger.debug(errors);
		return resolveViewName(model, "appendForm", entity, null, null);
	}
	return resolveViewName(model, "append", entity, null, null);
}

@RequestMapping(method=RequestMethod.GET, value="/choice")
public String choiceForm(Model model, @ModelAttribute(MODEL_ENTITY_KEY)T entity, BindingResult errors) throws Exception{
	Q query = createQueryObject();
	Object result = service.findAll(query);
	T newEntity = service.read(entity);
	if(newEntity == null) 
		throw new HttpClientErrorException(HttpStatus.NOT_FOUND);
	return resolveViewName(model, "choiceForm", newEntity, query, result);
}

@RequestMapping(method=RequestMethod.GET, value="/{"+ID+"}/choice")
public String choice(Model model, @ModelAttribute(MODEL_ENTITY_KEY)T entity, BindingResult errors) throws Exception{
	T newEntity = service.read(entity);
	if(newEntity == null) 
		throw new HttpClientErrorException(HttpStatus.NOT_FOUND);
	
	return resolveViewName(model, "choice", newEntity, null, null);
}
		if(ClassUtils.isAssignable(EntityPageContainer.class, query.getClass())){
			EntityPageContainer target = (EntityPageContainer)query;
			target.setPageable(pageable);
			logger.info("pageable copied in query object : ");
		}
*/

