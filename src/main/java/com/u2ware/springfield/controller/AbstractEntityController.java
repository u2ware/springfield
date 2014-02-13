package com.u2ware.springfield.controller;

import javax.servlet.http.HttpServletRequest;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpStatus;
import org.springframework.ui.Model;
import org.springframework.util.StringUtils;
import org.springframework.validation.BindingResult;
import org.springframework.validation.ObjectError;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import com.u2ware.springfield.domain.EntityInformation;
import com.u2ware.springfield.domain.Pagination;
import com.u2ware.springfield.service.EntityService;
import com.u2ware.springfield.validation.EntityValidator;
import com.u2ware.springfield.validation.RejectableException;


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
public abstract class AbstractEntityController<T,Q> implements EntityController<T,Q> {
	
	protected final Logger logger = LoggerFactory.getLogger(getClass());

	protected abstract EntityInformation<T, Q> getInformation();
	protected abstract EntityService<T,Q> getService();
	protected abstract EntityValidator<T,Q> getValidator();

	/////////////////////////////////////////
	// 
	////////////////////////////////////////
	@ModelAttribute(MODEL_ENTITY)
	public T createEntityObject(){
		try {
			T command = getInformation().getEntityClass().newInstance();
			return command;
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}
	@ModelAttribute(MODEL_QUERY)
	public Q createQueryObject(){
		try {
			Q command = getInformation().getQueryClass().newInstance();
			return command;
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}

	/////////////////////////////////////////
	// 
	////////////////////////////////////////
	protected void validate(BindingResult errors, RejectableException e){
		
		String field = e.getField();
		String errorCode = e.getErrorCode();
		Object[] errorArgs = e.getErrorArgs();
		String defaultMessage = e.getDefaultMessage();

		if(field == null){
			errors.reject(errorCode, errorArgs, defaultMessage);
		}else{
			errors.rejectValue(field, errorCode, errorArgs, defaultMessage);
		}
	}
	
	
	
	protected String resolveViewName(Model model, BindingResult errors, String commandMethod,  Object entity, Object query, Pageable pageable, Object queryResult)throws Exception{
		
		if(errors.hasErrors()){
			for(ObjectError objectError : errors.getAllErrors()){
				logger.warn("validator error: \n"+objectError.toString());
			}
		}
		if(entity == null && query == null) 
			throw new HttpClientErrorException(HttpStatus.NOT_FOUND);
		
		model.addAttribute(MODEL_INFORMATION, getInformation());
		model.addAttribute(MODEL_ENTITY, entity);
		model.addAttribute(MODEL_QUERY, query);
		model.addAttribute(MODEL_QUERY_PAGEABLE, pageable);
		model.addAttribute(MODEL_QUERY_RESULT, queryResult);

		/*
		if(queryResult != null && ClassUtils.isAssignableValue(PageImpl.class, queryResult)){
			PageImpl<?> p = (PageImpl<?>)queryResult;
			logger.warn("getNumber : "+p.getNumber());
			logger.warn("getSize : "+p.getSize());
			logger.warn("getTotalElements : "+p.getTotalElements());
			logger.warn("getTotalPages "+p.getTotalPages());
			
			logger.warn("getCurrentIndex "+p.getCurrentIndex());
			logger.warn("getBeginIndex "+p.getBeginIndex());
			logger.warn("getEndIndex "+p.getEndIndex());
		}
*/		
		
		ServletRequestAttributes attrs = (ServletRequestAttributes)RequestContextHolder.getRequestAttributes();
		HttpServletRequest request = attrs.getRequest();

		
		String commandMethodAppend = "";
///		//change view name by User agent 		
////		String userAgent = request.getHeader("User-Agent");
////		logger.debug(userAgent);
////		if(userAgent == null){
////			commandMethodAppend = "-mobile";
////		}
		
		String identityPath = entity != null ? getInformation().getEntityPath(entity) : "";
		String requestUri = request.getRequestURI();
		String extension = StringUtils.getFilenameExtension(requestUri);

		//String path = ClassUtils.convertClassNameToResourcePath(className)

		String viewName = //getInformation().getBasePackage().replace('.', '/')+
				 getInformation().getTopLevelMapping()+"/"
				+commandMethod+commandMethodAppend
				+(StringUtils.hasText(extension) ? "."+extension : "");
		
		if(StringUtils.hasText(getInformation().getAttributesCSV())){
			viewName = viewName + "?" + getInformation().getAttributesCSV();
		}

	
		logger.warn("response model: "+COMMAND_ID_PATH+"="+identityPath);
		logger.warn("response model: "+COMMAND_METHOD+"="+commandMethod);
		logger.warn("response model: "+COMMAND_EXTENSION+"="+(extension ==  null ? "" : "."+extension));
		logger.warn("response model: "+COMMAND_VIEW+"="+viewName);
		
		
		model.addAttribute(COMMAND_ID_PATH , identityPath);
		model.addAttribute(COMMAND_METHOD , commandMethod);
		model.addAttribute(COMMAND_EXTENSION , extension ==  null ? "" : "."+extension);
		model.addAttribute(COMMAND_VIEW , viewName);


		return viewName;
	}	
	
	
	@RequestMapping(method={RequestMethod.GET, RequestMethod.POST}, value="")
	public String find(
			@RequestParam(required=false,value=ENABLE_PARAMETER_NAME,defaultValue="true")Boolean pageEnable,
			Model model, Pageable pageable, @ModelAttribute(MODEL_QUERY)Q query, BindingResult errors) throws Exception{

		logger.warn("request method: find()");
		logger.warn("request model : "+query);	


		Pageable p = pageEnable ? pageable : null;
		logger.warn("request model : pageable="+p);	
		
		
		getValidator().find(query, errors);
		if(errors.hasErrors()){
			return resolveViewName(model, errors, "find", null, query, p, new Pagination<T>());
		}
		
		try{
			Iterable<?> result = getService().find(query, p);
			if(result == null)
				result = new Pagination<T>();
			
			return resolveViewName(model, errors, "find", null, query, p, result);
		}catch(RejectableException e){
			validate(errors, e);
			return resolveViewName(model, errors, "find", null, query, p, new Pagination<T>());
		}
	}
	
	
	@RequestMapping(method=RequestMethod.GET, value="/"+COMMAND_ID_PATH+"")
	public String read(Model model, @ModelAttribute(MODEL_ENTITY)T entity,BindingResult errors) throws Exception{
		
		logger.warn("request method: read()");
		logger.warn("request model : "+entity);	

		getValidator().read(entity, errors);
		if(errors.hasErrors()){
			return resolveViewName(model, errors, "read", entity, null, null, null);
		}

		try{
			T newEntity = getService().read(entity);
			if(newEntity == null) 
				throw new HttpClientErrorException(HttpStatus.NOT_FOUND);		

			return resolveViewName(model, errors, "read", newEntity, null, null, null);

		}catch(RejectableException e){
			validate(errors, e);
			return resolveViewName(model, errors, "read", entity, null, null, null);
		}
	}

	
	@RequestMapping(method=RequestMethod.GET, value="/new")
	public String createForm(Model model, @ModelAttribute(MODEL_ENTITY)T entity, BindingResult errors) throws Exception{

		logger.warn("request method: createForm()");
		logger.warn("request model : "+entity);	

		getValidator().createForm(entity, errors);
		if(errors.hasErrors()){
			return resolveViewName(model, errors, "createForm", entity, null, null, null);
		}
		
		try{
			T newEntity = getService().createForm(entity);
			return resolveViewName(model, errors, "createForm", newEntity, null, null, null);
			
		}catch(RejectableException e){
			validate(errors, e);
			return resolveViewName(model, errors, "createForm", entity, null, null, null);
		}
	}
	
	
	@RequestMapping(method=RequestMethod.POST, value="/new")
	public String create(Model model, @ModelAttribute(MODEL_ENTITY) T entity, BindingResult errors) throws Exception{
		
		logger.warn("request method: create()");
		logger.warn("request model : "+entity);	

		getValidator().create(entity, errors);
		if(errors.hasErrors()){
			return resolveViewName(model, errors, "createForm", entity, null, null, null);
		}

		try{
			T newEntity = getService().create(entity);		
			return resolveViewName(model, errors,"create", newEntity, null, null, null);
			
		}catch(RejectableException e){
			validate(errors, e);
			return resolveViewName(model, errors, "createForm", entity, null, null, null);
		}
	}
	
	
	@RequestMapping(method=RequestMethod.GET, value="/"+COMMAND_ID_PATH+"/edit")
	public String updateForm(Model model, @ModelAttribute(MODEL_ENTITY)T entity, BindingResult errors) throws Exception{
		
		logger.warn("request method: updateForm()");
		logger.warn("request model : "+entity);	

		getValidator().updateForm(entity, errors);
		if(errors.hasErrors()){
			return resolveViewName(model, errors, "updateForm", entity, null, null, null);
		}

		try{
			T newEntity = getService().updateForm(entity);
			return resolveViewName(model, errors, "updateForm", newEntity, null, null, null);

		}catch(RejectableException e){
			validate(errors, e);
			return resolveViewName(model, errors, "updateForm", entity, null, null, null);
		}
	}
	
	
	@RequestMapping(method=RequestMethod.PUT, value="/"+COMMAND_ID_PATH+"/edit")
	public String update(Model model, @ModelAttribute(MODEL_ENTITY) T entity,BindingResult errors) throws Exception{

		logger.warn("request method: update()");
		logger.warn("request model : "+entity);	

		getValidator().update(entity, errors);
		if(errors.hasErrors()){
			return resolveViewName(model, errors, "updateForm", entity, null, null, null);
		}
		
		try{
			T newEntity = getService().update(entity);
			return resolveViewName(model, errors, "update", newEntity, null, null, null);
			
		}catch(RejectableException e){
			validate(errors, e);
			return resolveViewName(model, errors, "updateForm", entity, null, null, null);
		}
	}

	
	@RequestMapping(method=RequestMethod.DELETE, value="/"+COMMAND_ID_PATH+"/edit")
	public String delete(Model model, @ModelAttribute(MODEL_ENTITY)T entity, BindingResult errors) throws Exception{
		
		logger.warn("request method: delete()");
		logger.warn("request model : "+entity);	

		getValidator().delete(entity, errors);
		if(errors.hasErrors()){
			return resolveViewName(model, errors, "read", entity, null, null, null);
		}

		try{
			T newEntity = getService().delete(entity);
			return resolveViewName(model, errors, "delete", newEntity, null, null, null);
		}catch(RejectableException e){
			validate(errors, e);
			return resolveViewName(model, errors, "read", entity, null, null, null);
		}
	}	
	
}
