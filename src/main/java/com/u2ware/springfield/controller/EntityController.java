package com.u2ware.springfield.controller;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.data.domain.Pageable;
import org.springframework.ui.Model;
import org.springframework.util.StringUtils;
import org.springframework.validation.BindingResult;
import org.springframework.validation.ObjectError;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import com.u2ware.springfield.domain.ValidationRejectableException;
import com.u2ware.springfield.service.EntityService;


public abstract class EntityController<T,Q> {
	
	protected final Log logger = LogFactory.getLog(getClass());

	public final static String COMMAND_ID              = "command_id";
	public final static String COMMAND_SEQ             = "command_seq";
	public final static String COMMAND_PATH            = "command_path";
	public final static String COMMAND_METHOD          = "command_method";
	public final static String COMMAND_EXTENSION       = "command_extension";
	public final static String COMMAND_VIEW            = "command_view";

	public final static String MODEL_ENTITY           = "model_entity";
	public final static String MODEL_ENTITY_METAMODEL = "model_entity_metamodel";

	public final static String MODEL_QUERY            = "model_query";
	public final static String MODEL_QUERY_METAMODEL  = "model_query_metamodel";
	public final static String MODEL_QUERY_PAGEABLE   = "model_query_pageable";
	public final static String MODEL_QUERY_RESULT     = "model_query_result";
	
	private EntityService<T,Q> service;
	private HandlerMetamodel<T,Q> metamodel; 
	private EntityValidator<T,Q> validator;

	private String serviceName;
	private String metamodelName;
	private String validatorName;

	public EntityController(EntityService<T,Q> service, HandlerMetamodel<T,Q> metamodel){
		this.service = service;
		this.metamodel = metamodel;
	}

	public EntityController(EntityService<T,Q> service, HandlerMetamodel<T,Q> metamodel, EntityValidator<T,Q> validator){
		this.service = service;
		this.metamodel = metamodel;
		this.validator = validator;
	}
	public EntityController(String serviceName, EntityService<T,Q> service, String metamodelName, HandlerMetamodel<T,Q> metamodel){
		this.serviceName = serviceName;
		this.service = service;
		this.metamodelName = metamodelName;
		this.metamodel = metamodel;
	}
	public EntityController(String serviceName, EntityService<T,Q> service, String metamodelName, HandlerMetamodel<T,Q> metamodel, String validatorName, EntityValidator<T,Q> validator){
		this.serviceName = serviceName;
		this.service = service;
		this.metamodelName = metamodelName;
		this.metamodel = metamodel;
		this.validatorName = validatorName;
		this.validator = validator;
	}

	public EntityService<T,Q> getService() {
		logger.info("service : "+getServiceName());
		return service;
	}
	public HandlerMetamodel<T, Q> getMetamodel() {
		return metamodel;
	}
	public EntityValidator<T,Q> getValidator() {
		logger.info("validator : "+getValidatorName());
		return validator;
	}
	public String getServiceName() {
		return serviceName;
	}
	public String getMetamodelName() {
		return metamodelName;
	}
	public String getValidatorName() {
		return validatorName;
	}

	public void setValidatorName(String validatorName) {
		this.validatorName = validatorName;
	}

	@ModelAttribute(MODEL_ENTITY)
	public T createEntityObject(){
		try {
			T command = metamodel.getEntityClass().newInstance();
			logger.info("@ModelAttribute("+MODEL_ENTITY+"): "+metamodel.getEntityClass());	
			return command;
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}
	@ModelAttribute(MODEL_QUERY)
	public Q createQueryObject(){
		try {
			Q command = metamodel.getQueryClass().newInstance();
			logger.info("@ModelAttribute("+MODEL_QUERY+"): "+metamodel.getQueryClass());	
			return command;
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}

	protected void validate(BindingResult errors, ValidationRejectableException e){
		
		String errorCode = e.getErrorCode();
		String field = e.getField();
		Object[] errorArgs = e.getErrorArgs();
		String defaultMessage = e.getDefaultMessage();
		
		if(defaultMessage == null){
			defaultMessage = errorCode;
		}
		if(field == null){
			for(String id : metamodel.getIdentity()){
				errors.rejectValue(id, errorCode, errorArgs, defaultMessage);
			}
		}else{
			errors.rejectValue(field, errorCode, errorArgs, defaultMessage);
		}
	}
	
	
	
	protected String resolveViewName(Model model, BindingResult errors, String commandMethod,  T entity, Q query, Pageable pageable, Object queryResult)throws Exception{
		
		if(errors.hasErrors()){
			for(ObjectError objectError : errors.getAllErrors()){
				logger.info(objectError);
			}
		}
		
		model.addAttribute(MODEL_ENTITY, entity);
		model.addAttribute(MODEL_ENTITY_METAMODEL, metamodel);
		
		model.addAttribute(MODEL_QUERY, query);
		model.addAttribute(MODEL_QUERY_PAGEABLE, pageable);
		model.addAttribute(MODEL_QUERY_RESULT, queryResult);
		//model.addAttribute(MODEL_QUERY_METAMODEL, queryMetamodel);

		ServletRequestAttributes attrs = (ServletRequestAttributes)RequestContextHolder.getRequestAttributes();
		HttpServletRequest request = attrs.getRequest();

		
		String commandMethodAppend = "";
///		//change view name by User agent 		
////		String userAgent = request.getHeader("User-Agent");
////		logger.debug(userAgent);
////		if(userAgent == null){
////			commandMethodAppend = "-mobile";
////		}

		String commandId = entity != null ? metamodel.getIdentityUri(entity) : "";
		String requestUri = request.getRequestURI();
		String extension = StringUtils.getFilenameExtension(requestUri);
		String viewName = metamodel.getTopLevelMapping()+"/"
				+commandMethod+commandMethodAppend
				+(StringUtils.hasText(extension) ? "."+extension : "");
		
		if(StringUtils.hasText(metamodel.getAttributesCSV())){
			viewName = viewName + "?" + metamodel.getAttributesCSV();
		}
		
		//logger.info("command_seq: "+metamodel.getSeq());
		logger.info("command_path: "+metamodel.getTopLevelMapping());
		logger.info("command_id: "+commandId);
		logger.info("command_method: "+commandMethod);
		logger.info("command_extension: "+extension);
		logger.info("command_view: "+viewName);
		
		model.addAttribute(COMMAND_PATH , metamodel.getTopLevelMapping());
		model.addAttribute(COMMAND_ID , commandId);
		model.addAttribute(COMMAND_METHOD , commandMethod);
		model.addAttribute(COMMAND_EXTENSION , extension ==  null ? "" : "."+extension);
		model.addAttribute(COMMAND_VIEW , viewName);
		//model.addAttribute(COMMAND_SEQ , metamodel.getSeq());


		return viewName;
	}
	/*
	public abstract String home(Model model, Q query,BindingResult errors) throws Exception;
	
	public abstract String findForm(EntityPageable pageable, Model model, Q query,BindingResult errors) throws Exception;
	
	public abstract String find(EntityPageable pageable, Model model, Q query,BindingResult errors) throws Exception;
	
	public abstract String read(Model model, T entity,BindingResult errors) throws Exception;

	public abstract String createForm(Model model, T entity, BindingResult errors) throws Exception;
	
	public abstract String create(Model model, T entity, BindingResult errors) throws Exception;

	public abstract String updateForm(Model model, T entity, BindingResult errors) throws Exception;
	
	public abstract String update(Model model, T entity,BindingResult errors) throws Exception;
	
	public abstract String delete(Model model, T entity, BindingResult errors) throws Exception;
	*/
	
	
}
