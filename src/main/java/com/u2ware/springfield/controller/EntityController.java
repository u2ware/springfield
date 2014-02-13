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
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import com.u2ware.springfield.domain.EntityInformation;
import com.u2ware.springfield.service.EntityService;
import com.u2ware.springfield.validation.EntityValidator;
import com.u2ware.springfield.validation.RejectableException;


public abstract class EntityController<T,Q> {
	
	protected static final Logger logger = LoggerFactory.getLogger(EntityController.class);

	public final static String COMMAND_ID_PATH   = "command_id_path";
	public final static String COMMAND_METHOD    = "command_method";
	public final static String COMMAND_EXTENSION = "command_extension";
	public final static String COMMAND_VIEW      = "command_view";

	public final static String MODEL_INFORMATION      = "model_information";

	public final static String MODEL_ENTITY           = "model_entity";
	public final static String MODEL_QUERY            = "model_query";
	public final static String MODEL_QUERY_PAGEABLE   = "model_query_pageable";
	public final static String MODEL_QUERY_RESULT     = "model_query_result";
	
	private EntityInformation<T,Q> information; 
	private EntityService<T,Q> service;
	private EntityValidator<T,Q> validator;

	/////////////////////////////////////////
	// 
	////////////////////////////////////////
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

	/////////////////////////////////////////
	// 
	////////////////////////////////////////
	@ModelAttribute(MODEL_ENTITY)
	public T createEntityObject(){
		try {
			T command = getInformation().getEntityClass().newInstance();
			logger.info("@ModelAttribute("+MODEL_ENTITY+"): "+getInformation().getEntityClass());	
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
			logger.info("@ModelAttribute("+MODEL_QUERY+"): "+getInformation().getQueryClass());	
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
				logger.info(objectError.toString());
			}
		}
		if(entity == null && query == null) 
			throw new HttpClientErrorException(HttpStatus.NOT_FOUND);
		
		model.addAttribute(MODEL_INFORMATION, getInformation());
		model.addAttribute(MODEL_ENTITY, entity);
		model.addAttribute(MODEL_QUERY, query);
		model.addAttribute(MODEL_QUERY_PAGEABLE, pageable);
		model.addAttribute(MODEL_QUERY_RESULT, queryResult);

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
		String viewName = getInformation().getTopLevelMapping()+"/"
				+commandMethod+commandMethodAppend
				+(StringUtils.hasText(extension) ? "."+extension : "");
		
		if(StringUtils.hasText(getInformation().getAttributesCSV())){
			viewName = viewName + "?" + getInformation().getAttributesCSV();
		}

	
		//logger.info("command_seq: "+metamodel.getSeq());
		logger.info("command_id: "+identityPath);
		logger.info("command_method: "+commandMethod);
		logger.info("command_extension: "+extension);
		logger.info("command_view: "+viewName);
		
		model.addAttribute(COMMAND_ID_PATH , identityPath);
		model.addAttribute(COMMAND_METHOD , commandMethod);
		model.addAttribute(COMMAND_EXTENSION , extension ==  null ? "" : "."+extension);
		model.addAttribute(COMMAND_VIEW , viewName);
		//model.addAttribute(COMMAND_SEQ , metamodel.getSeq());


		return viewName;
	}	
}
