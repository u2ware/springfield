package com.u2ware.springfield.controller;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.BindingResult;
import org.springframework.validation.SmartValidator;

import com.u2ware.springfield.service.GenericService;
import com.u2ware.springfield.service.ValidationException;
import com.u2ware.springfield.service.ValidationGroup;


public abstract class GenericController<D> {

	protected Log logger = LogFactory.getLog(getClass()); 

	public static String ROOT_PATTERN = "<root>";
	public static String UNIQUE_PATTERN = "<unique>";
	
	protected Class<D> domainClass;
	protected GenericService<D> service;
	protected String requestMappingRootPatternValue;
	protected String requestMappingUniquePatternValue;

	@Autowired(required=false)
	protected SmartValidator smartValidator;

	@Autowired(required=false) 
	protected ModelMapper modelMapper;

	public void setSmartValidator(SmartValidator smartValidator) {
		this.smartValidator = smartValidator;
	}
	public void setModelMapper(ModelMapper modelMapper) {
		this.modelMapper = modelMapper;
	}

	public void setDomainClass(Class<D> domainClass) {
		this.domainClass = domainClass;
	}
	public void setService(GenericService<D> service){
		this.service = service;
	}
	public void setRequestMappingRootPatternValue(String root) {
		this.requestMappingRootPatternValue = root;
	}
	public void setRequestMappingUniquePatternValue(String unique) {
		this.requestMappingUniquePatternValue = unique;
	}

	public String getRequestMappingRootPatternValue() {
		return requestMappingRootPatternValue;
	}
	public String getRequestMappingUniquePatternValue() {
		return requestMappingUniquePatternValue;
	}
	public Class<D> getDomainClass() {
		return domainClass;
	}
	
	
	protected ResponseEntity<?> create(D dto, BindingResult errors){
		try{
			logger.debug("request dto: "+dto);
			smartValidator.validate(dto, errors, ValidationGroup.Edit.class);
			
			if(errors.hasErrors()){
				HttpStatus status = HttpStatus.BAD_REQUEST;
				Object body = new ErrorDto(status, errors);
				
				logger.debug("response status: "+status.name()+"\n"+errors);
				return new ResponseEntity<Object>(body, status);
			}
			
			HttpStatus status = HttpStatus.OK;
			Object body = service.create(dto);

			logger.debug("response status: "+status.name());
			return new ResponseEntity<Object>(body, status);

		}catch(ValidationException e){

			errors.rejectValue(e.field(), e.errorCode(), e.errorArgs(), e.defaultMessage());

			HttpStatus status = HttpStatus.BAD_REQUEST;
			Object body = new ErrorDto(status, errors);
			
			logger.debug("response status: "+status.name()+"\n"+errors);
			return new ResponseEntity<Object>(body, status);

		}catch(Exception e){
			HttpStatus status = HttpStatus.INTERNAL_SERVER_ERROR;			
			Object body = new ErrorDto(status, e);
			
			logger.debug("response status: "+status.name(), e);
			return new ResponseEntity<Object>(body, status);
		}
	}
	
	
	protected ResponseEntity<?> read(D dto, BindingResult errors){
		try{
			logger.debug("request dto: "+dto);
			smartValidator.validate(dto, errors, ValidationGroup.Read.class);

			if(errors.hasErrors()){
				HttpStatus status = HttpStatus.BAD_REQUEST;
				Object body = new ErrorDto(status, errors);
				
				logger.debug("response status: "+status.name()+"\n"+errors);
				return new ResponseEntity<Object>(body, status);
			}
			
			HttpStatus status = HttpStatus.OK;
			Object body = service.read(dto);

			logger.debug("response status: "+status.name());
			return new ResponseEntity<Object>(body, status);

		}catch(ValidationException e){

			errors.rejectValue(e.field(), e.errorCode(), e.errorArgs(), e.defaultMessage());

			HttpStatus status = HttpStatus.BAD_REQUEST;
			Object body = new ErrorDto(status, errors);
			
			logger.debug("response status: "+status.name()+"\n"+errors);
			return new ResponseEntity<Object>(body, status);

		}catch(Exception e){
			HttpStatus status = HttpStatus.INTERNAL_SERVER_ERROR;			
			Object body = new ErrorDto(status, e);
			
			logger.debug("response status: "+status.name(), e);
			return new ResponseEntity<Object>(body, status);
		}
	}

	
	protected ResponseEntity<?> update(D dto, BindingResult errors){
		try{
			logger.debug("request dto: "+dto);
			smartValidator.validate(dto, errors, ValidationGroup.Edit.class);
			
			if(errors.hasErrors()){
				HttpStatus status = HttpStatus.BAD_REQUEST;
				Object body = new ErrorDto(status, errors);
				
				logger.debug("response status: "+status.name()+"\n"+errors);
				return new ResponseEntity<Object>(body, status);
			}
			
			HttpStatus status = HttpStatus.OK;
			Object body = service.update(dto);

			logger.debug("response status: "+status.name());
			return new ResponseEntity<Object>(body, status);

		}catch(ValidationException e){

			errors.rejectValue(e.field(), e.errorCode(), e.errorArgs(), e.defaultMessage());

			HttpStatus status = HttpStatus.BAD_REQUEST;
			Object body = new ErrorDto(status, errors);
			
			logger.debug("response status: "+status.name()+"\n"+errors);
			return new ResponseEntity<Object>(body, status);

		}catch(Exception e){
			HttpStatus status = HttpStatus.INTERNAL_SERVER_ERROR;			
			Object body = new ErrorDto(status, e);
			
			logger.debug("response status: "+status.name(), e);
			return new ResponseEntity<Object>(body, status);
		}
	}
	
	
	protected ResponseEntity<?> delete(D dto, BindingResult errors){
		try{
			logger.debug("request dto: "+dto);

			smartValidator.validate(dto, errors, ValidationGroup.Read.class);
			if(errors.hasErrors()){
				HttpStatus status = HttpStatus.BAD_REQUEST;
				Object body = new ErrorDto(status, errors);
				logger.debug("response status: "+status.name()+"\n"+errors);
				return new ResponseEntity<Object>(body, status);
			}

			HttpStatus status = HttpStatus.OK;
			Object body = service.delete(dto);
			logger.debug("response status: "+status.name());
			return new ResponseEntity<Object>(body, status);

		}catch(ValidationException e){

			errors.rejectValue(e.field(), e.errorCode(), e.errorArgs(), e.defaultMessage());

			HttpStatus status = HttpStatus.BAD_REQUEST;
			Object body = new ErrorDto(status, errors);
			logger.debug("response status: "+status.name()+"\n"+errors);
			return new ResponseEntity<Object>(body, status);

		}catch(Exception e){
			HttpStatus status = HttpStatus.INTERNAL_SERVER_ERROR;			
			Object body = new ErrorDto(status, e);
			logger.debug("response status: "+status.name(), e);
			return new ResponseEntity<Object>(body, status);
		}
	}
	
	
	protected ResponseEntity<?> find(String query, D dto, Pageable pageable, BindingResult errors){
		try{
			logger.debug("request dto: "+dto);

			HttpStatus status = HttpStatus.OK;
			Object body = service.find(query, dto, pageable);
			logger.debug("response status: "+status.name());
			return new ResponseEntity<Object>(body, status);

		}catch(ValidationException e){
			errors.rejectValue(e.field(), e.errorCode(), e.errorArgs(), e.defaultMessage());

			HttpStatus status = HttpStatus.BAD_REQUEST;
			Object body = new ErrorDto(status, errors);
			logger.debug("response status: "+status.name()+"\n"+errors);
			return new ResponseEntity<Object>(body, status);

		}catch(Exception e){
			HttpStatus status = HttpStatus.INTERNAL_SERVER_ERROR;			
			Object body = new ErrorDto(status, e);
			logger.debug("response status: "+status.name(), e);
			return new ResponseEntity<Object>(body, status);
		}
	}	
}
/*
protected static HttpServletRequest getCurrentRequest() {
	RequestAttributes requestAttributes = RequestContextHolder.getRequestAttributes();
	Assert.state(requestAttributes != null, "Could not find current request via RequestContextHolder");
	Assert.isInstanceOf(ServletRequestAttributes.class, requestAttributes);
	HttpServletRequest servletRequest = ((ServletRequestAttributes) requestAttributes).getRequest();
	Assert.state(servletRequest != null, "Could not find current HttpServletRequest");
	return servletRequest;
}
*/
