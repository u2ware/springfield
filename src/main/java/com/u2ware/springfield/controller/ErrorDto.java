package com.u2ware.springfield.controller;

import java.util.List;

import org.springframework.http.HttpStatus;
import org.springframework.validation.Errors;
import org.springframework.validation.ObjectError;

public class ErrorDto {

	public HttpStatus status;
	public String message;
	public List<ObjectError> errors;
	
	public ErrorDto(HttpStatus status, Errors errors){
		this.status = status;
		this.message = "validation error"; 
		this.errors = errors.getAllErrors();
	}
	public ErrorDto(HttpStatus status, Exception e){
		this.status = status;
		this.message = e.getMessage();
	}
	
	public String getMessage() {
		return message;
	}
	public void setMessage(String message) {
		this.message = message;
	}
	public List<ObjectError> getErrors() {
		return errors;
	}
	public void setErrors(List<ObjectError> objectErrors) {
		this.errors = objectErrors;
	}
	public HttpStatus getStatus() {
		return status;
	}
	public void setStatus(HttpStatus httpStatus) {
		this.status = httpStatus;
	}
}
