package com.u2ware.springfield.controller;

import org.springframework.validation.Errors;

public interface EntityValidator<T,Q> {

	public void home(Q target, Errors errors);
	public void findForm(Q target, Errors errors) ;
	public void find(Q target, Errors errors);
	public void read(T target, Errors errors);
	public void createForm(T target, Errors errors) ;
	public void create(T target, Errors errors);
	public void updateForm(T target, Errors errors);
	public void update(T target, Errors errors);
	public void delete(T target, Errors errors) ;
	
}
