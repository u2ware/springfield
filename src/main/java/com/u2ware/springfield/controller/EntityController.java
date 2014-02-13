package com.u2ware.springfield.controller;

import org.springframework.data.domain.Pageable;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;


public interface EntityController<T,Q> {
	
	public final static String COMMAND_ID_PATH   = "command_id_path";
	public final static String COMMAND_METHOD    = "command_method";
	public final static String COMMAND_EXTENSION = "command_extension";
	public final static String COMMAND_VIEW      = "command_view";

	public final static String MODEL_INFORMATION     = "model_information";

	public final static String MODEL_ENTITY           = "model_entity";
	public final static String MODEL_QUERY            = "model_query";
	public final static String MODEL_QUERY_PAGEABLE   = "model_query_pageable";
	public final static String MODEL_QUERY_RESULT     = "model_query_result";
	
	public static final String PAGE_PARAMETER_NAME   = "model_query_pageable.pageNumber";
	public static final String SIZE_PARAMETER_NAME   = "model_query_pageable.pageSize";
	public static final String SORT_PARAMETER_NAME   = "model_query_pageable.pageSort";
	public static final String ENABLE_PARAMETER_NAME = "model_query_pageable.pageEnable";

	

	public String find(Boolean pageEnable, Model model, Pageable pageable, Q query, BindingResult errors) throws Exception;
	
	public String read(Model model, T entity, BindingResult errors) throws Exception;
	
	public String createForm(Model model, T entity, BindingResult errors) throws Exception;
	
	public String create(Model model, T entity, BindingResult errors) throws Exception;
	
	public String updateForm(Model model, T entity, BindingResult errors) throws Exception;
	
	public String update(Model model, T entity, BindingResult errors) throws Exception;
	
	public String delete(Model model, T entity, BindingResult errors) throws Exception;

}
