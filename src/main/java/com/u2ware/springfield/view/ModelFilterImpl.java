package com.u2ware.springfield.view;

import java.util.Map;

public class ModelFilterImpl implements ModelFilter{

	public final static String MODEL_ENTITY           = "model_entity";
	public final static String MODEL_QUERY_RESULT     = "model_query_result";

	@Override
	public Object extractOutputModel(Map<String, Object> model) {
		if(model.get(MODEL_ENTITY) != null){
			return model.get(MODEL_ENTITY);
			
		}else if(model.get(MODEL_QUERY_RESULT) != null){
			return model.get(MODEL_QUERY_RESULT);
			
		}else{
			return null;
		}
	}
	/*
	@Override
	public String extractOutputViewName(Map<String, Object> model, String defaultName) {
		return (String)model.get("");
	}

	@Override
	public String extractOutputViewPath(Map<String, Object> model, String defaultResource) {
		return (String)model.get("");
	}
	*/
}