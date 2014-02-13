package com.u2ware.springfield.view;

import java.util.Map;

public interface ModelFilter {
	
	public Object extractOutputModel(Map<String, Object> model) ;
	//public String extractOutputViewName(Map<String, Object> model, String defaultName) ;
	//public String extractOutputViewPath(Map<String, Object> model, String defaultResource) ;
}
