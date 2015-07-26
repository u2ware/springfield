package com.u2ware.springfield.view;

import java.util.HashMap;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;

import com.fasterxml.jackson.databind.ObjectMapper;

public class GenericView {

    @Autowired(required=false)
    private ObjectMapper objectMapper;

    public GenericView(ObjectMapper objectMapper) {
		this.objectMapper = objectMapper;
	}

	@SuppressWarnings("unchecked")
	public Map<String, Object> parse(String content) throws Exception {
		return objectMapper.readValue(content, Map.class);
	}
	public String format(Object value) throws Exception {
		return objectMapper.writeValueAsString(value);
	}
    
    public void render(Exception e, HttpServletRequest request, HttpServletResponse response) throws Exception{
		Map<String,Object> value = new HashMap<String,Object>();
		value.put("status", HttpStatus.BAD_REQUEST);
		value.put("message", e.getMessage());
		render(value, request, response);
	}

    public void render(Object value, HttpServletRequest request, HttpServletResponse response) throws Exception{
		response.setContentType(MediaType.APPLICATION_JSON_VALUE);
		objectMapper.writeValue(response.getOutputStream(), value);
		response.getOutputStream().flush();
		response.getOutputStream().close();
    }
    

}
