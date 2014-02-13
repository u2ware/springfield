package com.u2ware.springfield.view.jackson;

import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.servlet.view.json.MappingJackson2JsonView;

import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.joda.JodaModule;
import com.u2ware.springfield.view.ViewResolverSupport;


public class JsonView extends MappingJackson2JsonView{

	private static final Logger logger = LoggerFactory.getLogger(JsonView.class);

	
	public JsonView(){
		super();
		super.getObjectMapper().registerModule(new JodaModule());
		super.getObjectMapper().configure(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS, false);
	}
	
	
	protected Object filterModel(Map<String, Object> model) {
		return ViewResolverSupport.getResponseModel(model);
	}

	@Override
	protected void prepareResponse(HttpServletRequest request, HttpServletResponse response) {
		super.prepareResponse(request, response);
		response.setHeader("Access-Control-Allow-Origin","*");
	}

	@Override
	protected void setResponseContentType(HttpServletRequest request, HttpServletResponse response) {
		try{
			String userAgent = request.getHeader("User-Agent");


			if(userAgent.contains("IE") && userAgent.contains("8")){
				logger.debug("IE 8.0 version!!!");
				response.setContentType("text/html;charset=UTF-8");
			}else{
				super.setResponseContentType(request, response);
			}
		}catch(Exception e){
			super.setResponseContentType(request, response);
		}

		logger.debug("setResponseContentType: "+response.getContentType());
	}

}