package com.u2ware.springfield.view.jackson;

import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import net.sf.uadetector.UserAgent;
import net.sf.uadetector.UserAgentStringParser;
import net.sf.uadetector.service.UADetectorServiceFactory;

import org.springframework.web.servlet.view.json.MappingJackson2JsonView;

import com.u2ware.springfield.view.ViewResolverSupport;


public class JsonView extends MappingJackson2JsonView{

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
			UserAgentStringParser parser = UADetectorServiceFactory.getResourceModuleParser();
			UserAgent agent = parser.parse(request.getHeader("User-Agent"));
			if("IE".equals(agent.getName()) && "8".equals(agent.getVersionNumber().getMajor())){
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