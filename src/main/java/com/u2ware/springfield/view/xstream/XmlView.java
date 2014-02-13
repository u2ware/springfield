package com.u2ware.springfield.view.xstream;

import java.util.Map;

import javax.servlet.ServletException;

import org.springframework.web.servlet.view.xml.MarshallingView;

import com.u2ware.springfield.view.ViewResolverSupport;

public class XmlView extends MarshallingView{

	protected Object locateToBeMarshalled(Map<String, Object> model) throws ServletException {
		return ViewResolverSupport.getResponseModel(model);
	}
}
