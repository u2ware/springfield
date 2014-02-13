package com.u2ware.springfield.view.xstream;

import java.util.Map;

import javax.servlet.ServletException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.oxm.xstream.XStreamMarshaller;
import org.springframework.web.servlet.view.xml.MarshallingView;

import com.u2ware.springfield.view.ModelFilter;

public class XmlView extends MarshallingView{

	protected final Logger logger = LoggerFactory.getLogger(getClass());

	private ModelFilter modelFilter;

	@Autowired(required=false) 
	public void setMarshaller(XStreamMarshaller marshaller) {
		super.setMarshaller(marshaller);
	}
	
	public void setModelFilter(ModelFilter modelFilter) {
		this.modelFilter = modelFilter;
	}

	protected Object locateToBeMarshalled(Map<String, Object> model) throws ServletException {
		if(modelFilter != null){
			return modelFilter.extractOutputModel(model);
		}else{
			return super.locateToBeMarshalled(model);
		}
	}
}
