package com.u2ware.springfield.view.html;

import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.http.MediaType;
import org.springframework.web.servlet.View;

import com.u2ware.springfield.view.GenericView;

public class HtmlView implements View{

	private Log logger = LogFactory.getLog(getClass());
	
    private GenericView genericView;

    public HtmlView(GenericView genericView){
    	this.genericView = genericView;
    }
	
	@Override
	public String getContentType() {
		return MediaType.TEXT_HTML_VALUE;
	}

	@Override
	public void render(Map<String, ?> model, HttpServletRequest request, HttpServletResponse response) throws Exception {
		logger.debug(getClass()+" "+genericView);
	}

}
