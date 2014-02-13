package com.u2ware.springfield.sample.others.context;

import java.util.Enumeration;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;

import com.u2ware.springfield.support.context.ContextBroker;

@Controller
public class SessionContextBrokerController {

	protected final Log logger = LogFactory.getLog(getClass());

	@Autowired @Qualifier("sessionContextBroker")
	private ContextBroker sessionContextBroker;

	@RequestMapping("/sessionContextBroker/set")
	public void set(HttpServletRequest req, HttpServletResponse res) throws Exception{
		
		//String key = req.getParameter("key");
		String value = req.getParameter("value");
		
		sessionContextBroker.put(value);
		
		Enumeration<String> names = req.getSession().getAttributeNames();
		while(names.hasMoreElements()){
			String name = names.nextElement();
			res.getWriter().print(name);
			res.getWriter().print("=");
			res.getWriter().println(req.getSession().getAttribute(name));
		}
		
		res.getWriter().flush();
		res.getWriter().close();
	}
	
	@RequestMapping("/sessionContextBroker/get")
	public void get(HttpServletRequest req, HttpServletResponse res)throws Exception{
		
		//String key = req.getParameter("key");
		String value = sessionContextBroker.get(String.class);
		
		
		res.getWriter().println(value);
		
		Enumeration<String> names = req.getSession().getAttributeNames();
		while(names.hasMoreElements()){
			String name = names.nextElement();
			res.getWriter().print(name);
			res.getWriter().print("=");
			res.getWriter().println(req.getSession().getAttribute(name));
		}
		res.getWriter().flush();
		res.getWriter().close();
	}
}
