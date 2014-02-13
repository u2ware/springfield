package com.u2ware.springfield.support.multipart;

import javax.servlet.http.HttpServletRequest;

import org.springframework.util.StringUtils;
import org.springframework.web.context.WebApplicationContext;
import org.springframework.web.context.support.WebApplicationContextUtils;
import org.springframework.web.multipart.MultipartResolver;
import org.springframework.web.multipart.support.MultipartFilter;

public class MultipartFileFilter extends MultipartFilter{

	protected MultipartResolver lookupMultipartResolver(HttpServletRequest request) {
		String filterMultipartResolver = request.getParameter(DEFAULT_MULTIPART_RESOLVER_BEAN_NAME);
		return lookupMultipartResolver(filterMultipartResolver);
	}

	private MultipartResolver lookupMultipartResolver(String name) {
		String beanName = (StringUtils.hasText(name)) ? name : super.getMultipartResolverBeanName();
	
		WebApplicationContext wac = WebApplicationContextUtils.getWebApplicationContext(getServletContext());
		if (wac != null && wac.containsBean(beanName)) {
			if (logger.isDebugEnabled()) {
				logger.debug("Using MultipartResolver '" + beanName + "' for MultipartFilter");
			}
			return wac.getBean(beanName, MultipartResolver.class);
		}
		else {
			return lookupMultipartResolver();
		}
	}
	
}
