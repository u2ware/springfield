package com.u2ware.springfield.support.multipart;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.fileupload.FileUpload;
import org.apache.commons.fileupload.ProgressListener;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.web.multipart.MultipartException;
import org.springframework.web.multipart.MultipartHttpServletRequest;
import org.springframework.web.multipart.commons.CommonsMultipartResolver;

public class MultipartFileResolver extends CommonsMultipartResolver {

	private static ThreadLocal<MultipartFileProgressor> progressListener = new ThreadLocal<MultipartFileProgressor>();

	private MultipartFileProgressor getMultipartBeanListener() {
		MultipartFileProgressor listener = progressListener.get();
        if(listener == null){
            listener = new MultipartFileProgressor();
            progressListener.set(listener);
        }
        return listener;
	}

	@Override
	public MultipartHttpServletRequest resolveMultipart(HttpServletRequest request) throws MultipartException {

		logger.debug("resolveMultipart start");
		MultipartFileProgressor listener = getMultipartBeanListener();
		logger.debug("resolveMultipart listener "+listener.hashCode());
		MultipartHttpServletRequest multipartRequest = super.resolveMultipart(request);
		logger.debug("resolveMultipart end");
		return multipartRequest;
	}
	
	
	@Override
	protected FileUpload prepareFileUpload(String encoding) {

		logger.debug("prepareFileUpload start");
		FileUpload fileUpload = super.prepareFileUpload(encoding);
		
		MultipartFileProgressor listener = getMultipartBeanListener();
		logger.debug("prepareFileUpload listener "+listener.hashCode());
		fileUpload.setProgressListener(listener);

		logger.debug("prepareFileUpload end");
		return fileUpload;
	}

	public class MultipartFileProgressor implements ProgressListener{

		protected final Log logger = LogFactory.getLog(getClass());

		@Override
		public void update(long pBytesRead, long pContentLength, int pItems) {
			
			logger.debug("update :  "+pBytesRead+" "+pContentLength+" "+pItems);
		}

	}
}

