package com.u2ware.springfield.view.multipart;

import java.io.File;
import java.io.FileInputStream;
import java.io.OutputStream;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.FileCopyUtils;
import org.springframework.web.servlet.view.AbstractView;

import com.u2ware.springfield.support.multipart.MultipartFileHandler;
import com.u2ware.springfield.view.ViewResolverSupport;

public class MultipartFileBeanView extends AbstractView{

	private static final Logger logger = LoggerFactory.getLogger(MultipartFileBeanView.class);

	private MultipartFileHandler multipartFileHandler;
	private boolean isDownload = false;
	
	public void setMultipartFileHandler(MultipartFileHandler multipartFileHandler) {
		this.multipartFileHandler = multipartFileHandler;
	}
	public void setDownload(boolean isDownload) {
		this.isDownload = isDownload;
	}
	
	protected boolean generatesDownloadContent() {
		return isDownload;
	}
	
	@Override
	protected void renderMergedOutputModel(Map<String, Object> model, HttpServletRequest request, HttpServletResponse response) throws Exception {
		
		if(generatesDownloadContent()){
			download(request, response, model);
		}else{
			stream(request, response, model);
		}
		
	}	
	
	private MultipartFileBean filterModel(Map<String, Object> model)throws Exception {
		try{
			return (MultipartFileBean)ViewResolverSupport.getResponseModel(model);
		}catch(Exception e){
			throw new Exception("Downloading is not found in model");
		}
	}	
	
	protected void download(HttpServletRequest request, HttpServletResponse response, Map<String, Object> model) throws Exception{
		
		MultipartFileBean bean = filterModel(model);
		
		String contentFile = bean.getContentFile();
		String contentName = bean.getContentName();
		String contentType = bean.getContentType();
		long contentSize = bean.getContentSize();
		
		File file = multipartFileHandler.findFile(contentFile);
		logger.debug("contentFile : "+contentFile);
		logger.debug("contentName : "+contentName);
		logger.debug("contentType : "+contentType);
		logger.debug("contentSize : "+contentSize);
		logger.debug("file : "+file);

		String filename = new String(contentName.getBytes("euc-kr") , "8859_1"); 

		response.setHeader("Content-Disposition", "attachment; filename=\"" + filename + "\";");
		response.setHeader("Content-Transfer-Encoding", "binary");
		response.setHeader("Content-Length", ""+contentSize);
		response.setContentType(contentType);
		response.setContentLength((int)contentSize);
		
		this.copy(file, response.getOutputStream());
	}
	
	protected void stream(HttpServletRequest request, HttpServletResponse response, Map<String, Object> model) throws Exception{
		
		MultipartFileBean bean = filterModel(model);

		String contentFile = bean.getContentFile();
		String contentName = bean.getContentName();
		String contentType = bean.getContentType();
		long contentSize = bean.getContentSize();
		
		File file = multipartFileHandler.findFile(contentFile);
		logger.debug("contentFile : "+contentFile);
		logger.debug("contentName : "+contentName);
		logger.debug("contentType : "+contentType);
		logger.debug("contentSize : "+contentSize);
		logger.debug("file : "+file);

		response.setContentType(contentType);
		response.setContentLength((int)contentSize);
		copy(file, response.getOutputStream());
	}
	
	private void copy(File file, OutputStream out) throws Exception{

		FileCopyUtils.copy(new FileInputStream(file), out);
		/*
		FileInputStream fis = null;
		try {
			fis = new FileInputStream(file);
			FileCopyUtils.copy(fis, out);
		} catch(Exception e){
			e.printStackTrace();
		}finally{
			if(fis != null){
				try{
					fis.close();
				}catch(Exception e){
				}
			}
		}
		out.flush();
		*/
	}
}
