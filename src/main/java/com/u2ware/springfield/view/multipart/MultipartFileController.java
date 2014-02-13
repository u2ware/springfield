package com.u2ware.springfield.view.multipart;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.servlet.View;

import com.u2ware.springfield.controller.EntityController;
import com.u2ware.springfield.support.multipart.DownloadFile;
import com.u2ware.springfield.support.multipart.MultipartFileHandler;
import com.u2ware.springfield.view.jackson.JsonView;

public abstract class MultipartFileController {
	
	protected final Log logger = LogFactory.getLog(getClass());

	public abstract MultipartFileHandler getMultipartFileHandler() ;
	private JsonView jsonView = new JsonView();
	
	public void progress(HttpServletRequest request, HttpServletResponse response) throws Exception{
		//write bean to json
	}

	@RequestMapping(value="/upload", method=RequestMethod.POST)
	public View upload(HttpServletRequest request, HttpServletResponse response, @RequestParam("multipartFile")MultipartFile[] multipartFile, Model model) throws Exception{
		
		logger.fatal("upload");
		
		List<DefaultDownloadBean> result = new ArrayList<DefaultDownloadBean>();

		for(MultipartFile f : multipartFile){
			File file = getMultipartFileHandler().saveFile(f);
			
			String contentFile = file.getName();
			String contentName = f.getOriginalFilename();
			String contentType = f.getContentType();
			long contentSize = f.getSize();

			DefaultDownloadBean bean = new DefaultDownloadBean(contentFile, contentName, contentType, contentSize);
			result.add(bean);
		}
		model.addAttribute(EntityController.MODEL_ENTITY, result.size() != 1 ? result : result.get(0));
		
		return jsonView;
	}
	
	@RequestMapping(value="/delete", method=RequestMethod.POST)
	public View delete(HttpServletRequest request, HttpServletResponse response, @RequestParam("multipartFile")String[] multipartFile, Model model) throws Exception{
		
		logger.fatal("delete");

		List<DefaultDownloadBean> result = new ArrayList<DefaultDownloadBean>();
	
		for(String f : multipartFile){

			getMultipartFileHandler().deleteFile(f);
			
			String contentFile = f;
			String contentName = null;
			String contentType = null;
			long contentSize = 0;

			result.add(new DefaultDownloadBean(contentFile, contentName, contentType, contentSize));
		}
		model.addAttribute(EntityController.MODEL_ENTITY, result.size() != 1 ? result : result.get(0));

		return jsonView;
	}

	
	
	
	public static class DefaultDownloadBean implements DownloadFile{
		private String contentFile;
		private String contentName;
		private String contentType;
		private long contentSize;
		public DefaultDownloadBean(String contentFile, String contentName,
				String contentType, long contentSize) {
			super();
			this.contentFile = contentFile;
			this.contentType = contentType;
			this.contentName = contentName;
			this.contentSize = contentSize;
		}
		public String getContentFile() {
			return contentFile;
		}
		public void setContentFile(String contentFile) {
			this.contentFile = contentFile;
		}
		public String getContentType() {
			return contentType;
		}
		public void setContentType(String contentType) {
			this.contentType = contentType;
		}
		public String getContentName() {
			return contentName;
		}
		public void setContentName(String contentName) {
			this.contentName = contentName;
		}
		public long getContentSize() {
			return contentSize;
		}
		public void setContentSize(long contentSize) {
			this.contentSize = contentSize;
		}
	}
	
	
	/*
	@RequestMapping(value="/download", method=RequestMethod.GET)
	public View download(HttpServletRequest request, HttpServletResponse response, @RequestParam("fileName")String fileName, Model model) throws Exception{

		logger.fatal("download");
		
		HttpSession httpSession = request.getSession();
		//logger.debug(httpSession.hashCode());
	
		DefaultDownloadBean bean = (DefaultDownloadBean)httpSession.getAttribute(fileName);
		logger.fatal("session : "+bean);
		if(bean == null){
			throw new HttpClientErrorException(HttpStatus.NOT_FOUND);
		}

		model.addAttribute(EntityController.MODEL_ENTITY, bean);
		
		
		multipartFileView.setAction(Action.DOWNLOAD);
		multipartFileView.setMultipartFileHandler(getMultipartFileHandler());
		return multipartFileView;
	}

	@RequestMapping(value="/stream", method=RequestMethod.GET)
	public View stream(HttpServletRequest request, HttpServletResponse response, @RequestParam("fileName")String fileName, Model model) throws Exception{
		
		logger.fatal("stream");

		HttpSession httpSession = request.getSession();
		//logger.debug(httpSession.hashCode());
	
		DefaultDownloadBean bean = (DefaultDownloadBean)httpSession.getAttribute(fileName);
		logger.fatal("session : "+bean);
		if(bean == null){
			throw new HttpClientErrorException(HttpStatus.NOT_FOUND);
		}
		model.addAttribute(EntityController.MODEL_ENTITY, bean);
		
		multipartFileView.setAction(Action.STREAM);
		multipartFileView.setMultipartFileHandler(getMultipartFileHandler());
		return multipartFileView;
	}
	*/
}