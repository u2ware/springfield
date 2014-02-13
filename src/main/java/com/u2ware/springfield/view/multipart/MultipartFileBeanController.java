package com.u2ware.springfield.view.multipart;

import java.util.ArrayList;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.ui.Model;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.servlet.View;

import com.u2ware.springfield.support.multipart.MultipartFileHandler;
import com.u2ware.springfield.support.multipart.UploadFileNameResolver;
import com.u2ware.springfield.view.jackson.JsonView;

public abstract class MultipartFileBeanController {
	
	protected final Logger logger = LoggerFactory.getLogger(getClass());

	public final static String MODEL_ENTITY           = "model_entity";
	public final static String MODEL_QUERY_RESULT     = "model_query_result";

	public abstract MultipartFileHandler getMultipartFileHandler() ;
	public abstract UploadFileNameResolver getMultipartFileNameResolver(String name) ;
	
	private JsonView jsonView = new JsonView();
	
	public void progress(HttpServletRequest request, HttpServletResponse response) throws Exception{
		//write bean to json
	}

	
	protected View uploadHandle(MultipartFile[] multipartFile, String name, Model model) throws Exception{
		
		logger.warn("uploadHandle");
		
		List<MultipartFileBean> result = new ArrayList<MultipartFileBean>();
		
		for(MultipartFile f : multipartFile){
			String contentFile = getMultipartFileHandler().uploadFile(f, getMultipartFileNameResolver(name));
			String contentName = f.getOriginalFilename();
			String contentType = f.getContentType();
			long contentSize = f.getSize();

			MultipartFileBean bean = new MultipartFileBeanBase(contentFile, contentName, contentType, contentSize);
			result.add(bean);
		}
		model.addAttribute(MODEL_ENTITY, result.size() != 1 ? result : result.get(0));
		
		return jsonView;
	}
	
	protected View deleteHandle(String[] multipartFile, Model model) throws Exception{
		
		logger.warn("deleteHandle");

		List<MultipartFileBean> result = new ArrayList<MultipartFileBean>();
	
		for(String f : multipartFile){

			getMultipartFileHandler().deleteFile(f);
			
			String contentFile = f;
			String contentName = null;
			String contentType = null;
			long contentSize = 0;

			result.add(new MultipartFileBeanBase(contentFile, contentName, contentType, contentSize));
		}
		model.addAttribute(MODEL_ENTITY, result.size() != 1 ? result : result.get(0));

		return jsonView;
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