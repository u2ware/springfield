package com.u2ware.springfield.sample.others.multipart;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.multipart.MultipartFile;

import com.u2ware.springfield.support.multipart.MultipartFileController;
import com.u2ware.springfield.support.multipart.MultipartFileHandler;

@Controller
@RequestMapping("/others/multipart")
public class AjaxUploadController extends MultipartFileController{
	
	@Autowired 
	private MultipartFileHandler multipartFileHandler;
	
	public MultipartFileHandler getMultipartFileHandler() {
		return multipartFileHandler;
	}

	@Override
	public String getContentFile(MultipartFile multipartFile, String contentFile) {
		return ""+System.currentTimeMillis();
	}

	@RequestMapping(value = "/upload", method = RequestMethod.GET)
	public String uploadForm() throws Exception {
		return "/others/multipart/uploadForm";
	}

}