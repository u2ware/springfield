package com.u2ware.springfield.sample.others.multipart;

import java.io.IOException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.multipart.MultipartFile;

import com.u2ware.springfield.support.multipart.MultipartFileHandler;
import com.u2ware.springfield.support.multipart.UploadFileNameResolver;
import com.u2ware.springfield.view.multipart.MultipartFileBeanController;

@Controller
@RequestMapping("/others/multipart")
public class AjaxUploadController extends MultipartFileBeanController{
	
	@Autowired 
	private MultipartFileHandler multipartFileHandler;
	
	public MultipartFileHandler getMultipartFileHandler() {
		return multipartFileHandler;
	}
	
	public UploadFileNameResolver getMultipartFileNameResolver(final String name) {
		return new UploadFileNameResolver(){
			public String resolveFileName(MultipartFile multipartFile) throws IOException {
				return ""+System.currentTimeMillis();
			}};
	}

	@RequestMapping(value = "/upload", method = RequestMethod.GET)
	public String uploadForm() throws Exception {
		return "/others/multipart/uploadForm";
	}


}