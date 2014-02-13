package com.u2ware.springfield.support.multipart;

import java.io.File;

import org.springframework.web.multipart.MultipartFile;

import com.u2ware.springfield.domain.ValidationRejectableException;

public class UploadFileRepositoryImpl implements UploadFileRepository{

	private MultipartFileHandler multipartFileHandler;

	public void setMultipartFileHandler(MultipartFileHandler multipartFileHandler) {
		this.multipartFileHandler = multipartFileHandler;
	}
	public MultipartFileHandler getMultipartFileHandler() {
		return multipartFileHandler;
	}

	public void saveFile(UploadFile bean) throws ValidationRejectableException {

		MultipartFile multipartFile = bean.getMultipartFile(); 
		File file = getMultipartFileHandler().saveFile(multipartFile);

		bean.setContentFile(file.getName());
		bean.setContentName(multipartFile.getOriginalFilename());
		bean.setContentType(multipartFile.getContentType());
		bean.setContentSize(multipartFile.getSize());
	}

	public void deleteFile(UploadFile bean) throws ValidationRejectableException {
		getMultipartFileHandler().deleteFile(bean.getContentFile());
	}
}
