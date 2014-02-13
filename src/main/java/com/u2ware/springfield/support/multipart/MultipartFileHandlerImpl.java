package com.u2ware.springfield.support.multipart;

import java.io.File;
import java.io.IOException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.web.multipart.MultipartFile;

import com.u2ware.springfield.domain.ValidationRejectableException;

public class MultipartFileHandlerImpl implements MultipartFileHandler {
	
	protected final Log logger = LogFactory.getLog(getClass());

	private String location;
	
	public void setLocation(String location) {
		this.location = location;
	}

	public File saveFile(MultipartFile multipartFile) {

		logger.info("MultipartFile : "+location);
		logger.info("MultipartFile : "+multipartFile);
		logger.info("MultipartFile Name: "+multipartFile.getName());
		logger.info("MultipartFile Size : "+multipartFile.getSize());
		logger.info("MultipartFile ContentType: "+multipartFile.getContentType());
		logger.info("MultipartFile OriginalFilename: "+multipartFile.getOriginalFilename());

		try{
			StringBuilder path = new StringBuilder();
			path.append(System.currentTimeMillis());
			File dest = new File(location, path.toString());

			if(dest.createNewFile()){
				logger.info("Saved File : "+dest.getAbsolutePath());
				multipartFile.transferTo(dest);
				return dest;
			}else{
				throw new IOException(".....");
			}

		}catch(IOException e){
			e.printStackTrace();
			throw new ValidationRejectableException("multipartFile", "com.u2ware.springfield.repository.multipart.UploadIOError.message");
		}
	}
	
	public void deleteFile(String contentFile)  {
		logger.info("MultipartFile : "+contentFile);
		File file = new File(location, contentFile);
		
		if(file.exists()){
			file.delete();
			logger.info("Deleted File : "+file.getAbsolutePath());
		}
	}

	public File findFile(String contentFile)  {
		File file = new File(location, contentFile);
		return file;
	}
	
	


}


