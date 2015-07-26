package com.u2ware.springfield.view.upload;

import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.http.MediaType;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.servlet.View;

import com.google.common.io.ByteSource;
import com.u2ware.springfield.view.GenericView;

public class UploadView implements View{

    protected Log logger = LogFactory.getLog(getClass());
    
    private GenericView genericView;

    public UploadView(GenericView genericView){
    	this.genericView = genericView;
    }
	
	public String getContentType() {
		return MediaType.MULTIPART_FORM_DATA_VALUE;
	}

	@Override
	public void render(Map<String, ?> model, HttpServletRequest request, HttpServletResponse response) throws Exception {
		logger.debug(getClass());

	   	try{
			MultipartFile multipartFile = (MultipartFile)model.get("responseDto");
			String location = (String)model.get("responseView");
			if(multipartFile == null){
				throw new Exception("MultipartFile is null.");
			}
			if(location == null){
				throw new Exception("Location is not found.");
			}
			
			String uploadFilename = new Long(System.currentTimeMillis()).toString();
			String originalFilename = multipartFile.getOriginalFilename();
			String contentType = multipartFile.getContentType();
			Long size = multipartFile.getSize();

			logger.debug("upload ");
			logger.debug("\tcontentType: "+multipartFile.getContentType());
			logger.debug("\toriginalFilename: "+multipartFile.getName());
			logger.debug("\tuploadFilename: "+uploadFilename);
			logger.debug("\tsize: "+multipartFile.getSize());
			logger.debug("\tlocation: "+location);
			
			
			//////////
			Path uploadPath = Paths.get(System.getProperty("user.dir"), location, uploadFilename);
			logger.debug("uploadPath: "+uploadPath.toFile().getAbsolutePath());

			Path uploadDir = uploadPath.getParent();
			if (!Files.exists(uploadDir)){
			    Files.createDirectories(uploadDir);					
			}

			Map<String,Object> value = new HashMap<String,Object>();
			value.put("uploadFilename", uploadFilename);
			value.put("originalFilename", originalFilename);
			value.put("contentType", contentType);
			value.put("size", size);
			
			
			String json = genericView.format(value);
			ByteSource metadata = ByteSource.wrap(json.getBytes());
			//ByteSource line = ByteSource.wrap(System.getProperty("line.separator").getBytes());
			ByteSource data = ByteSource.wrap(multipartFile.getBytes());
			
			ByteSource source = ByteSource.concat(metadata, data);
			OutputStream uploadOutputStream = Files.newOutputStream(uploadPath);
			source.copyTo(uploadOutputStream);
			
			logger.debug("file system ");
			logger.debug("\t"+uploadPath.toAbsolutePath().toUri());

			genericView.render(value, request, response);

		}catch(Exception e){
			e.printStackTrace();
			genericView.render(e, request, response);
		}
	}
	
	
}
