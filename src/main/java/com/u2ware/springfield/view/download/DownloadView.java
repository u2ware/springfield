package com.u2ware.springfield.view.download;

import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.web.servlet.View;

import com.google.common.io.ByteStreams;
import com.u2ware.springfield.view.GenericView;

public class DownloadView implements View{

    protected Log logger = LogFactory.getLog(getClass());

    private GenericView genericView;

    public DownloadView(GenericView genericView){
    	this.genericView = genericView;
    }

	@Override
    public String getContentType() {
    	return "application/octet-stream";
    }
    
	@Override
	public void render(Map<String, ?> model, HttpServletRequest request, HttpServletResponse response) throws Exception {
		logger.debug(getClass());

		try{
			String uploadFilename = (String)model.get("responseDto");
			String location = (String)model.get("responseView");
			if(uploadFilename == null){
				throw new Exception("multipart url is not found.");
			}
			if(location == null){
				throw new Exception("Location is not found.");
			}

			Path uploadPath = Paths.get(System.getProperty("user.dir"), location, uploadFilename);
			if(! Files.exists(uploadPath)){
				throw new Exception("Path["+uploadPath.toString()+"] is not found.");
			}
			logger.debug("file system ");
			logger.debug("\t"+uploadPath.toAbsolutePath().toUri());
			
			
			InputStream input = Files.newInputStream(uploadPath);
			StringBuilder json = new StringBuilder();
			char value;
			while((value = (char) input.read()) != 0){
				json.append(value);
				if(value == '}'){
					break;
				}
			}
			
			Map<String,Object> multipartUpload = genericView.parse(json.toString());
			String filename = new String(multipartUpload.get("originalFilename").toString().getBytes("euc-kr") , "8859_1");
			Integer contentSize = (Integer)multipartUpload.get("size");
			String contentType = (String)multipartUpload.get("contentType");

			logger.debug("download ");
			logger.debug("\tcontentType: "+contentType);
			logger.debug("\toriginalFilename: "+filename);
			logger.debug("\tuploadFilename: "+uploadFilename);
			logger.debug("\tsize: "+contentSize);
			
			
			response.setHeader("Content-Disposition", "attachment; filename=\"" + filename + "\";");
			response.setHeader("Content-Transfer-Encoding", "binary");
			
			if(contentSize != null && contentSize > 0){
				response.setHeader("Content-Length", contentSize.toString());
				response.setContentLength(contentSize.intValue());
			}
			if(contentType != null){
				response.setContentType(contentType);
			}else{
				response.setContentType(getContentType());
			}
			
			ByteStreams.copy(input, response.getOutputStream());
		}catch(Exception e){
			e.printStackTrace();
			genericView.render(e, request, response);
		}
	}
}
