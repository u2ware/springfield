package com.u2ware.springfield.support.multipart;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

import org.apache.commons.io.IOUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.util.StringUtils;
import org.springframework.web.multipart.MultipartFile;

public class MultipartFileHandlerImpl implements MultipartFileHandler {
	
	protected final Log logger = LogFactory.getLog(getClass());

	private String location;

	public void setLocation(String location) {
		this.location = location;
	}
	
	/////////////////////////////////
	//
	////////////////////////////////
	private ContentFilePolicy defaultPolicy = new ContentFilePolicy(){
		public String getContentFile(MultipartFile multipartFile, String nameKeyword) {
			if(StringUtils.hasText(nameKeyword)){
				return nameKeyword+"_"+System.currentTimeMillis();
			}else{
				return ""+System.currentTimeMillis();
			}
		}
	};
	
	@Override
	public String saveFile(MultipartFile multipartFile) throws IOException {
		return saveFile(multipartFile, null, defaultPolicy);
	}
	@Override
	public String saveFile(MultipartFile multipartFile, String name) throws IOException {
		return saveFile(multipartFile, name, defaultPolicy);
	}
	@Override
	public String saveFile(MultipartFile multipartFile, String name, ContentFilePolicy policy) throws IOException {

		logger.info("MultipartFile : "+multipartFile);
		logger.info("MultipartFile Name: "+multipartFile.getName());
		logger.info("MultipartFile Size : "+multipartFile.getSize());
		logger.info("MultipartFile ContentType: "+multipartFile.getContentType());
		logger.info("MultipartFile OriginalFilename: "+multipartFile.getOriginalFilename());

		String contentFile = policy.getContentFile(multipartFile, name);
		logger.info("contentFile : "+contentFile);
		File dest = findFile(contentFile);

		if(dest.exists()){
			IOUtils.copyLarge(multipartFile.getInputStream(), new FileOutputStream(dest, false));

		}else{
			dest.getParentFile().mkdirs();
			if(dest.createNewFile()){
				logger.info("Saved File : "+dest.getAbsolutePath());
				multipartFile.transferTo(dest);
			}else{
				throw new IOException("cann't create file");
			}
		}
		return contentFile;
	}
	
	@Override
	public File findFile(String contentFile) throws IOException {
		logger.info("contentFile : "+contentFile);
		File file = new File(getBaseDir(), contentFile);
		logger.info("Find File : "+file);
		return file;
	}
	
	@Override
	public void deleteFile(String contentFile) throws IOException {
		
		if(contentFile.startsWith("/")){
			String[] paths = StringUtils.delimitedListToStringArray(contentFile, "/");
			for(int i = 0 ; i < paths.length; i++){
				logger.info(paths[i]);
			}
			for(int i = 0 ; i < paths.length - 1; i++){
				
				StringBuilder buf = new StringBuilder();
				for(int c = 0 ; c < (paths.length - i); c++){
					if(StringUtils.hasText(paths[c])){
						buf.append("/").append(paths[c]);
					}
				}

				String key = buf.toString();
				File file = findFile(key);
				if(file.delete()){
					logger.info("Deleted File : "+file);
				}
			}
		}else{
			File file = findFile(contentFile);
			if(file.delete()){
				logger.info("Deleted File : "+file);
			}
		}
	}

	/////////////////////////////////
	//
	////////////////////////////////
	private File dir;
	private File getBaseDir() {
		if(dir != null) return dir;
		try{
			dir = new File(location);
			if(dir.exists()){
				return dir;
			}
		}catch(Exception e){
			
		}
		try{
			dir = new File(System.getProperty("user.dir") , "uploadBySpringfield");
			if(dir.exists()){
				return dir;
			}else{
				boolean mkdir = dir.mkdir();
				if(mkdir){
					return dir;
				}
			}
		}catch(Exception e){
			
		}
		throw new RuntimeException("upload location is not good.");
	}
}
