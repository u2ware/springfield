package com.u2ware.springfield.support.multipart;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

import org.apache.commons.io.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.StringUtils;
import org.springframework.web.multipart.MultipartFile;

public class MultipartFileHandlerImpl implements MultipartFileHandler {
	
	protected final Logger logger = LoggerFactory.getLogger(getClass());

	private File directory;

	public void setDirectory(File directory) {
		this.directory = directory;
	}

	/////////////////////////////////
	//
	////////////////////////////////
	private UploadFileNameResolver defaultFilenameResolver = new UploadFileNameResolver(){
		public String resolveFileName(MultipartFile multipartFile) throws IOException {
			return System.currentTimeMillis()+"_"+multipartFile.getOriginalFilename();
		}
	};
	
	@Override
	public String uploadFile(MultipartFile multipartFile) throws IOException {
		return uploadFile(multipartFile, defaultFilenameResolver);
	}

	@Override
	public String uploadFile(MultipartFile multipartFile, UploadFileNameResolver resolver) throws IOException {


		String contentFile = resolver.resolveFileName(multipartFile);

		logger.warn("MultipartFile : "+multipartFile);
		logger.warn("MultipartFile Name: "+multipartFile.getName());
		logger.warn("MultipartFile Size : "+multipartFile.getSize());
		logger.warn("MultipartFile ContentType: "+multipartFile.getContentType());
		logger.warn("MultipartFile OriginalFilename: "+multipartFile.getOriginalFilename());
		logger.warn("Upload ContentFile : "+contentFile);
		File dest = findFile(contentFile);

		if(dest.exists()){
			logger.warn("Upload Rewrite : "+dest.getAbsolutePath());
			IOUtils.copyLarge(multipartFile.getInputStream(), new FileOutputStream(dest, false));

		}else{
			dest.getParentFile().mkdirs();
			if(dest.createNewFile()){
				logger.warn("Upload Make : "+dest.getAbsolutePath());
				multipartFile.transferTo(dest);
			}else{
				throw new IOException("cann't create file");
			}
		}
		return contentFile;
	}
	
	@Override
	public File findFile(String contentFile) throws IOException {
		File file = new File(directory, contentFile);
		return file;
	}
	
	@Override
	public void deleteFile(String contentFile) throws IOException {
		
		if(contentFile.startsWith("/")){
			String[] paths = StringUtils.delimitedListToStringArray(contentFile, "/");

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
					logger.warn("Deleted File : "+file);
				}
			}
		}else{
			File file = findFile(contentFile);
			if(file.delete()){
				logger.warn("Deleted File : "+file);
			}
		}
	}

}
