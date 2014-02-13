package com.u2ware.springfield.service.test3;

import java.io.IOException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.support.TransactionTemplate;
import org.springframework.web.multipart.MultipartFile;

import com.u2ware.springfield.repository.EntityRepository;
import com.u2ware.springfield.service.AbstractEntityService;
import com.u2ware.springfield.support.multipart.MultipartFileHandler;

@Service("fileBeanService")
public class FileBeanService extends AbstractEntityService<FileBean,FileBean>{

	@Autowired @Qualifier("fileBeanRepository")
	private EntityRepository<FileBean, Integer> fileBeanRepository;

	@Autowired 
	private TransactionTemplate transactionTemplate;
	
	@Override
	protected EntityRepository<FileBean, Integer> getRepository() {
		return fileBeanRepository;
	}

	@Override
	protected TransactionTemplate getTransactionTemplate() {
		return transactionTemplate;
	}
	
	@Autowired 
	private MultipartFileHandler multipartFileHandler;
	
	@Override
	public FileBean create(FileBean entity) {
		
		MultipartFile f = entity.getUploadFile();
		try {
			String contentFile = multipartFileHandler.uploadFile(f);
			String contentName = f.getOriginalFilename();
			String contentType = f.getContentType();
			Long contentSize = f.getSize();
		
			entity.setContentFile(contentFile);
			entity.setContentName(contentName);
			entity.setContentType(contentType);
			entity.setContentSize(contentSize);
			
			return super.create(entity);

		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}
}