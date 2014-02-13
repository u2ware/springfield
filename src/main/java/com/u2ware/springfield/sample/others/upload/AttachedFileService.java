package com.u2ware.springfield.sample.others.upload;

import java.io.IOException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.u2ware.springfield.repository.EntityRepository;
import com.u2ware.springfield.service.EntityServiceImpl;
import com.u2ware.springfield.support.multipart.MultipartFileHandler;
import com.u2ware.springfield.validation.RejectableException;


@Service
public class AttachedFileService extends EntityServiceImpl<AttachedFile, Integer>{

	@Autowired 
	private MultipartFileHandler multipartFileHandler;

	@Autowired @Qualifier("attachedFileRepository") 
	private EntityRepository<AttachedFile, Integer> attachedFileRepository;
	
	@Override
	protected EntityRepository<AttachedFile, Integer> getRepository() {
		return attachedFileRepository;
	}

	////////////////////////////////////
	//
	////////////////////////////////////
	@Override
	@Transactional
	public AttachedFile create(AttachedFile entity) {
		try {
			MultipartFile multipartFile = entity.getMultipartFile(); 
			String contentFile = multipartFileHandler.uploadFile(multipartFile);
			
			entity.setContentFile(contentFile);
			entity.setContentName(multipartFile.getOriginalFilename());
			entity.setContentType(multipartFile.getContentType());
			entity.setContentSize(multipartFile.getSize());
		} catch (IOException e) {
			throw new RejectableException("multipartFile" , "id");
		}
		AttachedFile newEntity = getRepository().create(entity);
		logger.debug(newEntity.getId().toString());
		
		return newEntity;
	}
	
	@Override
	@Transactional
	public AttachedFile delete(AttachedFile entity) {
		AttachedFile newEntity = getRepository().read(entity);
		try {
			multipartFileHandler.deleteFile(newEntity.getContentFile());
		} catch (IOException e) {
			throw new RejectableException("multipartFile" , "id");
		}
		getRepository().delete(newEntity);
		return entity;
	}
}
