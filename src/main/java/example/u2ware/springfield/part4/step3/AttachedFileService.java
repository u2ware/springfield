package example.u2ware.springfield.part4.step3;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.u2ware.springfield.repository.EntityRepository;
import com.u2ware.springfield.service.EntityServiceImpl;
import com.u2ware.springfield.support.multipart.UploadFileRepository;


@Service
public class AttachedFileService extends EntityServiceImpl<AttachedFile, Integer>{

	protected final Log logger = LogFactory.getLog(getClass());

	@Autowired 
	protected UploadFileRepository uploadFileRepository;

	@Autowired 
	public AttachedFileService(
		@Qualifier("attachedFileRepository") EntityRepository<AttachedFile, ?> r) {
		super("attachedFileRepository", r);
	}

	@Transactional
	public AttachedFile create(AttachedFile entity) {
		uploadFileRepository.saveFile(entity);
		return super.create(entity);
	}
	
	@Transactional
	public AttachedFile delete(AttachedFile entity) {
		AttachedFile newEntity = super.read(entity);
		uploadFileRepository.deleteFile(newEntity);
		return super.delete(newEntity);
	}
}
