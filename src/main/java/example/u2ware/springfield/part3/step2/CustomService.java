package example.u2ware.springfield.part3.step2;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.u2ware.springfield.domain.EntityPageable;
import com.u2ware.springfield.repository.EntityRepository;
import com.u2ware.springfield.service.EntityServiceImpl;

import example.u2ware.springfield.part1.step2.JpaBean;

@Service
public class CustomService extends EntityServiceImpl<JpaBean, Custom>{

	@Autowired
	public CustomService(
		@Qualifier("jpaBeanRepository")EntityRepository<JpaBean, Integer> r) {
		super("jpaBeanRepository", r);
	}

	@Override
	@Transactional
	public Iterable<JpaBean> findForm(Custom request, EntityPageable pageable) {
		logger.debug("Overide findForm ");
		logger.debug("Overide findForm ");
		logger.debug("Overide findForm ");
		logger.debug("Overide findForm ");
		logger.debug("Overide findForm ");
		return super.findForm(request, pageable);
	}
}
