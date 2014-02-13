package example.u2ware.springfield.part4.step2;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.u2ware.springfield.repository.EntityRepository;
import com.u2ware.springfield.service.EntityServiceImpl;
import com.u2ware.springfield.support.context.ContextBroker;

@Service
public class SetterBeanService extends EntityServiceImpl<SetterBean, SetterBean>{

	@Autowired @Qualifier("sessionContextBroker")
	private ContextBroker sessionContextBroker;
	
	@Autowired
	public SetterBeanService(
		@Qualifier("setterBeanRepository")EntityRepository<SetterBean, ?> r) {
		super("setterBeanRepository" , r);
	}
	
	@Override
	@Transactional
	public SetterBean create(SetterBean entity) {
		sessionContextBroker.put(entity);
		logger.debug(sessionContextBroker);
		logger.debug(entity);
		return super.create(entity);
	}
}
