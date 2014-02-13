package example.u2ware.springfield.part4.step2;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.u2ware.springfield.repository.EntityRepository;
import com.u2ware.springfield.service.EntityServiceImpl;
import com.u2ware.springfield.support.context.ContextBroker;

@Service
public class GetterBeanService extends EntityServiceImpl<GetterBean, SetterBean>{

	@Autowired @Qualifier("sessionContextBroker")
	private ContextBroker sessionContextBroker;	
	
	@Autowired
	public GetterBeanService(
		@Qualifier("getterBeanRepository")EntityRepository<GetterBean, ?> r) {
		super("getterBeanRepository", r);
	}

	@Override
	@Transactional
	public GetterBean createForm(GetterBean entity) {
		SetterBean setterBean = sessionContextBroker.get(SetterBean.class);
		logger.debug(sessionContextBroker);
		logger.debug(setterBean);
		entity.setCode(setterBean.getCode());
		entity.setName(setterBean.getName());
		
		return super.createForm(entity);
	}
	
}
