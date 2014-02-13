package example.u2ware.springfield.part3.step3;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.u2ware.springfield.domain.EntityPageImpl;
import com.u2ware.springfield.domain.EntityPageable;
import com.u2ware.springfield.repository.EntityRepository;
import com.u2ware.springfield.service.EntityService;

import example.u2ware.springfield.part1.step2.JpaBean;

@Service
@Transactional
public class FormService implements EntityService<Form, Form>{

	protected final Log logger = LogFactory.getLog(getClass());

	@Autowired 
	@Qualifier("jpaBeanRepository")
	protected EntityRepository<JpaBean, Integer> jpaBeanRepository;

	public Form home(Form query) {
		return query;
	}

	public Iterable<Form> findForm(Form query, EntityPageable pageable) {
		return find(query, pageable);
	}

	public Iterable<Form> find(Form query, EntityPageable pageable) {
		
		if(pageable != null && pageable.isEnable()){
			Page<JpaBean> r = jpaBeanRepository.findAll(query, pageable);
			
			List<Form> content = this.convert(r.getContent());
			long total = r.getTotalElements();
			
			return new EntityPageImpl<Form>(content, pageable, total);
		}else{
			List<JpaBean> r = jpaBeanRepository.findAll(query);
			
			List<Form> content = this.convert(r);
			return content;
		}
	}
	
	public Form read(Form entity) {
		JpaBean target = convert(entity);
		target = jpaBeanRepository.read(target);
		return convert(target);
	}

	public Form createForm(Form entity) {
		return entity;
	}

	public Form create(Form entity) {
		JpaBean target = convert(entity);
		target = jpaBeanRepository.create(target);
		return convert(target);
	}

	public Form updateForm(Form entity) {
		return read(entity);
	}

	public Form update(Form entity) {
		JpaBean target = convert(entity);
		target = jpaBeanRepository.update(target);
		return convert(target);
	}

	public Form delete(Form entity) {
		JpaBean target = convert(entity);
		jpaBeanRepository.delete(target);
		return entity;
	}
	
	private List<Form> convert(List<JpaBean> targets) {
		List<Form> content = new ArrayList<Form>();
		for(JpaBean target : targets){
			Form form = convert(target);
			content.add(form);
		}
		return content;
	}	
	private Form convert(JpaBean target){
		Form entity = new Form();
		entity.setId(target.getId());
		entity.setPassword(target.getPassword());
		entity.setName(target.getName());
		entity.setAddress(target.getAddress());
		return entity;
	}
	private JpaBean convert(Form entity){
		JpaBean target = new JpaBean();
		target.setId(entity.getId());
		target.setPassword(entity.getPassword());
		target.setName(entity.getName());
		target.setAddress(entity.getAddress());
		return target;
	}

	
}