package example.u2ware.springfield.part5.manager;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;

import org.apache.commons.collections.FactoryUtils;
import org.apache.commons.collections.list.LazyList;

import com.u2ware.springfield.domain.SortOrder;

@Entity
public class Manager {

	@Id @NotNull
	private @Getter @Setter Integer managerId;

	@NotNull
	private @Getter @Setter String name;
	
	@OneToMany(fetch=FetchType.EAGER,cascade=CascadeType.ALL ,orphanRemoval=true,mappedBy="projectId.managerId")
	private @Getter @Setter @SuppressWarnings("unchecked") List<Project> projects = LazyList.decorate(new ArrayList<SortOrder>(100), FactoryUtils.instantiateFactory(Project.class));
	
}