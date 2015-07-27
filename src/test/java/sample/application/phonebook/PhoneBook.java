package sample.application.phonebook;

import java.util.Collection;
import java.util.HashSet;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.OneToMany;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import com.u2ware.springfield.config.GenericMvc;


@GenericMvc
@Entity
public @Data @AllArgsConstructor @NoArgsConstructor class PhoneBook {

	@Id
	public String id;
	public String password;

	@OneToMany(mappedBy="phoneId.id", cascade=CascadeType.ALL, fetch=FetchType.EAGER)
	private Collection<Phone> phone = new HashSet<Phone>();
	
}
