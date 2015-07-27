package sample.application.phonebook;

import javax.persistence.EmbeddedId;
import javax.persistence.Entity;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

//@GenericMvc.Repository
@Entity
public @Data @AllArgsConstructor @NoArgsConstructor class Phone {

	@EmbeddedId
	private PhoneId phoneId;

	private String name;

}
