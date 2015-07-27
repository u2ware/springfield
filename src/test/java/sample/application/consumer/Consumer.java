package sample.application.consumer;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.validation.constraints.NotNull;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import com.u2ware.springfield.config.GenericMvc;
import com.u2ware.springfield.service.ValidationGroup;

@GenericMvc
@Entity
public @Data @AllArgsConstructor @NoArgsConstructor class Consumer {

	@Id
	@NotNull(groups=ValidationGroup.Read.class)
	public String id;
	
	@NotNull(groups=ValidationGroup.Edit.class)
	public String password;

}
