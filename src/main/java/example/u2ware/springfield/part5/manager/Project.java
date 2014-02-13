package example.u2ware.springfield.part5.manager;

import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.validation.Valid;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;

@Entity
public class Project {

	@EmbeddedId @Valid
	private @Getter @Setter ProjectId projectId;

	@NotNull
	private @Getter @Setter String description;
	
}
