package example.u2ware.springfield.part5.manager;

import java.io.Serializable;

import javax.persistence.Embeddable;
import javax.validation.constraints.NotNull;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Embeddable
public @EqualsAndHashCode @ToString @SuppressWarnings("serial") class ProjectId implements Serializable {

	@NotNull
	private @Getter @Setter Integer managerId;

	@NotNull
	private @Getter @Setter String code;
	
}
