package example.u2ware.springfield.part5.code;

import java.io.Serializable;

import javax.persistence.Embeddable;
import javax.validation.constraints.NotNull;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Embeddable
public @EqualsAndHashCode @ToString @SuppressWarnings("serial") class CodeId implements Serializable{

	@NotNull
	private @Getter @Setter Integer seq;

	@NotNull
	private @Getter @Setter Integer index;

}
