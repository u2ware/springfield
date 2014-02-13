package example.u2ware.springfield.part5.code;

import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.validation.Valid;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;

@Entity
public class Code {

	@EmbeddedId @Valid
	private @Getter @Setter CodeId codeId;
	
	@NotNull
	private @Getter @Setter String name;
	
	
}
