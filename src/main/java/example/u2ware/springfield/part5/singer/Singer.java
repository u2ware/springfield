package example.u2ware.springfield.part5.singer;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;

@Entity
public class Singer {
	
	@Id @NotNull
	private @Getter @Setter Integer singerId;

	@NotNull
	private @Getter @Setter String name;

}
