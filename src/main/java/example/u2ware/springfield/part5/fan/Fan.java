package example.u2ware.springfield.part5.fan;

import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import javax.validation.Valid;
import javax.validation.constraints.NotNull;

import example.u2ware.springfield.part5.singer.Singer;



import lombok.Getter;
import lombok.Setter;


@Entity
public class Fan{

	@Id @NotNull 
	private @Getter @Setter Integer fanId;

	@NotNull 
	private @Getter @Setter String name;
	
	@ManyToOne(fetch=FetchType.EAGER) @Valid
	private @Getter @Setter Singer singer;	
	
}
