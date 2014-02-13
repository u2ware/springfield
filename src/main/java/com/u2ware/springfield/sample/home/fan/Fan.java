package com.u2ware.springfield.sample.home.fan;

import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import javax.validation.Valid;
import javax.validation.constraints.NotNull;

import com.u2ware.springfield.sample.home.singer.Singer;




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
