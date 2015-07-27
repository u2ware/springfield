package sample.application.house;

import java.io.Serializable;

import javax.persistence.Embeddable;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@SuppressWarnings("serial")
@Data @AllArgsConstructor @NoArgsConstructor
@Embeddable
public class HouseId implements Serializable{

	
	public String code;

	public Integer seq;
	
	
}
