package sample.application.desk;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import com.u2ware.springfield.config.GenericMvc;

@GenericMvc(requestMappingRootPatternValue="/a/b/c")
@Entity
public @Data @AllArgsConstructor @NoArgsConstructor class Desk {

	@Id
	@GeneratedValue(strategy=GenerationType.IDENTITY)
	public int seq;
	
	public String password;


}
