package sample.application.phonebook;

import java.io.Serializable;

import javax.persistence.Embeddable;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;


@SuppressWarnings("serial")
@Embeddable
public @Data @AllArgsConstructor @NoArgsConstructor class PhoneId implements Serializable{

	private String id;

	@GeneratedValue(strategy=GenerationType.AUTO)
	private int seq;
	
	public PhoneId(String id){this.id = id;}
}
