package sample.application.house;

import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;

import sample.application.consumer.Consumer;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;


@Entity
public @Data @AllArgsConstructor @NoArgsConstructor class House {

	@EmbeddedId
	public HouseId id;
	public String name;

	@ManyToOne(fetch=FetchType.EAGER)
	public Consumer consumer;
}
