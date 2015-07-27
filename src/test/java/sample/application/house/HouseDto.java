package sample.application.house;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import com.u2ware.springfield.config.GenericMvc;

@GenericMvc(value=House.class, requestMappingUniquePatternValue="{idCode}|{idSeq}")
public @Data @AllArgsConstructor @NoArgsConstructor class HouseDto {

	public String idCode;
	public Integer idSeq;
	public String password;
	
	public String consumerId;
	public String consumerPassword;
}
