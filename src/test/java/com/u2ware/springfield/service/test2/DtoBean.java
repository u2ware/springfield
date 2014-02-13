package com.u2ware.springfield.service.test2;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import com.u2ware.springfield.config.Springfield;
import com.u2ware.springfield.config.Springfield.Strategy;

@Springfield(strategy=Strategy.DTO, topLevelMapping="/service/test2",identity="param1")
public @AllArgsConstructor @NoArgsConstructor @ToString class DtoBean {

	private @Getter @Setter String param1;
	private @Getter @Setter Integer param2;
	
}
