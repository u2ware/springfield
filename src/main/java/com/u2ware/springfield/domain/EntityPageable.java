package com.u2ware.springfield.domain;

import java.util.List;

import org.springframework.data.domain.Pageable;

public interface EntityPageable extends Pageable{

	public List<SortOrder> getSortOrders();

	public boolean isEnable();
}
