package com.u2ware.springfield.domain;

import org.springframework.data.domain.Page;


public interface EntityPage<T> extends Page<T>{

	public int getStatus();
	
	public int getPageNumber();
	public int getPageSize();

	public int getPreviousPage();
	public int getBeginPage();
	public int getEndPage();
	public int getNextPage();
}