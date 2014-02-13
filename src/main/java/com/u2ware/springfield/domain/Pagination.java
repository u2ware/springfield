package com.u2ware.springfield.domain;

import java.util.ArrayList;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;

public class Pagination<T> extends PageImpl<T>{

	private static final long serialVersionUID = -1602106183136879815L;

	public Pagination() {
		super(new ArrayList<T>());
	}
	public Pagination(List<T> content) {
		super(content);
	}
	public Pagination(List<T> content, Pageable pageable, long total) {
		super(content, pageable, total);
	}
	public Pagination(Page<T> page, Pageable pageable) {
		super(page.getContent(), pageable, page.getTotalElements());
	}

	public int getCurrentIndex(){
		return getNumber() + 1;
	}

	public int getBeginIndex(){
		return Math.max(1,  getCurrentIndex() - 5);
	}
	
	public int getEndIndex(){
		return Math.max(1,  Math.min(getBeginIndex() + 10, getTotalPages()));
	}
}
