package com.u2ware.springfield.domain;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;

public class EntityPageImpl<T> implements EntityPage<T> {

	public int number;
	public int size;
	public int totalPages;
	public int numberOfElements;
	public long totalElements;

	public boolean hasPreviousPage;
	public boolean isFirstPage;
	public boolean hasNextPage;
	public boolean isLastPage;
	public boolean hasContent;
	
	private int range = 10;
	private int beginPage = 0;
	private int endPage = 0;
	private int previousPage = -1;
	private int nextPage = -1;

	public List<T> content;
	public Sort sort;
	
	private int status;

	public EntityPageImpl() {
		this(new PageImpl<T>(new ArrayList<T>()));
	}
	public EntityPageImpl(List<T> content) {
		this(new PageImpl<T>(content));
	}
	public EntityPageImpl(List<T> content, Pageable pageable, long total) {
		this(new PageImpl<T>(content, pageable, total));
	}
	public EntityPageImpl(List<T> content, int pageNumber, int pageSize, Sort sort , long total) {
		this(new PageImpl<T>(content, new PageRequest(pageNumber, pageSize, sort), total));
	}
	public EntityPageImpl(List<T> content, Page<?> p) {
		this(new PageImpl<T>(content, new PageRequest(p.getNumber(), p.getSize(), p.getSort()) , p.getTotalElements()));
	}
	
	public EntityPageImpl(Page<T> page) {
		this.number = page.getNumber();
		this.size = page.getSize();
		this.totalPages = page.getTotalPages();
		this.numberOfElements = page.getNumberOfElements();
		this.totalElements = page.getTotalElements();
		this.content = page.getContent();
		
		this.sort = page.getSort();

		this.hasPreviousPage = page.hasPreviousPage();
		this.isFirstPage = page.isFirstPage();
		this.hasNextPage = page.hasNextPage();
		this.isLastPage = page.isLastPage();
		this.hasContent = page.hasContent();


		
		if(totalPages == 0) return;
		int currentPage = page.getNumber();
		int currentGroup = currentPage / range;
		
		this.previousPage = currentGroup > 0 ? (currentGroup - 1) * range : -1;
		this.beginPage = currentGroup * range;
		this.endPage = Math.min(totalPages, (currentGroup+1) * range) - 1;
		this.nextPage = endPage < totalPages-1 ? endPage + 1 : -1;
	}

	
	
	public int getNumber() {
		return number;
	}
	public int getSize() {
		return size;
	}
	public int getTotalPages() {
		return totalPages;
	}
	public int getNumberOfElements() {
		return numberOfElements;
	}
	public long getTotalElements() {
		return totalElements;
	}
	public List<T> getContent() {
		return content;
	}
	public Sort getSort() {
		return sort;
	}
	public boolean isHasPreviousPage() {
		return hasPreviousPage;
	}
	public boolean isFirstPage() {
		return isFirstPage;
	}
	public boolean isHasNextPage() {
		return hasNextPage;
	}
	public boolean isLastPage() {
		return isLastPage;
	}
	public boolean isHasContent() {
		return hasContent;
	}
	public int getBeginPage() {
		return beginPage;
	}
	public int getEndPage() {
		return endPage;
	}
	public int getPreviousPage() {
		return previousPage;
	}
	public int getNextPage() {
		return nextPage;
	}
	public boolean hasPreviousPage() {
		return hasPreviousPage;
	}
	public boolean hasNextPage() {
		return hasNextPage;
	}
	public boolean hasContent() {
		return hasContent;
	}
	public Iterator<T> iterator() {
		return content.iterator();
	}
	public int getStatus() {
		return status;
	}
	public void setStatus(int status) {
		this.status = status;
	}

	public int getPageNumber() {
		return number;
	}
	public int getPageSize() {
		return size;
	}

}
