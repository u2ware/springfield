package com.u2ware.springfield.domain;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.collections.FactoryUtils;
import org.apache.commons.collections.list.LazyList;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Order;

public final class EntityPageRequest implements EntityPageable{

	private boolean enable = true;
	private int pageNumber = 0;
	private int pageSize = 10;

	@SuppressWarnings("unchecked")
	private List<SortOrder> sortOrders = LazyList.decorate(new ArrayList<SortOrder>(100), FactoryUtils.instantiateFactory(SortOrder.class));
	
	public EntityPageRequest() {
	}
	public EntityPageRequest(boolean enable) {
		this.enable = enable;
	}
	public EntityPageRequest(int pageNumber, int pageSize) {
		this.pageNumber = pageNumber;
		this.pageSize = pageSize;
	}


	public boolean isEnable() {
		return enable;
	}
	public void setEnable(boolean enable) {
		this.enable = enable;
	}
	public int getOffset(){
		return pageNumber*pageSize;
	}
	public int getPageNumber() {
		return pageNumber;
	}
	public void setPageNumber(int pageNumber) {
		this.pageNumber = pageNumber;
	}
	public int getPageSize() {
		return pageSize;
	}
	public void setPageSize(int pageSize) {
		this.pageSize = pageSize;
	}
	public List<SortOrder> getSortOrders() {
		return sortOrders;
	}
	public void setSortOrders(List<SortOrder> sortOrders) {
		this.sortOrders = sortOrders;
	}
	
	
	public void addSortOrder(String property){
		sortOrders.add(new SortOrder(property));
	}
	public void addSortOrder(String property , int direction){
		sortOrders.add(new SortOrder(property, direction));
	}
	public void removeSortOrder(String property , int direction){
	}
	
	
	public Sort getSort() {
		if(sortOrders == null || sortOrders.size() == 0) 
			return null;
		
		List<Order> orderList = new ArrayList<Order>();
		for(SortOrder sortOrder : sortOrders){
			Order order = sortOrder.toOrder();
			if(order != null){
				orderList.add(order);
			}
		}
		if(orderList.size() == 0){
			return null;
		}

		return new Sort(orderList);
	}
	@Override
	public String toString() {
		return "EntityPageRequest [pageNumber=" + pageNumber + ", pageSize="
				+ pageSize + ", enable=" + enable + ", sortOrders="
				+ sortOrders + "]";
	}
	
	
}