package com.u2ware.springfield.domain;

import java.io.Serializable;

import org.springframework.data.domain.Sort.Direction;
import org.springframework.data.domain.Sort.Order;

public class SortOrder implements Serializable{

	/**
	 * 
	 */
	private static final long serialVersionUID = 8935712342108801252L;
	
	private int direction = 0;
	private String property;
	
	public SortOrder(){
	}
	public SortOrder(String property){
		this.property = property;
	}
	public SortOrder( String property, int direction) {
		this.property = property;
		this.direction = direction;
	}
	public String getProperty() {
		return property;
	}
	public void setProperty(String property) {
		this.property = property;
	}
	public int getDirection() {
		return direction;
	}
	public void setDirection(int direction) {
		this.direction = direction;
	}
	public int getNextDirection() {
		switch(direction){
			case  0 : return 1;
			case  1 : return -1;
			case -1 : return 0;
			default : return 1;
		}
	}
	public Order toOrder(){
		switch(direction){
			case  1 : return new Order(Direction.ASC, property);
			case  -1 : return new Order(Direction.DESC, property);
			default : return null;
		}
	}

	public String toString() {
		return "SortOrder [direction=" + direction + ", property=" + property
				+ "]";
	}
}