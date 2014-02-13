package com.u2ware.springfield.domain;

import java.util.ArrayList;
import java.util.List;

import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.data.domain.Sort.Order;

public class PaginationRequest extends PageRequest{

	private static final long serialVersionUID = -6958874691326349721L;

	public PaginationRequest(int page, int size) {
		super(page, size);
	}

	public PaginationRequest(int page, int size, Direction direction, String... properties) {
		super(page, size, direction, properties);
	}

	public PaginationRequest(int page, int size, Sort sort) {
		super(page, size, sort);
	}

	public PaginationRequest(int page, int size, String... sortSource) {
		super(page, size, PaginationRequest.parseParameterIntoSort(sortSource));
	}
	
	private static Sort parseParameterIntoSort(String[] source) {
		if(source == null) return null;
		
		List<Order> allOrders = new ArrayList<Sort.Order>();
		for (String part : source) {
			if (part == null) {
				continue;
			}
			String[] elements = part.split(",");
			Direction direction = Direction.fromStringOrNull(elements[elements.length - 1]);
			for (int i = 0; i < elements.length; i++) {
				if (i == elements.length - 1 && direction != null) {
					continue;
				}
				allOrders.add(new Order(direction, elements[i]));
			}
		}
		return allOrders.isEmpty() ? null : new Sort(allOrders);
	}
}
