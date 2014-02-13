package com.u2ware.springfield.controller;

import java.util.ArrayList;
import java.util.List;

public class HandlerMappingNavigation {

	private String title;
	private String path;
	private String link;
	private List<HandlerMappingNavigation> childs;

	public void setTitle(String title) {
		this.title = title;
	}

	public void setPath(String path) {
		this.path = path;
	}

	public void setLink(String link) {
		this.link = link;
	}

	public String getTitle() {
		return title;
	}
	public String getPath() {
		return path;
	}

	public String getLink() {
		if(childs != null && childs.size() > 0){
			return childs.get(0).getLink();
		}
		return link;
	}
	
	public List<HandlerMappingNavigation> getChilds() {
		if(childs == null){
			return new ArrayList<HandlerMappingNavigation>();
		}
		return childs;
	}
	protected HandlerMappingNavigation addChild(HandlerMappingNavigation child){
		if(childs == null) 
			childs = new ArrayList<HandlerMappingNavigation>();
		if(childs.contains(child)){
			return childs.get(childs.indexOf(child));
		}else{
			childs.add(child);
			return child;
		}
	}
	
	@Override
	public String toString() {
		return "Navigation [title=" + title + ", path=" + path + ", link="
				+ getLink() + "]";
	}
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((path == null) ? 0 : path.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		HandlerMappingNavigation other = (HandlerMappingNavigation) obj;
		if (path == null) {
			if (other.path != null)
				return false;
		} else if (!path.equals(other.path))
			return false;
		return true;
	}


}