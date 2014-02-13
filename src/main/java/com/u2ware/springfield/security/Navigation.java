package com.u2ware.springfield.security;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import org.springframework.util.CollectionUtils;

import com.thoughtworks.xstream.annotations.XStreamAlias;
import com.thoughtworks.xstream.annotations.XStreamAsAttribute;
import com.thoughtworks.xstream.annotations.XStreamImplicit;

@SuppressWarnings("serial")
@XStreamAlias("navigation") 
public class Navigation implements Serializable{

	public final static String OBJECT_NAME = "navigation";
	
	@XStreamAsAttribute private boolean selected ;
	@XStreamAsAttribute private boolean hide;

	@XStreamAsAttribute private String pattern;
	@XStreamAsAttribute private String method;
	@XStreamAsAttribute private String access;
	@XStreamAsAttribute private boolean accessible;
	@XStreamAsAttribute private String path;
	@XStreamAsAttribute private String name;
	@XStreamImplicit private List<Navigation> children = new ArrayList<Navigation>();
	
	public boolean isSelected() {
		return selected;
	}
	public void setSelected(boolean selected) {
		this.selected = selected;
	}
	public boolean isHide() {
		return hide;
	}
	public void setHide(boolean hide) {
		this.hide = hide;
	}
	public String getPattern() {
		return pattern;
	}
	public void setPattern(String pattern) {
		this.pattern = pattern;
	}
	public String getMethod() {
		return method;
	}
	public void setMethod(String method) {
		this.method = method;
	}
	public String getAccess() {
		return access;
	}
	public void setAccess(String access) {
		this.access = access;
	}
	public boolean isAccessible() {
		return accessible;
	}
	public void setAccessible(boolean accessible) {
		this.accessible = accessible;
	}
	public String getPath() {
		return path;
	}
	public void setPath(String path) {
		this.path = path;
	}
	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}
	public List<Navigation> getChildren() {
		return children;
	}
	public void setChildren(List<Navigation> children) {
		this.children = children;
	}
	public Navigation addChild(Navigation child) {
		this.children.add(child);
		return child;
	}

	public void travel(NavigationVisitor visitor){
		travel(0, visitor, this);
	}

	private void travel(int depth, NavigationVisitor visitor, Navigation n) {
		visitor.visit(n);
		
		if(CollectionUtils.isEmpty(n.getChildren())) return;
		int newDepth = depth +1;
		for(Navigation c : n.getChildren()){
			travel(newDepth, visitor, c);
		}
	}
	
	
	
	@Override
	public String toString() {
		return "Navigation ["
				+ "  selected=" + selected 
				+ ", pattern=" + pattern 
				+ ", hide=" + hide 
				+ ", method=" + method 
				+ ", path=" + path 
				+ ", access=" + access
				+ ", name=" + name 
				+ "]";
	}
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((pattern == null) ? 0 : pattern.hashCode());
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
		Navigation other = (Navigation) obj;
		if (pattern == null) {
			if (other.pattern != null)
				return false;
		} else if (!pattern.equals(other.pattern))
			return false;
		if (path == null) {
			if (other.path != null)
				return false;
		} else if (!path.equals(other.path))
			return false;
		return true;
	}
	
	

}


//public boolean isAuthorizable();

