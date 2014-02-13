package com.u2ware.springfield.security.authorization;

import org.springframework.security.access.ConfigAttribute;

public class AuthorityAttribute implements ConfigAttribute {

	private static final long serialVersionUID = 3676226070094389333L;
	
	private String authorizeExpressionString;

    public AuthorityAttribute(String authorizeExpressionString) {
        this.authorizeExpressionString = authorizeExpressionString;
    }

    public String getAttribute() {
        return authorizeExpressionString;
    }
    
	@Override
	public String toString() {
		return authorizeExpressionString;
	}

}