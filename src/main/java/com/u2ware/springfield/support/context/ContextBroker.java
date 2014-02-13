package com.u2ware.springfield.support.context;

public interface ContextBroker  {

	public <O> void put(O object);
	public <O> O get(Class<O> type);
	public <O> O get(Class<O> type, boolean throwException);
	public <O> O remove(Class<O> type);
}
