package com.u2ware.springfield.repository.jdbc;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.List;

import org.springframework.dao.DataAccessException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.jdbc.core.ConnectionCallback;
import org.springframework.jdbc.core.JdbcTemplate;

import com.mysema.query.sql.SQLQuery;
import com.mysema.query.sql.SQLTemplates;
import com.mysema.query.sql.dml.SQLDeleteClause;
import com.mysema.query.types.Predicate;
import com.mysema.query.types.path.PathBuilder;
import com.u2ware.springfield.domain.Pagination;
import com.u2ware.springfield.repository.AbstractQueryDslExecutor;

public class JdbcQueryDslExecutor<T> extends AbstractQueryDslExecutor<T> {

	private final PathBuilder<T> metamodel;
	private final JdbcTemplate jdbcTemplate;
	private SQLTemplates dialect;
	
	public JdbcQueryDslExecutor(Class<T> entityClass, JdbcTemplate jdbcTemplate) {
		this.metamodel = AbstractQueryDslExecutor.createPathBuilder(entityClass);
		this.jdbcTemplate = jdbcTemplate;
	}

	public void setDialect(SQLTemplates dialect) {
		this.dialect = dialect;
	}

	@Override
	public PathBuilder<T> getPathBuilder() {
		return metamodel;
	}
	
	
	@Override
	public List<T> findAll(final Predicate predicate, final Sort... sorts) {
		return jdbcTemplate.execute(new ConnectionCallback<List<T>>(){
			public List<T> doInConnection(Connection con) throws SQLException, DataAccessException {
				SQLQuery query = new SQLQuery(con, dialect).from(metamodel);
				applyWhere(query, predicate);
				applySorting(query, sorts);
				return query.list(metamodel);
			}
		});
	}
	
	
	@Override
	public Page<T> findAll(final Predicate predicate, final Pageable pageable, final Sort... sorts) {
		return jdbcTemplate.execute(new ConnectionCallback<Page<T>>(){
			public Page<T> doInConnection(Connection con) throws SQLException, DataAccessException {
				SQLQuery query = new SQLQuery(con, dialect).from(metamodel);
				applyWhere(query, predicate);
				applySorting(query, sorts);
				applyPagination(query, pageable);
				List<T> content = query.list(metamodel);
				
				
				SQLQuery countQuery = new SQLQuery(con, dialect).from(metamodel);
				applyWhere(countQuery, predicate);
				long total = countQuery.count();

				return new Pagination<T>(content, pageable, total);
			}
		});
	}
	@Override
	public long count(final Predicate predicate) {
		return jdbcTemplate.execute(new ConnectionCallback<Long>(){
			public Long doInConnection(Connection con) throws SQLException, DataAccessException {
				SQLQuery query = new SQLQuery(con, dialect).from(metamodel);
				applyWhere(query, predicate);
				return query.count();
			}
		});
	}
	@Override
	public long deleteAll(final Predicate predicate) {
		return jdbcTemplate.execute(new ConnectionCallback<Long>(){
			public Long doInConnection(Connection con) throws SQLException, DataAccessException {
				SQLDeleteClause query = new SQLDeleteClause(con, dialect, null);
				applyWhere(query, predicate);
				return query.execute();
			}
		});
	}
}