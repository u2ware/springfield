package com.u2ware.springfield.sample.security.user.login;

import java.util.Date;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.joda.time.DateTime;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.security.web.authentication.rememberme.PersistentRememberMeToken;
import org.springframework.security.web.authentication.rememberme.PersistentTokenRepository;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.u2ware.springfield.repository.EntityRepository;
import com.u2ware.springfield.sample.security.PersistentLogins;

@Service
public class LoginUserRememberMeRepository implements PersistentTokenRepository{

	//InMemoryTokenRepositoryImpl m;
	//JdbcTokenRepositoryImpl j;
	
	protected final Log logger = LogFactory.getLog(getClass());

	@Autowired @Qualifier("persistentLoginsRepository")
	public EntityRepository<PersistentLogins, String> persistentLoginsRepository;
	
	
	private @AllArgsConstructor class FindBySeries{
		@Getter @Setter private String series;
	}
	private @AllArgsConstructor class FindByUsername{
		@Getter @Setter private String username;
	}
	
	
	@Override
	@Transactional
	public PersistentRememberMeToken getTokenForSeries(String seriesId) {
		
		logger.debug("getTokenForSeries : "+seriesId);
		
		boolean exists = persistentLoginsRepository.exists(seriesId);
		logger.debug("exists : "+exists);
		
		if(exists){
			PersistentLogins entity = persistentLoginsRepository.read(seriesId);
			logger.debug("read entity : "+entity);

			String username = entity.getUsername();
			String series = entity.getSeries();
			String tokenValue = entity.getToken();
			Date date = entity.getLastUsed().toDate();
			PersistentRememberMeToken token = new PersistentRememberMeToken(username, series, tokenValue, date);
			logger.debug("token : "+token);
			return token;
		}
		logger.debug("token : null");
		return null;
	}

	@Override
	@Transactional
	public void createNewToken(PersistentRememberMeToken token) {
		
		logger.debug("createNewToken : "+token.getSeries());

		PersistentLogins entity = new PersistentLogins();
		entity.setUsername(token.getUsername());
		entity.setSeries(token.getSeries());
		entity.setToken(token.getTokenValue());
		entity.setLastUsed(new DateTime(token.getDate()));
		
		PersistentLogins newEntity = persistentLoginsRepository.create(entity);
		
		logger.debug("created entity "+newEntity);
	}

	@Override
	@Transactional
	public void updateToken(String series, String tokenValue, Date lastUsed) {

		logger.debug("updateToken : "+series);

		boolean exists = persistentLoginsRepository.exists(series);
		logger.debug("exists : "+exists);
		
		if(exists){
			
			PersistentLogins entity = persistentLoginsRepository.read(series);
			entity.setToken(tokenValue);
			entity.setLastUsed(new DateTime());
			
			PersistentLogins newEntity = persistentLoginsRepository.update(entity);
			logger.debug("updated entity "+newEntity);
		}
	}


	@Override
	@Transactional
	public void removeUserTokens(String username) {
		logger.debug("removeUserTokens : "+username);

		List<PersistentLogins> entities = persistentLoginsRepository.findAll(new FindByUsername(username));
		for(PersistentLogins entity : entities){
			persistentLoginsRepository.delete(entity);
			logger.debug("removed entity "+entity);
		}
	
	}
}
