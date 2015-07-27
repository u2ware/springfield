package sample.application.desk;

import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.rest.core.annotation.RepositoryRestResource;

//@RepositoryRestResource
@RepositoryRestResource(collectionResourceRel = "desks", path = "desks")
public interface DeskRepository extends PagingAndSortingRepository<Desk, String>{

	
}
