package service;

import exceptions.ServiceException;
import java.util.List;

/**
 * Interfaz genérica de servicio que define operaciones de negocio
 * Capa intermedia entre el menú (UI) y los DAOs
 * 
 * @param <T> Tipo de la entidad
 */
public interface GenericService<T> {
    
    /**
     * Inserta una nueva entidad con validaciones y transacción
     * 
     * @param entity Entidad a insertar
     * @return Entidad insertada con ID asignado
     * @throws ServiceException si hay error en validación o transacción
     */
    T insertar(T entity) throws ServiceException;
    
    /**
     * Actualiza una entidad existente con validaciones y transacción
     * 
     * @param entity Entidad con datos actualizados
     * @return Entidad actualizada
     * @throws ServiceException si hay error en validación o transacción
     */
    T actualizar(T entity) throws ServiceException;
    
    /**
     * Elimina (baja lógica) una entidad por ID con transacción
     * 
     * @param id ID de la entidad a eliminar
     * @throws ServiceException si hay error en la transacción
     */
    void eliminar(Long id) throws ServiceException;
    
    /**
     * Obtiene una entidad por su ID
     * 
     * @param id ID de la entidad
     * @return Entidad encontrada
     * @throws ServiceException si no existe o está eliminada
     */
    T getById(Long id) throws ServiceException;
    
    /**
     * Obtiene todas las entidades activas (no eliminadas)
     * 
     * @return Lista de entidades
     * @throws ServiceException si hay error al consultar
     */
    List<T> getAll() throws ServiceException;
}
