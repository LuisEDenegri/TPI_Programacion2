package exceptions;

/**
 * Excepcion general para errores en la capa de servicio
 * Encapsula errores de la capa DAO, validaciones y logica de negocio
 */
public class ServiceException extends Exception {
    
    /**
     * Constructor con mensaje
     * 
     * @param message Descripcion del error
     */
    public ServiceException(String message) {
        super(message);
    }
    
    /**
     * Constructor con mensaje y causa raíz
     * util para encapsular excepciones de capas inferiores 
     * 
     * @param message Descripcion del error
     * @param cause Excepcion original que causo el error
     */
    public ServiceException(String message, Throwable cause) {
        super(message, cause);
    }
    
    /**
     * Constructor solo con causa
     * 
     * @param cause Excepción original
     */
    public ServiceException(Throwable cause) {
        super(cause);
    }
}
