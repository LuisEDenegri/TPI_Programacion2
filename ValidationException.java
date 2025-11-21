package exceptions;

/**
 * Excepción específica para errores de validación de datos
 * Se lanza cuando los datos no cumplen con las reglas de negocio
 * 
 * Ejemplos:
 * - Campo obligatorio vacío
 * - Formato inválido (email, IP, etc.)
 * - Violación de unicidad
 * - Violación de regla 1→1
 */
public class ValidationException extends ServiceException {
    
    /**
     * Constructor con mensaje de validación
     * 
     * @param message Descripción del error de validación
     */
    public ValidationException(String message) {
        super(message);
    }
    
    /**
     * Constructor con mensaje y causa
     * 
     * @param message Descripción del error
     * @param cause Causa raíz del error
     */
    public ValidationException(String message, Throwable cause) {
        super(message, cause);
    }
}
