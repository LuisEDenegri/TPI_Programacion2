package service;

import config.DatabaseConnection;
import dao.ConfiguracionRedDao;
import entities.ConfiguracionRed;
import exceptions.ServiceException;
import exceptions.ValidationException;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.List;

/**
 * Maneja validaciones, transacciones y lógica de negocio
 */
public class ConfiguracionRedService implements GenericService<ConfiguracionRed> {
    
    private final ConfiguracionRedDao configuracionRedDao;
    
    /**
     * Constructor que recibe la implementación del DAO
     */
    public ConfiguracionRedService(ConfiguracionRedDao configuracionRedDao) {
        this.configuracionRedDao = configuracionRedDao;
    }
    
    @Override
    public ConfiguracionRed insertar(ConfiguracionRed configuracion) throws ServiceException {
        Connection conn = null;
        try {
            // Validaciones de Negocio
            validarConfiguracionRed(configuracion);
            
            // Inicio de Transacción
            conn = DatabaseConnection.getConnection();
            conn.setAutoCommit(false);
            
            // Crear ConfiguracionRed
            ConfiguracionRed configuracionCreada = configuracionRedDao.crear(configuracion, conn);
            
            // Commit si todo salió bien
            conn.commit();
            return configuracionCreada;
            
        } catch (ValidationException e) {
            // Rollback ante error de validación
            rollback(conn);
            throw e;
        } catch (SQLException e) {
            // Rollback ante cualquier error de BD
            rollback(conn);
            throw new ServiceException("Error transaccional al crear ConfiguracionRed", e);
        } finally {
            // Restablecer autoCommit y cerrar recursos
            cerrarConexion(conn);
        }
    }
    
    @Override
    public ConfiguracionRed actualizar(ConfiguracionRed configuracion) throws ServiceException {
        Connection conn = null;
        try {
            // Validaciones
            if (configuracion.getId() == null) {
                throw new ValidationException("El ID de ConfiguracionRed es obligatorio para actualizar");
            }
            validarConfiguracionRed(configuracion);
            
            // Verificar que existe
            ConfiguracionRed existente = configuracionRedDao.leer(configuracion.getId());
            if (existente == null || Boolean.TRUE.equals(existente.getEliminado())) {
                throw new ValidationException("ConfiguracionRed con ID " + configuracion.getId() + " no existe");
            }
            
            // Inicio de Transacción
            conn = DatabaseConnection.getConnection();
            conn.setAutoCommit(false);
            
            // Actualizar
            ConfiguracionRed configuracionActualizada = configuracionRedDao.actualizar(configuracion, conn);
            
            // Commit
            conn.commit();
            return configuracionActualizada;
            
        } catch (ValidationException e) {
            rollback(conn);
            throw e;
        } catch (SQLException e) {
            rollback(conn);
            throw new ServiceException("Error transaccional al actualizar ConfiguracionRed", e);
        } finally {
            cerrarConexion(conn);
        }
    }
    
    @Override
    public void eliminar(Long id) throws ServiceException {
        Connection conn = null;
        try {
            // Validaciones
            if (id == null) {
                throw new ValidationException("El ID es obligatorio para eliminar");
            }
            
            // Verificar que existe
            ConfiguracionRed existente = configuracionRedDao.leer(id);
            if (existente == null || Boolean.TRUE.equals(existente.getEliminado())) {
                throw new ValidationException("ConfiguracionRed con ID " + id + " no existe");
            }
            
            // Inicio de Transacción
            conn = DatabaseConnection.getConnection();
            conn.setAutoCommit(false);
            
            // Eliminar
            configuracionRedDao.eliminar(id, conn);
            
            // Commit
            conn.commit();
            
        } catch (ValidationException e) {
            rollback(conn);
            throw e;
        } catch (SQLException e) {
            rollback(conn);
            throw new ServiceException("Error transaccional al eliminar ConfiguracionRed", e);
        } finally {
            cerrarConexion(conn);
        }
    }
    
    @Override
    public ConfiguracionRed getById(Long id) throws ServiceException {
        try {
            if (id == null) {
                throw new ValidationException("El ID es obligatorio");
            }
            
            ConfiguracionRed configuracion = configuracionRedDao.leer(id);
            if (configuracion == null || Boolean.TRUE.equals(configuracion.getEliminado())) {
                throw new ValidationException("ConfiguracionRed con ID " + id + " no encontrada");
            }
            return configuracion;
        } catch (SQLException e) {
            throw new ServiceException("Error al obtener ConfiguracionRed por ID", e);
        }
    }
    
    @Override
    public List<ConfiguracionRed> getAll() throws ServiceException {
        try {
            return configuracionRedDao.leerTodos();
        } catch (SQLException e) {
            throw new ServiceException("Error al obtener todas las ConfiguracionesRed", e);
        }
    }
        
    
    // Valida una ConfiguracionRed según reglas de negocio
     
    private void validarConfiguracionRed(ConfiguracionRed configuracion) throws ValidationException {
        if (configuracion == null) {
            throw new ValidationException("ConfiguracionRed no puede ser nula");
        }
        
        // Validar dhcpHabilitado
        if (configuracion.getDhcpHabilitado() == null) {
            throw new ValidationException("El campo dhcpHabilitado es obligatorio");
        }
        
        // Si DHCP está deshabilitado, validar que tenga IPs configuradas
        if (Boolean.FALSE.equals(configuracion.getDhcpHabilitado())) {
            if (esCadenaVacia(configuracion.getIp())) {
                throw new ValidationException("IP es obligatoria cuando DHCP esta deshabilitado");
            }
            if (esCadenaVacia(configuracion.getMascara())) {
                throw new ValidationException("Mascara es obligatoria cuando DHCP está deshabilitado");
            }
            if (esCadenaVacia(configuracion.getGateway())) {
                throw new ValidationException("Gateway es obligatorio cuando DHCP está deshabilitado");
            }
        }
        
        // Validar formato de IP si esta presente
        if (!esCadenaVacia(configuracion.getIp())) {
            if (!validarFormatoIP(configuracion.getIp())) {
                throw new ValidationException("Formato de IP invalido: " + configuracion.getIp());
            }
            if (configuracion.getIp().length() > 45) {
                throw new ValidationException("IP excede longitud maxima de 45 caracteres");
            }
        }
        
        // Validar formato de Máscara si esta presente
        if (!esCadenaVacia(configuracion.getMascara())) {
            if (!validarFormatoIP(configuracion.getMascara())) {
                throw new ValidationException("Formato de máscara invalido: " + configuracion.getMascara());
            }
            if (configuracion.getMascara().length() > 45) {
                throw new ValidationException("Mascara excede longitud máxima de 45 caracteres");
            }
        }
        
        // Validar formato de Gateway si esta presente
        if (!esCadenaVacia(configuracion.getGateway())) {
            if (!validarFormatoIP(configuracion.getGateway())) {
                throw new ValidationException("Formato de gateway invalido: " + configuracion.getGateway());
            }
            if (configuracion.getGateway().length() > 45) {
                throw new ValidationException("Gateway excede longitud máxima de 45 caracteres");
            }
        }
        
        // Validar formato de DNS Primario si está presente
        if (!esCadenaVacia(configuracion.getDnsPrimario())) {
            if (!validarFormatoIP(configuracion.getDnsPrimario())) {
                throw new ValidationException("Formato de DNS primario invalido: " + configuracion.getDnsPrimario());
            }
            if (configuracion.getDnsPrimario().length() > 45) {
                throw new ValidationException("DNS primario excede longitud maxima de 45 caracteres");
            }
        }
    }
    
    
     // Valida formato de dirección IP (IPv4 e IPv6)
     
    private boolean validarFormatoIP(String ip) {
        if (ip == null || ip.trim().isEmpty()) {
            return false;
        }
        
        // IPv4
        String ipv4Pattern = "^((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$";
        if (ip.matches(ipv4Pattern)) {
            return true;
        }
        
        // IPv6 
        String ipv6Pattern = "^([0-9a-fA-F]{1,4}:){7}[0-9a-fA-F]{1,4}$";
        return ip.matches(ipv6Pattern);
    }
    
    
     //Verifica si una cadena es nula o vacía
     
    private boolean esCadenaVacia(String cadena) {
        return cadena == null || cadena.trim().isEmpty();
    }
    
    // Metodos auciliares de transaccion
    
    private void rollback(Connection conn) {
        if (conn != null) {
            try {
                conn.rollback();
            } catch (SQLException rollbackEx) {
                System.err.println("Error en rollback: " + rollbackEx.getMessage());
            }
        }
    }
    
    private void cerrarConexion(Connection conn) {
        if (conn != null) {
            try {
                conn.setAutoCommit(true);
                conn.close();
            } catch (SQLException closeEx) {
                System.err.println("Error cerrando conexion: " + closeEx.getMessage());
            }
        }
    }
}
