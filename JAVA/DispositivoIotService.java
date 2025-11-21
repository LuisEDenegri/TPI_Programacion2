package service;

import config.DatabaseConnection;
import dao.DispositivoIotDao;
import dao.ConfiguracionRedDao;
import entities.DispositivoIoT;
import entities.ConfiguracionRed;
import exceptions.ServiceException;
import exceptions.ValidationException;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.List;

/**
 *  DispositivoIoT (Entidad A)
 * 
 * Responsabilidades:
 * 1. Orquestar transacciones entre DispositivoIoT y ConfiguracionRed
 * 2. Garantizar atomicidad: o se crean/actualizan ambos, o ninguno
 * 3. Validar reglas de negocio (campos obligatorios, formatos, unicidad)
 * 4. Mantener integridad de relación 1→1 unidireccional
 * 
 * @author Luis - Especialista en Logica de Negocio
 */
public class DispositivoIotService implements GenericService<DispositivoIoT> {
    
    private final DispositivoIotDao dispositivoDao;
    private final ConfiguracionRedDao configDao;
    
    public DispositivoIotService(DispositivoIotDao dispositivoDao, ConfiguracionRedDao configDao) {
        this.dispositivoDao = dispositivoDao;
        this.configDao = configDao;
    }
    
    /**
     * METODO PRINCIPAL: Insertar DispositivoIoT con su ConfiguracionRed
     * 
     * Orquesta la creación de A y B en una SOLA TRANSACCIÓN:
     * 1. Valida ambas entidades
     * 2. Crea ConfiguracionRed (B) primero
     * 3. Asocia B a A
     * 4. Crea DispositivoIoT (A)
     * 5. Commit atómico
     * 
     * Si CUALQUIER paso falla → ROLLBACK completo
     * 
     * @param dispositivo DispositivoIoT a crear (debe contener ConfiguracionRed)
     * @return DispositivoIoT creado con ID asignado
     * @throws ServiceException si hay error en validación o transacción
     */
    @Override
    public DispositivoIoT insertar(DispositivoIoT dispositivo) throws ServiceException {
        Connection conn = null;
        try {
            validarDispositivoIoT(dispositivo);
            
            // Validar que tiene ConfiguracionRed 
            if (dispositivo.getConfiguracionRed() == null) {
                throw new ValidationException("DispositivoIoT debe tener una ConfiguracionRed asociada");
            }
            
            // Validar unicidad de serial en BD
            if (existeSerialEnBD(dispositivo.getSerial())) {
                throw new ValidationException("Ya existe un DispositivoIoT con el serial: " + dispositivo.getSerial());
            }
            
            // Validar la ConfiguracionRed asociada
            validarConfiguracionRedParaDispositivo(dispositivo.getConfiguracionRed());
            
            conn = DatabaseConnection.getConnection();
            conn.setAutoCommit(false);  
            
            ConfiguracionRed configCreada = configDao.crear(dispositivo.getConfiguracionRed(), conn);
            
            dispositivo.setConfiguracionRed(configCreada);  //ID asignado
            
            DispositivoIoT dispositivoCreado = dispositivoDao.crear(dispositivo, conn);
            
            conn.commit();  
            
            System.out.println("DispositivoIoT y ConfiguracionRed creados exitosamente en transaccion");
            return dispositivoCreado;
            
        } catch (ValidationException e) {
            // Rollback ante error de validacion
            rollback(conn);
            throw e;
        } catch (SQLException e) {
            // Rollback ante cualquier error de BD
            rollback(conn);
            throw new ServiceException("Error transaccional al crear DispositivoIoT y ConfiguracionRed", e);
        } finally {
            cerrarConexion(conn);
        }
    }
    
    /**
     * Actualiza DispositivoIoT y su ConfiguracionRed en una transacción
     * 
     * @param dispositivo DispositivoIoT con datos actualizados
     * @return DispositivoIoT actualizado
     * @throws ServiceException si hay error
     */
    @Override
    public DispositivoIoT actualizar(DispositivoIoT dispositivo) throws ServiceException {
        Connection conn = null;
        try {
            // Validaciones previas
            if (dispositivo.getId() == null) {
                throw new ValidationException("El ID del DispositivoIoT es obligatorio para actualizar");
            }
            
            validarDispositivoIoT(dispositivo);
            
            // Verificar que existe
            DispositivoIoT existente = dispositivoDao.leer(dispositivo.getId());
            if (existente == null || Boolean.TRUE.equals(existente.getEliminado())) {
                throw new ValidationException("DispositivoIoT con ID " + dispositivo.getId() + " no existe");
            }
            
            // Validar unicidad de serial
            if (!dispositivo.getSerial().equals(existente.getSerial())) {
                if (existeSerialEnBD(dispositivo.getSerial())) {
                    throw new ValidationException("Ya existe otro DispositivoIoT con el serial: " + dispositivo.getSerial());
                }
            }
            
            // Validar relacion 1→1
            if (dispositivo.getConfiguracionRed() == null) {
                throw new ValidationException("DispositivoIoT debe tener una ConfiguracionRed asociada");
            }
            
            if (dispositivo.getConfiguracionRed().getId() == null) {
                throw new ValidationException("ConfiguracionRed debe tener un ID valido para actualizar");
            }
            
            validarConfiguracionRedParaDispositivo(dispositivo.getConfiguracionRed());
            
            // Inicio de transaccion
            conn = DatabaseConnection.getConnection();
            conn.setAutoCommit(false);
            
            // Actualizar ConfiguracionRed primero
            ConfiguracionRed configActualizada = configDao.actualizar(dispositivo.getConfiguracionRed(), conn);
            dispositivo.setConfiguracionRed(configActualizada);
            
            // Actualizar DispositivoIoT
            DispositivoIoT dispositivoActualizado = dispositivoDao.actualizar(dispositivo, conn);
            
            // Commit
            conn.commit();
            
            System.out.println("DispositivoIoT y ConfiguracionRed actualizados exitosamente");
            return dispositivoActualizado;
            
        } catch (ValidationException e) {
            rollback(conn);
            throw e;
        } catch (SQLException e) {
            rollback(conn);
            throw new ServiceException("Error transaccional al actualizar DispositivoIoT", e);
        } finally {
            cerrarConexion(conn);
        }
    }
    
    /**
     * Elimina (baja lógica) DispositivoIoT y su ConfiguracionRed en cascada
     * 
     * @param id ID del DispositivoIoT a eliminar
     * @throws ServiceException si hay error
     */
    @Override
    public void eliminar(Long id) throws ServiceException {
        Connection conn = null;
        try {
            // 1. Validaciones
            if (id == null) {
                throw new ValidationException("El ID es obligatorio para eliminar");
            }
            
            // Verificar que existe
            DispositivoIoT existente = dispositivoDao.leer(id);
            if (existente == null || Boolean.TRUE.equals(existente.getEliminado())) {
                throw new ValidationException("DispositivoIoT con ID " + id + " no existe");
            }
            
            // Inicio de Transacción
            conn = DatabaseConnection.getConnection();
            conn.setAutoCommit(false);
            
            // Eliminar DispositivoIoT
            dispositivoDao.eliminar(id, conn);
            
            // Eliminar ConfiguracionRed asociada
            if (existente.getConfiguracionRed() != null && existente.getConfiguracionRed().getId() != null) {
                configDao.eliminar(existente.getConfiguracionRed().getId(), conn);
            }
            
            // Commit
            conn.commit();
            
            System.out.println("DispositivoIoT y ConfiguracionRed eliminados exitosamente");
            
        } catch (ValidationException e) {
            rollback(conn);
            throw e;
        } catch (SQLException e) {
            rollback(conn);
            throw new ServiceException("Error transaccional al eliminar DispositivoIoT", e);
        } finally {
            cerrarConexion(conn);
        }
    }
    
    @Override
    public DispositivoIoT getById(Long id) throws ServiceException {
        try {
            if (id == null) {
                throw new ValidationException("El ID es obligatorio");
            }
            
            DispositivoIoT dispositivo = dispositivoDao.leer(id);
            if (dispositivo == null || Boolean.TRUE.equals(dispositivo.getEliminado())) {
                throw new ValidationException("DispositivoIoT con ID " + id + " no encontrado");
            }
            return dispositivo;
        } catch (SQLException e) {
            throw new ServiceException("Error al obtener DispositivoIoT por ID", e);
        }
    }
    
    @Override
    public List<DispositivoIoT> getAll() throws ServiceException {
        try {
            return dispositivoDao.leerTodos();
        } catch (SQLException e) {
            throw new ServiceException("Error al obtener todos los DispositivosIoT", e);
        }
    }
    
    /**
     * BUSQUEDA POR CAMPO RELEVANTE: Serial 
     * Requerido por el TP: "al menos una búsqueda por un campo relevante"
     * 
     * @param serial Serial del dispositivo
     * @return DispositivoIoT encontrado
     * @throws ServiceException si no se encuentra
     */
    public DispositivoIoT buscarPorSerial(String serial) throws ServiceException {
        try {
            if (serial == null || serial.trim().isEmpty()) {
                throw new ValidationException("El serial es obligatorio para la busqueda");
            }
            
            // Convertir a mayusculas 
            serial = serial.toUpperCase().trim();
            
            DispositivoIoT dispositivo = dispositivoDao.buscarPorSerial(serial);
            if (dispositivo == null || Boolean.TRUE.equals(dispositivo.getEliminado())) {
                throw new ValidationException("No se encontro DispositivoIoT con serial: " + serial);
            }
            return dispositivo;
        } catch (SQLException e) {
            throw new ServiceException("Error al buscar DispositivoIoT por serial", e);
        }
    }
    
    
    public List<DispositivoIoT> buscarPorModelo(String modelo) throws ServiceException {
        try {
            if (modelo == null || modelo.trim().isEmpty()) {
                throw new ValidationException("El modelo es obligatorio para la busqueda");
            }
            return dispositivoDao.buscarPorModelo(modelo);
        } catch (SQLException e) {
            throw new ServiceException("Error al buscar por modelo", e);
        }
    }
    
    
    public List<DispositivoIoT> buscarPorUbicacion(String ubicacion) throws ServiceException {
        try {
            if (ubicacion == null || ubicacion.trim().isEmpty()) {
                throw new ValidationException("La ubicacion es obligatoria para la busqueda");
            }
            return dispositivoDao.buscarPorUbicacion(ubicacion);
        } catch (SQLException e) {
            throw new ServiceException("Error al buscar por ubicacion", e);
        }
    }
    
    

    private void validarDispositivoIoT(DispositivoIoT dispositivo) throws ValidationException {
        if (dispositivo == null) {
            throw new ValidationException("DispositivoIoT no puede ser nulo");
        }
        
        if (dispositivo.getSerial() == null || dispositivo.getSerial().trim().isEmpty()) {
            throw new ValidationException("El serial es obligatorio");
        }
        if (dispositivo.getSerial().length() > 50) {
            throw new ValidationException("El serial excede la longitud maxima de 50 caracteres");
        }
        // Validar formato de serial 
        if (!validarFormatoSerial(dispositivo.getSerial())) {
            throw new ValidationException("Formato de serial invalido. Use formato: SYYYY-NNN");
        }
        
    
        if (dispositivo.getModelo() == null || dispositivo.getModelo().trim().isEmpty()) {
            throw new ValidationException("El modelo es obligatorio");
        }
        if (dispositivo.getModelo().length() > 50) {
            throw new ValidationException("El modelo excede la longitud máxima de 50 caracteres");
        }
        
        if (dispositivo.getUbicacion() != null && dispositivo.getUbicacion().length() > 120) {
            throw new ValidationException("La ubicacion excede la longitud máxima de 120 caracteres");
        }
        
        if (dispositivo.getFirmwareVersion() != null) {
            if (dispositivo.getFirmwareVersion().length() > 30) {
                throw new ValidationException("La version de firmware excede la longitud maxima de 30 caracteres");
            }
            // Validar formato si esta presente 
            if (!dispositivo.getFirmwareVersion().isEmpty() && 
                !validarFormatoVersionFirmware(dispositivo.getFirmwareVersion())) {
                throw new ValidationException("Formato de version de firmware invalido. Use formato: vX.Y.Z");
            }
        }
    }
    
    
    private void validarConfiguracionRedParaDispositivo(ConfiguracionRed config) throws ValidationException {
        if (config == null) {
            throw new ValidationException("ConfiguracionRed no puede ser nula");
        }
        
        // dhcpHabilitado es obligatorio
        if (config.getDhcpHabilitado() == null) {
            throw new ValidationException("El campo dhcpHabilitado es obligatorio en ConfiguracionRed");
        }
        
        // Si DHCP esta deshabilitado, validar que tenga configuracion manual
        if (Boolean.FALSE.equals(config.getDhcpHabilitado())) {
            if (esCadenaVacia(config.getIp())) {
                throw new ValidationException("IP es obligatoria cuando DHCP esta deshabilitado");
            }
            if (esCadenaVacia(config.getMascara())) {
                throw new ValidationException("Máscara es obligatoria cuando DHCP esta deshabilitado");
            }
            if (esCadenaVacia(config.getGateway())) {
                throw new ValidationException("Gateway es obligatorio cuando DHCP esta deshabilitado");
            }
        }
        
        // Validar IPs si estan presentes
        if (!esCadenaVacia(config.getIp()) && !validarFormatoIP(config.getIp())) {
            throw new ValidationException("Formato de IP invalido: " + config.getIp());
        }
        
        if (!esCadenaVacia(config.getMascara()) && !validarFormatoIP(config.getMascara())) {
            throw new ValidationException("Formato de mascara invalido: " + config.getMascara());
        }
        
        if (!esCadenaVacia(config.getGateway()) && !validarFormatoIP(config.getGateway())) {
            throw new ValidationException("Formato de gateway invalido: " + config.getGateway());
        }
        
        if (!esCadenaVacia(config.getDnsPrimario()) && !validarFormatoIP(config.getDnsPrimario())) {
            throw new ValidationException("Formato de DNS primario invalido: " + config.getDnsPrimario());
        }
    }
    
    /**
     * Verificar unicidad de serial
     * Verifica si existe un DispositivoIoT con el serial dado
     * Implementa: "impedir más de un B por A" 
     */
    private boolean existeSerialEnBD(String serial) throws SQLException {
        DispositivoIoT existente = dispositivoDao.buscarPorSerial(serial);
        return existente != null && !Boolean.TRUE.equals(existente.getEliminado());
    }
    
    
    
     //Valida formato de serial: SYYYY-NNN
     
    private boolean validarFormatoSerial(String serial) {
        if (serial == null || serial.isEmpty()) {
            return false;
        }
       
        String serialPattern = "^S\\d{4}-\\d{3}$";
        return serial.matches(serialPattern);
    }
    
     //Valida formato de dirección IP (IPv4 e IPv6)
     
    private boolean validarFormatoIP(String ip) {
        if (ip == null || ip.trim().isEmpty()) {
            return false;
        }
        
        // Validar IPv4
        String ipv4Pattern = "^((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$";
        if (ip.matches(ipv4Pattern)) {
            return true;
        }
        
        // Validar IPv6 
        String ipv6Pattern = "^([0-9a-fA-F]{1,4}:){7}[0-9a-fA-F]{1,4}$";
        return ip.matches(ipv6Pattern);
    }
    
  
    private boolean validarFormatoVersionFirmware(String version) {
        if (version == null || version.isEmpty()) {
            return false;
        }
        String versionPattern = "^v?\\d+\\.\\d+\\.\\d+$";
        return version.matches(versionPattern);
    }
    
     // Verifica si una cadena es nula o vacia
    
    private boolean esCadenaVacia(String cadena) {
        return cadena == null || cadena.trim().isEmpty();
    }
    
    
    
     // Realiza rollback de forma segura
     
    private void rollback(Connection conn) {
        if (conn != null) {
            try {
                conn.rollback();
                System.err.println("ROLLBACK ejecutado - Transaccion revertida");
            } catch (SQLException rollbackEx) {
                System.err.println("Error en rollback: " + rollbackEx.getMessage());
            }
        }
    }
    
    
     // Cierra conexión de forma segura y restaura autoCommit
     
    private void cerrarConexion(Connection conn) {
        if (conn != null) {
            try {
                conn.setAutoCommit(true);  // Restaurar comportamiento por defecto
                conn.close();
            } catch (SQLException closeEx) {
                System.err.println("Error cerrando conexion: " + closeEx.getMessage());
            }
        }
    }
}
